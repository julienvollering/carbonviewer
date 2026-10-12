## contribute.R ##
# "Contribute your peat depths" module (Phase 1: standardize + download).
#
# Turns the uploaded peat depth points into records that follow the
# Data Exchange Standard for Peat Surveys v1.0 (corrected profile) with the
# CarbonViewer Norwegian profile (see standard/), and lets the user download
# them as a zip. Nothing is stored on the server. The module reads the uploaded
# zip itself (read_dataset_zip() in global.R), so it does not need the volume
# calculation.

contribute_app_version <- "CarbonViewer 1.0.0"

# Wizard steps, in order
contribute_steps <- c("intro", "measure", "review", "metadata", "validate", "download")

# Columns of the review table (coordinates are on the map and in the final preview)
contribute_review_columns <- c("point", "recordDate", "surfacePeatDepth", "probeReachedBottom",
                               "peatDepthRemarks", "locationRemarks")

# Columns the user can edit per row in the review table
contribute_editable <- c("recordDate", "probeReachedBottom", "peatDepthRemarks", "locationRemarks")

# Hidden sort key of the review table: 1 for rows moved to the top
contribute_top_column <- "top"

# Map colours; "selected" also colours the selected rows of the review table
contribute_colors <- c(yes = "#555555", no = "#f0ad4e", selected = "#1f78b4")

#####################
# TRANSLATION       #
#####################

# Server-side lookup in translation.json (keys are Norwegian). The wizard is
# rendered on the server, so it cannot rely on shiny.i18n's client-side switch.
contribute_tr <- local({
  table <- NULL
  function(key, lang = "no") {
    if (!identical(lang, "en")) return(key)
    if (is.null(table)) {
      table <<- jsonlite::fromJSON(file.path(app_dir, "translation.json"))$translation
    }
    hit <- table$en[match(key, table$no)]
    if (is.na(hit)) key else hit
  }
})

##########################
# STANDARDIZE THE POINTS #
##########################

fmt_num <- function(x, digits) {
  out <- formatC(round(as.numeric(x), digits), format = "f", digits = digits,
                 drop0trailing = TRUE)
  out[is.na(x)] <- NA_character_
  out
}

# Boolean value as the TDWG Boolean Controlled Vocabulary writes it ("true" or
# "false"), from free text ("Yes", "ja", "TRUE", "1", ...); NA when not recognised
normalize_boolean <- function(x) {
  x <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(x))
  out[x %in% c("true", "t", "yes", "y", "ja", "j", "1")] <- "true"
  out[x %in% c("false", "f", "no", "n", "nei", "0")] <- "false"
  out
}

# peatDepthResolution written for a resolution of n cm (as in "Nearest 1cm")
resolution_value <- function(n) paste0("Nearest ", fmt_num(n, 2), "cm")

# Label of a peatDepthResolution value, e.g. "Nearest 2.5cm" -> "Nærmeste 2,5 cm"
resolution_label <- function(value, std, lang = "no") {
  if (is.null(value) || length(value) == 0 || is_blank(value[1])) return(NULL)
  ch <- std_choices(std, "peatDepthResolution", lang)
  if (value %in% ch) return(names(ch)[match(value, ch)])
  n <- sub("^Nearest ([0-9.]+) ?cm$", "\\1", value)
  if (identical(n, value)) return(value)
  if (lang == "no") paste0("Nærmeste ", sub(".", ",", n, fixed = TRUE), " cm") else paste0("Nearest ", n, " cm")
}

# Upload columns that are not standard attributes (kept as extra columns)
contribute_extra_columns <- function(points, std) {
  up <- names(sf::st_drop_geometry(points))
  known <- c(tolower(std_names(std, "peat_data", include_hidden = TRUE)),
             "x", "y", "torvdybde_cm", "peat_depth_cm")
  up[!tolower(up) %in% known]
}

# Does the upload have a complete column for a standard attribute?
upload_has <- function(points, name, complete = TRUE) {
  up <- sf::st_drop_geometry(points)
  hit <- match(tolower(name), tolower(names(up)))
  if (is.na(hit)) return(FALSE)
  !complete || !any(is_blank(up[[hit]]))
}

#' Turn the app's points into standard peat data records
#'
#' Coordinates, depth and the profile defaults are derived; dataset-wide
#' answers from the wizard fill the rest. Values for standard attributes found
#' in the uploaded CSV (matched by name, case-insensitive) win over the
#' answers. Other upload columns are kept after the standard columns.
#'
#' @param points sf points with `torvdybde_cm` (as from `read_dataset_zip()`).
#' @param answers named list: recordDate, surveyor, organizationName, location,
#'   probeType, peatDepthResolution, coordinateUncertainty, peatDepthRemarks and
#'   bottom: was the bottom reached at "all", "some" or "none" of the points.
#' @param std standard from `load_standard()`.
#' @return data frame of character columns in the standard's order.
standardize_points <- function(points, answers = list(), std) {
  n <- nrow(points)
  attrs <- std_names(std, "peat_data")
  out <- as.data.frame(stats::setNames(rep(list(rep(NA_character_, n)), length(attrs)), attrs),
                       stringsAsFactors = FALSE)
  ans <- function(name) {
    v <- answers[[name]]
    if (is.null(v) || length(v) == 0 || is_blank(v[1])) NA_character_ else as.character(v[1])
  }

  # Derived from the geometry and the depth
  en <- sf::st_coordinates(sf::st_transform(points, 25833))
  ll <- sf::st_coordinates(sf::st_transform(points, 4326))
  out$easting <- fmt_num(en[, "X"], 2)
  out$northing <- fmt_num(en[, "Y"], 2)
  out$latitude <- fmt_num(ll[, "Y"], 7)
  out$longitude <- fmt_num(ll[, "X"], 7)
  out$surfacePeatDepth <- fmt_num(points$torvdybde_cm, 2)

  # Profile defaults
  for (name in names(std$no_profile$defaults$peat_data)) {
    if (name %in% attrs) out[[name]] <- std_default(std, name)
  }

  # Dataset-wide answers
  out$recordDate <- ans("recordDate")
  out$surveyor <- ans("surveyor")
  out$institution <- ans("organizationName")
  out$location <- ans("location")
  out$probeType <- ans("probeType")
  if (!is.na(ans("peatDepthResolution"))) out$peatDepthResolution <- ans("peatDepthResolution")
  out$coordinateUncertainty <- fmt_num(suppressWarnings(as.numeric(ans("coordinateUncertainty"))), 2)
  out$peatDepthRemarks <- ans("peatDepthRemarks")
  out$probeReachedBottom <- switch(ans("bottom") %||% "",
                                   all = "true", some = "true", none = "false", NA_character_)

  # Values from the upload win, row by row
  derived <- c("latitude", "longitude", "easting", "northing", "geodeticDatum",
               "verbatimSRS", "surfacePeatDepth")
  up <- sf::st_drop_geometry(points)
  for (name in setdiff(attrs, derived)) {
    hit <- match(tolower(name), tolower(names(up)))
    if (is.na(hit)) next
    vals <- up[[hit]]
    vals <- if (inherits(vals, "Date")) format(vals, "%Y-%m-%d") else as.character(vals)
    if (identical(std_field(std, name)$type, "boolean")) {
      b <- normalize_boolean(vals)
      # An empty or unrecognised probeReachedBottom means unsure, which the
      # standard counts as false. Other unrecognised values are kept, so that
      # the check reports them.
      if (name == "probeReachedBottom") b[is.na(b)] <- "false"
      vals <- ifelse(is.na(b), vals, b)
    }
    keep <- !is_blank(vals)
    out[[name]][keep] <- vals[keep]
  }

  # Extra columns from the upload are allowed by the standard
  for (extra in contribute_extra_columns(points, std)) {
    out[[extra]] <- as.character(up[[extra]])
  }
  out
}

# Per-row edits from the review table (NA = no edit) laid over the records
apply_edits <- function(records, edits) {
  if (is.null(edits) || nrow(edits) != nrow(records)) return(records)
  for (name in names(edits)) {
    idx <- !is.na(edits[[name]])
    records[[name]][idx] <- edits[[name]][idx]
  }
  records
}

empty_edits <- function(n) {
  as.data.frame(stats::setNames(rep(list(rep(NA_character_, n)), length(contribute_editable)),
                                contribute_editable),
                stringsAsFactors = FALSE)
}

# Earliest and latest date in recordDate (ranges count with both ends)
record_date_range <- function(record_date) {
  parts <- unlist(strsplit(stats::na.omit(as.character(record_date)), "/", fixed = TRUE))
  d <- as.Date(substr(parts, 1, 10), format = "%Y-%m-%d")
  d <- d[!is.na(d)]
  if (length(d) == 0) return(c(NA_character_, NA_character_))
  format(c(min(d), max(d)), "%Y-%m-%d")
}

###################
# BUILD METADATA  #
###################

#' Build the one-row metadata table
#'
#' @param meta named list with the metadata answers (title, organizationName,
#'   abstract, purpose, contactElectronicMailAddress, license, rights,
#'   geographicDescription, methods, bibliographicCitation).
#' @param records peat data from `standardize_points()`.
#' @return one-row data frame of character columns in the standard's order.
build_metadata <- function(meta, records, std) {
  terms <- std_names(std, "metadata")
  out <- as.data.frame(stats::setNames(rep(list(NA_character_), length(terms)), terms),
                       stringsAsFactors = FALSE)
  for (name in intersect(names(meta), terms)) {
    v <- meta[[name]]
    if (!is.null(v) && length(v) > 0 && !is_blank(v[1])) out[[name]] <- trimws(as.character(v[1]))
  }

  lat <- suppressWarnings(as.numeric(records$latitude))
  lon <- suppressWarnings(as.numeric(records$longitude))
  if (any(!is.na(lat))) {
    out$westBoundingCoordinate <- fmt_num(min(lon, na.rm = TRUE), 6)
    out$eastBoundingCoordinate <- fmt_num(max(lon, na.rm = TRUE), 6)
    out$northBoundingCoordinate <- fmt_num(max(lat, na.rm = TRUE), 6)
    out$southBoundingCoordinate <- fmt_num(min(lat, na.rm = TRUE), 6)
  }
  dates <- record_date_range(records$recordDate)
  out$temporalCoverageStartDate <- dates[1]
  out$temporalCoverageEndDate <- dates[2]
  out
}

# Draft "methods" text from the wizard answers
draft_methods <- function(answers, records, std, lang = "no") {
  n <- nrow(records)
  dates <- record_date_range(records$recordDate)
  n_no <- sum(records$probeReachedBottom %in% "false")
  probe <- answers$probeType
  # "other" has no meaningful label for an outside reader; the equipment is
  # described in the remarks instead, so the method text leaves it out.
  probe_lab <- if (!is.null(probe) && !is_blank(probe) && !identical(probe, "other")) {
    lab <- names(std_choices(std, "probeType", lang))[match(probe, std_choices(std, "probeType", lang))]
    tolower(sub(" \\(.*\\)$", "", lab))
  }
  pos <- Filter(function(p) identical(p$id, answers$positioning), std$no_profile$positioning)
  pos_lab <- if (length(pos)) pos[[1]]$methods[[lang]]
  unc <- answers$coordinateUncertainty
  res_lab <- resolution_label(answers$peatDepthResolution, std, lang)
  v <- std_versions(std)

  if (lang == "no") {
    s <- paste0("Torvdybde ble målt i ", n, " punkter")
    if (!is.na(dates[1])) {
      s <- paste0(s, if (dates[1] == dates[2]) paste0(" den ", dates[1])
                  else paste0(" mellom ", dates[1], " og ", dates[2]))
    }
    if (!is.null(probe_lab)) s <- paste0(s, " med ", probe_lab)
    s <- paste0(s, ".")
    if (!is.null(res_lab)) s <- paste0(s, " Dybden ble lest av til ", tolower(res_lab), ".")
    s <- paste0(s, if (n_no == 0) " Bunnen av torva ble nådd i alle punkter."
                else if (n_no == n) " Det er ikke bekreftet at bunnen av torva ble nådd i noen punkter, så dybdene er minsteverdier."
                else paste0(" Bunnen av torva ble ikke nådd (eller er usikker) i ", n_no, if (n_no == 1) " punkt." else " punkter."))
    if (!is.null(pos_lab)) {
      s <- paste0(s, " Posisjonene ble målt med ", pos_lab,
                  if (!is.null(unc) && !is.na(unc)) paste0(" (anslått usikkerhet ", unc, " m)"), ".")
    }
    paste0(s, " Dataene ble standardisert med ", contribute_app_version,
           " etter Data Exchange Standard for Peat Surveys v", v$standard,
           " (profil ", v$profile, ", norsk profil ", v$no_profile, ").")
  } else {
    s <- paste0("Peat depth was measured at ", n, " points")
    if (!is.na(dates[1])) {
      s <- paste0(s, if (dates[1] == dates[2]) paste0(" on ", dates[1])
                  else paste0(" between ", dates[1], " and ", dates[2]))
    }
    if (!is.null(probe_lab)) s <- paste0(s, " using ", if (grepl("^[aeiou]", probe_lab)) "an " else "a ", probe_lab)
    s <- paste0(s, ".")
    if (!is.null(res_lab)) s <- paste0(s, " Depths were read to the ", tolower(res_lab), ".")
    s <- paste0(s, if (n_no == 0) " The bottom of the peat was reached at all points."
                else if (n_no == n) " The bottom of the peat was not confirmed at any point, so the depths are minimum values."
                else paste0(" The bottom of the peat was not reached (or is uncertain) at ", n_no, if (n_no == 1) " point." else " points."))
    if (!is.null(pos_lab)) {
      s <- paste0(s, " Positions were recorded with a ", pos_lab,
                  if (!is.null(unc) && !is.na(unc)) paste0(" (estimated uncertainty ", unc, " m)"), ".")
    }
    paste0(s, " Data were standardized with ", contribute_app_version,
           " to the Data Exchange Standard for Peat Surveys v", v$standard,
           " (profile ", v$profile, ", Norwegian profile ", v$no_profile, ").")
  }
}

# Suggested bibliographicCitation
draft_citation <- function(meta, records) {
  dates <- record_date_range(records$recordDate)
  year <- if (is.na(dates[2])) format(Sys.Date(), "%Y") else substr(dates[2], 1, 4)
  org <- if (is_blank(meta$organizationName %||% "")) "[organisation]" else meta$organizationName
  title <- if (is_blank(meta$title %||% "")) "[title]" else meta$title
  lic_name <- if (identical(meta$license, "Other") && !is_blank(meta$rights %||% NA)) meta$rights else meta$license
  lic <- if (is_blank(lic_name %||% "")) "" else paste0(" Licence: ", lic_name, ".")
  paste0(org, " (", year, "). ", title, ". Peat depth records, Data Exchange Standard for Peat Surveys v1.0.", lic)
}

####################
# WRITE THE FILES  #
####################

#' Write peat_data.csv, metadata.csv and README.txt
#'
#' CSV files follow RFC 4180 (comma-separated, CRLF line endings, fields
#' quoted when needed, UTF-8).
#'
#' @param dir folder to write into (created if needed).
#' @return the paths of the written files.
write_standard_package <- function(records, metadata, std, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(dir, c("peat_data.csv", "metadata.csv", "README.txt"))
  data.table::fwrite(records, paths[1], eol = "\r\n", na = "", quote = "auto")
  data.table::fwrite(metadata, paths[2], eol = "\r\n", na = "", quote = "auto")

  v <- std_versions(std)
  changes <- vapply(std$changes, `[[`, "", "id")
  extras <- setdiff(names(records), std_names(std, "peat_data"))
  readme <- c(
    paste("Peat depth records:", metadata$title %||% ""),
    "",
    paste0("Created with ", contribute_app_version, " on ", format(Sys.Date(), "%Y-%m-%d"), "."),
    "",
    "Standard",
    paste0("  ", std$standard$citation),
    paste0("  Corrected machine-readable profile ", v$profile, " (changes ", changes[1], "-",
           changes[length(changes)], ", see standard/peat_standard_changes.csv in the CarbonViewer repository)."),
    paste0("  ", std$no_profile$profile$name, " ", v$no_profile, "."),
    "  https://github.com/NINAnor/carbonviewer/tree/main/standard",
    "",
    "Files",
    "  peat_data.csv  One record per peat depth measurement. Columns follow the attribute order of the standard.",
    "  metadata.csv   One header row and one value row describing the dataset.",
    "  README.txt     This file.",
    "",
    "Notes",
    "  - CSV files follow RFC 4180 (comma-separated, CRLF line endings, UTF-8).",
    "  - surfacePeatDepth is in centimetres, measured from the surface.",
    "  - latitude/longitude are in EPSG:4326 (geodeticDatum). easting/northing are the",
    "    original coordinates in EPSG:25833 (ETRS89 / UTM zone 33N, see verbatimSRS).",
    "  - probeReachedBottom = false means the depth is a minimum value. Yes/no values are",
    "    written true/false, following the TDWG Boolean Controlled Vocabulary",
    "    (http://rs.tdwg.org/boolean/values/).",
    "  - gridReference (Ordnance Survey grid) is not used in Norway and is left out.",
    if (length(extras)) paste0("  - Extra columns kept from the uploaded file: ", paste(extras, collapse = ", "), "."),
    "",
    paste0("Licence: ", metadata$license %||% "",
           if (!is_blank(metadata$rights %||% NA)) paste0(" (", metadata$rights, ")") else ""),
    paste0("Cite as: ", metadata$bibliographicCitation %||% "")
  )
  writeLines(readme, paths[3], useBytes = TRUE)
  paths
}

#' Store a submission (Phase 2 placeholder)
#'
#' Will write the package and a manifest (UUID, timestamp, app and profile
#' version, licence, consent) to a staging store for curator review.
#' Not implemented in Phase 1: the app only offers the download.
store_submission <- function(package_paths, manifest) {
  warning("store_submission() is not implemented yet (Phase 2).")
  invisible(FALSE)
}

#################
# TABLE HELPERS #
#################

# Records with number columns as numbers, so that tables sort them correctly
as_display <- function(records, std) {
  for (name in names(records)) {
    if (identical(std_field(std, name)$type, "number")) {
      records[[name]] <- suppressWarnings(as.numeric(records[[name]]))
    }
  }
  records
}

# Table header whose cells show a description on hover. Long camelCase names
# may wrap between words, so the table fits beside the map.
dt_header <- function(columns, tips) {
  tags$table(class = "display compact contribute-dt",
             tags$thead(tags$tr(lapply(seq_along(columns), function(i) {
               label <- gsub("([a-z])([A-Z])", "\\1<wbr>\\2", htmltools::htmlEscape(columns[i]))
               tags$th(HTML(label), title = if (!is.na(tips[i]) && nzchar(tips[i])) tips[i])
             }))))
}

# DataTables' own texts in Norwegian (NULL keeps the English defaults)
dt_language <- function(lang) {
  if (!identical(lang, "no")) return(NULL)
  list(search = "Søk:", lengthMenu = "Vis _MENU_ rader",
       info = "Viser _START_ til _END_ av _TOTAL_ rader", infoEmpty = "Ingen rader",
       infoFiltered = "(filtrert fra _MAX_ rader)", zeroRecords = "Ingen treff",
       paginate = list(previous = "Forrige side", `next` = "Neste side"))
}

contribute_css <- function(ns) {
  sprintf(paste(
    ".contribute-dt thead th[title] { text-decoration: underline dotted; cursor: help; }",
    "#%1$s table.dataTable tbody tr.selected > * {",
    "  box-shadow: inset 0 0 0 9999px %2$s !important; background-color: %2$s !important;",
    "  color: #fff !important; }",
    ".contribute-toolbar-row { display: flex; flex-wrap: wrap; align-items: center; gap: 6px; margin-bottom: 8px; }",
    ".contribute-toolbar-label { min-width: 12em; }",
    ".contribute-toolbar-row .btn { margin: 0; }",
    ".contribute-undo { margin-left: auto; }",
    ".contribute-details > summary { display: list-item; cursor: pointer; color: #3c8dbc; }",
    ".contribute-details > summary:hover { text-decoration: underline; }",
    sep = "\n"), ns("table"), contribute_colors[["selected"]])
}

# Browser side of the module: reorders the review table on request, and tells
# the server when the user has left a filled-in email field
contribute_js <- function(ns) {
  sprintf(paste(
    "Shiny.addCustomMessageHandler('%1$s', function(m) {",
    "  var table = $('#' + m.id).data('datatable');",
    "  if (table) table.order(m.order).draw();",
    "});",
    "$(document).on('focusout', '#%2$s', function() {",
    "  if (this.value) Shiny.setInputValue('%3$s', true);",
    "});",
    sep = "\n"), ns("order"), ns("contactElectronicMailAddress"), ns("email_touched"))
}

#############
# MODULE UI #
#############

mod_contribute_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(contribute_css(ns))),
              tags$script(HTML(contribute_js(ns)))),
    fluidRow(
      box(width = 12,
          uiOutput(ns("progress")),
          tabsetPanel(
            id = ns("wizard"), type = "hidden",
            tabPanelBody("intro", uiOutput(ns("step_intro"))),
            tabPanelBody("measure", uiOutput(ns("step_measure"))),
            tabPanelBody("review",
                         uiOutput(ns("step_review")),
                         fluidRow(
                           column(7,
                                  uiOutput(ns("review_tools")),
                                  DT::DTOutput(ns("table")),
                                  uiOutput(ns("review_columns"))),
                           column(5, leafletOutput(ns("map"), height = 650))
                         )),
            tabPanelBody("metadata", uiOutput(ns("step_metadata"))),
            tabPanelBody("validate",
                         uiOutput(ns("step_validate")),
                         uiOutput(ns("validate_drafts")),
                         uiOutput(ns("validate_metadata")),
                         DT::DTOutput(ns("preview"))),
            tabPanelBody("download", uiOutput(ns("step_download")))
          ),
          hr(),
          uiOutput(ns("nav"))
      )
    )
  )
}

#################
# MODULE SERVER #
#################

#' @param zip_path reactive returning the path of the uploaded zip (NULL before
#'   an upload). The test data cannot be contributed, so they never come here.
#' @param lang reactive: "no" or "en".
#' @param tab_name tabName of the contribute tab in the sidebar menu.
#' @param menu_id id of the sidebarMenu.
#' @return list with `invite()`: shows the invitation popup (once per session).
mod_contribute_server <- function(id, zip_path, lang,
                                  tab_name = "contribute", menu_id = "tabs") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    std <- load_standard()
    tr <- function(key) contribute_tr(key, lang())
    val <- function(name, default = NULL) isolate(input[[name]]) %||% default

    step <- reactiveVal("intro")
    invited <- reactiveVal(FALSE)
    edits <- reactiveVal(NULL)
    # Selected rows, shared by the review table, the map and the buttons
    selected <- reactiveVal(integer(0))
    # Rows moved to the top of the review table
    on_top <- reactiveVal(integer(0))
    drafts <- reactiveValues(methods = NULL, bibliographicCitation = NULL)

    # The uploaded zip is read as soon as it arrives, without waiting for the
    # volume calculation: list(shape, points), list(error = code) or NULL
    dataset <- reactive({
      path <- zip_path()
      if (is.null(path)) return(NULL)
      dir <- tempfile("contribute_")
      on.exit(unlink(dir, recursive = TRUE), add = TRUE)
      tryCatch(read_dataset_zip(path, dir), error = function(e) list(error = "bad_zip"))
    })
    points <- reactive(dataset()$points)
    shape <- reactive(dataset()$shape)

    has_points <- reactive(!is.null(points()) && nrow(points()) > 0)
    # The "reached the bottom?" question is skipped when the upload has its own column
    asks_bottom <- reactive(has_points() && !upload_has(points(), "probeReachedBottom", complete = FALSE))

    # New upload: start over (the intro says if the zip cannot be used)
    observeEvent(dataset(), {
      edits(empty_edits(if (has_points()) nrow(points()) else 0))
      selected(integer(0))
      on_top(integer(0))
      step("intro")
    })

    observeEvent(step(), updateTabsetPanel(session, "wizard", selected = step()))

    #################
    # INVITATION    #
    #################
    # Someone who has opened the tab already needs no invitation
    observe({
      if (identical(session$rootScope()$input[[menu_id]], tab_name)) invited(TRUE)
    })

    invite <- function() {
      if (isolate(invited())) return(invisible())
      invited(TRUE)
      l <- isolate(lang())
      showModal(modalDialog(
        title = contribute_tr("Vil du bidra med torvdybdene dine?", l),
        p(contribute_tr("Målingene dine kan bli treningsdata for nasjonal kartlegging av torv. Vi hjelper deg å gjøre dem om til en standard datafil med noen få spørsmål. Det er frivillig, og ingenting lagres på serveren.", l)),
        footer = tagList(
          modalButton(contribute_tr("Ikke nå", l)),
          actionButton(ns("invite_yes"), contribute_tr("Ja, vis meg", l), class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    }

    observeEvent(input$invite_yes, {
      removeModal()
      shinydashboard::updateTabItems(session$rootScope(), menu_id, tab_name)
      step("intro")
    })

    #################
    # ANSWERS       #
    #################
    uncertainty <- reactive({
      pos <- input$positioning
      if (is.null(pos) || pos == "") return(NA_real_)
      if (pos == "custom") return(suppressWarnings(as.numeric(input$uncertainty_custom)))
      p <- Filter(function(x) identical(x$id, pos), std$no_profile$positioning)
      if (length(p)) as.numeric(p[[1]]$uncertainty_m) else NA_real_
    })

    record_date <- reactive({
      d <- input$recordDate
      if (is.null(d) || all(is.na(d))) return(NA_character_)
      d <- format(as.Date(d), "%Y-%m-%d")
      if (is.na(d[2]) || d[1] == d[2]) d[1] else paste0(d[1], "/", d[2])
    })

    # One of the profile's choices, or "Nearest <n>cm" for another resolution
    depth_resolution <- reactive({
      res <- input$peatDepthResolution
      if (!identical(res, "custom")) return(res)
      n <- suppressWarnings(as.numeric(input$resolution_custom))
      if (length(n) == 0 || is.na(n) || n <= 0) NA_character_ else resolution_value(n)
    })

    answers <- reactive(list(
      recordDate = record_date(),
      surveyor = input$surveyor,
      organizationName = input$organizationName,
      location = input$location,
      probeType = input$probeType,
      peatDepthResolution = depth_resolution(),
      positioning = input$positioning,
      coordinateUncertainty = uncertainty(),
      peatDepthRemarks = input$peatDepthRemarks,
      bottom = input$bottom
    ))

    # Reactives and observers below call req(has_points()) first and evaluate
    # reactives into local variables before passing them on. In the full app,
    # raster makes nrow(), %in%, as.data.frame() etc. S4 generics, and S4
    # dispatch turns a quiet req() stop inside their arguments into a real
    # error that ends the session.
    records <- reactive({
      req(has_points())
      pts <- points()
      ans <- answers()
      r <- standardize_points(pts, ans, std)
      apply_edits(r, edits())
    })

    # The licence list offers profile choices: NLOD 2.0 is written as "Other" + rights
    license_terms <- reactive({
      key <- input$license
      if (is.null(key) || key == "") return(list(license = NA_character_, rights = NA_character_))
      choice <- std_choice(std, "license", key)
      list(license = choice$value %||% key,
           rights = choice$rights %||% (if (identical(key, "Other")) input$rights else NA_character_))
    })

    meta_answers <- reactive({
      req(has_points())
      lic <- license_terms()
      r <- records()
      m <- list(
        title = input$title,
        organizationName = input$organizationName,
        abstract = input$abstract,
        purpose = input$purpose,
        contactElectronicMailAddress = input$contactElectronicMailAddress,
        license = lic$license,
        rights = lic$rights,
        geographicDescription = input$geographicDescription
      )
      # Drafts stand in until the check step has shown its text boxes
      if (is.null(input$methods)) {
        ans <- answers()
        m$methods <- draft_methods(ans, r, std, lang())
      } else {
        m$methods <- input$methods
      }
      m$bibliographicCitation <- input$bibliographicCitation %||% draft_citation(m, r)
      m
    })

    metadata <- reactive({
      req(has_points())
      m <- meta_answers()
      r <- records()
      build_metadata(m, r, std)
    })

    issues <- reactive({
      req(has_points())
      l <- lang()
      r <- records()
      m <- metadata()
      rbind(validate_against_standard(r, std, "peat_data", l),
            validate_against_standard(m, std, "metadata", l))
    })

    #################
    # FIELD CHECKS  #
    #################
    required_msg <- function() if (lang() == "no") "Må fylles ut" else "Required"

    iv_intro <- shinyvalidate::InputValidator$new()
    iv_intro$add_rule("consent", function(value) {
      if (!isTRUE(value)) {
        if (lang() == "no") "Kryss av for å gå videre" else "Tick the box to continue"
      }
    })

    iv_measure <- shinyvalidate::InputValidator$new()
    iv_measure$add_rule("recordDate", function(value) {
      if (has_points() && upload_has(points(), "recordDate")) return(NULL)
      if (is.null(value) || any(is.na(value))) return(required_msg())
      if (as.Date(value[2]) < as.Date(value[1])) {
        if (lang() == "no") "Sluttdato er før startdato" else "End date is before start date"
      }
    })
    iv_measure$add_rule("surveyor", function(value) {
      if (has_points() && upload_has(points(), "surveyor")) return(NULL)
      if (is_blank(value %||% "")) required_msg()
    })
    iv_measure$add_rule("bottom", function(value) {
      if (has_points() && upload_has(points(), "probeReachedBottom", complete = FALSE)) return(NULL)
      if (is.null(value) || value == "") required_msg()
    })
    iv_measure$add_rule("positioning", function(value) {
      if (is.null(value) || value == "") required_msg()
    })
    above_zero <- function(value) {
      if (is.null(value) || is.na(value) || value <= 0) {
        if (lang() == "no") "Oppgi et tall større enn 0" else "Enter a number above 0"
      }
    }
    iv_measure$add_rule("uncertainty_custom", function(value) {
      if (identical(input$positioning, "custom")) above_zero(value)
    })
    iv_measure$add_rule("resolution_custom", function(value) {
      if (identical(input$peatDepthResolution, "custom")) above_zero(value)
    })

    iv_meta <- shinyvalidate::InputValidator$new()
    for (name in c("title", "organizationName", "abstract", "license")) {
      local({
        nm <- name
        iv_meta$add_rule(nm, function(value) if (is_blank(value %||% "")) required_msg())
      })
    }
    iv_meta$add_rule("rights", function(value) {
      if (identical(input$license, "Other") && is_blank(value %||% "")) required_msg()
    })

    # The email address has its own validator, so it can be checked as soon as
    # the user leaves the field (see contribute_js()), not only on "Neste"
    iv_email <- shinyvalidate::InputValidator$new()
    iv_email$add_rule("contactElectronicMailAddress", function(value) {
      if (is_blank(value %||% "")) return(required_msg())
      if (!grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", trimws(value))) {
        if (lang() == "no") "Ugyldig e-postadresse" else "Not a valid email address"
      }
    })
    observeEvent(input$email_touched, iv_email$enable(), once = TRUE)

    validators <- list(intro = list(iv_intro), measure = list(iv_measure),
                       metadata = list(iv_meta, iv_email))

    # Problems that keep the user from leaving a step (NULL when there are none)
    step_problem <- function(s) {
      ivs <- validators[[s]]
      for (iv in ivs) iv$enable()
      if (!all(vapply(ivs, function(iv) iv$is_valid(), TRUE))) {
        return(tr("Noen felt må fylles ut eller rettes."))
      }
      if (s == "review" && asks_bottom() && identical(input$bottom, "some")) {
        r <- records()
        if (!any(r$probeReachedBottom %in% "false")) {
          return(tr("Du svarte at dere ikke nådde bunnen i noen punkter, men ingen punkter er merket. Velg punktene og trykk «Nådde ikke bunnen», eller endre svaret i steg 2."))
        }
      }
      if (s == "validate") {
        iss <- issues()
        if (any(iss$level == "error")) {
          return(tr("Noe må rettes før du kan laste ned. Gå tilbake og rett feltene nedenfor."))
        }
      }
      NULL
    }

    # Going forward runs the checks of every step passed, as "Neste" does
    go_to <- function(target) {
      if (!has_points() || !target %in% contribute_steps) return(invisible())
      from <- match(step(), contribute_steps)
      to <- match(target, contribute_steps)
      if (to > from) {
        for (s in contribute_steps[from:(to - 1)]) {
          problem <- step_problem(s)
          if (!is.null(problem)) {
            step(s)
            showNotification(problem, type = "warning")
            return(invisible())
          }
        }
      }
      step(target)
    }

    #################
    # NAVIGATION    #
    #################
    # Step names double as links
    output$progress <- renderUI({
      labels <- c(intro = tr("Om bidraget"), measure = tr("Om målingene"),
                  review = tr("Se over punktene"), metadata = tr("Om datasettet"),
                  validate = tr("Kontroll"), download = tr("Last ned"))
      cur <- match(step(), contribute_steps)
      tags$p(lapply(seq_along(contribute_steps), function(i) {
        s <- contribute_steps[i]
        lab <- paste0(i, ". ", labels[[s]])
        item <- if (i == cur) {
          tags$b(lab)
        } else if (has_points()) {
          tags$a(href = "#", lab,
                 onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                                   ns("goto"), s))
        } else {
          tags$span(lab, style = "color:#888")
        }
        tagList(item, if (i < length(contribute_steps)) " → ")
      }))
    })

    output$nav <- renderUI({
      i <- match(step(), contribute_steps)
      blocked <- FALSE
      if (step() == "validate" && has_points()) {
        iss <- issues()
        blocked <- any(iss$level == "error")
      }
      can_next <- i < length(contribute_steps) && has_points() && !blocked
      tagList(
        if (i > 1) actionButton(ns("back"), tr("Tilbake"), icon = icon("arrow-left")),
        if (can_next)
          actionButton(ns("next_step"), tr("Neste"), icon = icon("arrow-right"), class = "btn-primary")
      )
    })

    observeEvent(input$back, {
      i <- match(step(), contribute_steps)
      if (i > 1) step(contribute_steps[i - 1])
    })

    observeEvent(input$next_step, {
      i <- match(step(), contribute_steps)
      if (i < length(contribute_steps)) go_to(contribute_steps[i + 1])
    })

    observeEvent(input$goto, go_to(input$goto))

    #################
    # 1. INTRO      #
    #################
    # Why an uploaded zip cannot be used (codes from read_dataset_zip())
    upload_problem <- function(code) {
      switch(code,
             no_shp = tr("Zip-filen mangler en .shp-fil med området."),
             no_prj = tr("Zip-filen mangler en .prj-fil med koordinatsystemet."),
             no_csv = tr("Zip-filen mangler en .csv-fil med torvdybdene."),
             corrupt_shp = tr("Shapefilen i zip-filen kan ikke leses."),
             bad_csv = tr("CSV-filen i zip-filen må ha kolonnene x, y og torvdybde_cm."),
             tr("Zip-filen kan ikke leses."))
    }

    output$step_intro <- renderUI({
      l <- lang()
      error <- dataset()$error
      tagList(
        h3(tr("Bidra med torvdybdene dine")),
        if (!is.null(error)) {
          div(class = "alert alert-danger", upload_problem(error), " ",
              tr("Se bruksanvisningen for hvordan zip-filen skal se ut, og last den opp på nytt."))
        } else if (!has_points()) {
          div(class = "alert alert-info",
              tr("Last opp en zip-fil med målingene dine under «Last opp data». Du trenger ikke å beregne volumet først."))
        } else {
          div(class = "alert alert-success",
              sprintf(tr("Zip-filen din er lest inn: %s punkter med torvdybde."), nrow(points())))
        },
        p(tr("Torvdybder er verdifulle utover ditt eget prosjekt. Samlet kan de bli treningsdata for bedre kart over torv i Norge.")),
        p(tr("Her gjør vi målingene dine om til en datafil som følger «Data Exchange Standard for Peat Surveys» (Natural England, v1.0) med en norsk profil. Du svarer på noen få spørsmål om målingene og datasettet, og laster ned en zip-fil.")),
        tags$ul(
          tags$li(tr("Det er frivillig.")),
          tags$li(tr("I denne versjonen lagres ingenting på serveren. Du laster ned filen og bestemmer selv hva du gjør med den.")),
          tags$li(tr("Du velger selv lisens for dataene."))
        ),
        h4(tr("Personopplysninger")),
        p(tr("Feltet «Hvem målte?» og e-postadressen til kontaktpersonen er personopplysninger. Du kan bruke navnet på en organisasjon og en felles e-postadresse i stedet. Hvis du deler filen videre, må mottakeren behandle disse opplysningene etter personvernreglene (GDPR).")),
        checkboxInput(ns("consent"), tr("Jeg har lest dette og vil gå videre"), value = val("consent", FALSE))
      )
    })

    #################
    # 2. MEASURE    #
    #################
    output$step_measure <- renderUI({
      l <- lang()
      pts <- if (has_points()) points()
      has_date <- !is.null(pts) && upload_has(pts, "recordDate")
      has_surveyor <- !is.null(pts) && upload_has(pts, "surveyor")
      pos <- std$no_profile$positioning
      pos_choices <- stats::setNames(vapply(pos, `[[`, "", "id"),
                                     vapply(pos, function(p) p$label[[l]], ""))
      d <- val("recordDate", c(NA, NA))

      tagList(
        h3(tr("Om målingene")),
        p(tr("Svarene gjelder alle punktene. I neste steg kan du se over punktene og rette enkeltpunkter.")),
        fluidRow(
          column(6,
                 # NA dates give an empty field (Shiny warns about them)
                 suppressWarnings(
                   dateRangeInput(ns("recordDate"),
                                  paste(std_label(std, "recordDate", l), if (!has_date) "*"),
                                  start = d[1], end = d[2], format = "yyyy-mm-dd",
                                  weekstart = 1, language = if (l == "no") "nb" else "en",
                                  separator = "–", width = "100%")),
                 std_help_text(std, "recordDate", l),
                 if (has_date) helpText(tr("Filen din har allerede dato for alle punkter. Den brukes.")),
                 std_input(std, "surveyor", ns, l, val("surveyor"), required = !has_surveyor),
                 std_input(std, "probeType", ns, l, val("probeType")),
                 # The profile's resolutions, or "Other" with a number of cm
                 radioButtons(ns("peatDepthResolution"), paste(std_label(std, "peatDepthResolution", l), "*"),
                              choices = c(std_choices(std, "peatDepthResolution", l),
                                          stats::setNames("custom", tr("Annen"))),
                              selected = val("peatDepthResolution", std_default(std, "peatDepthResolution")),
                              inline = TRUE, width = "100%"),
                 conditionalPanel(sprintf("input['%s'] == 'custom'", ns("peatDepthResolution")),
                                  numericInput(ns("resolution_custom"), tr("Oppløsning (cm)"),
                                               value = val("resolution_custom", NA), min = 0, step = 1)),
                 std_help_text(std, "peatDepthResolution", l)
          ),
          column(6,
                 if (!is.null(pts) && !asks_bottom()) {
                   tagList(tags$label(std_label(std, "probeReachedBottom", l)),
                           helpText(tr("Filen din har allerede en kolonne probeReachedBottom, og verdiene brukes. Tomme eller ukjente verdier regnes som «nådde ikke bunnen».")))
                 } else {
                   tagList(
                     radioButtons(ns("bottom"), paste(std_label(std, "probeReachedBottom", l), "*"),
                                  choiceNames = list(tr("Ja, i alle punktene"),
                                                     tr("Nei eller usikker i noen av punktene"),
                                                     tr("Nei eller usikker i alle punktene")),
                                  choiceValues = list("all", "some", "none"),
                                  selected = val("bottom", character(0)), width = "100%"),
                     std_help_text(std, "probeReachedBottom", l),
                     conditionalPanel(sprintf("input['%s'] == 'some'", ns("bottom")),
                                      div(class = "alert alert-info",
                                          tr("I neste steg merker du punktene der dere ikke nådde bunnen. Alle andre punkter regnes som målt helt til bunnen."))),
                     conditionalPanel(sprintf("input['%s'] == 'none'", ns("bottom")),
                                      div(class = "alert alert-info",
                                          tr("Alle dybdene regnes da som minsteverdier.")))
                   )
                 },
                 radioButtons(ns("positioning"), paste(std_label(std, "coordinateUncertainty", l), "*"),
                              choices = pos_choices, selected = val("positioning", character(0)),
                              width = "100%"),
                 conditionalPanel(sprintf("input['%s'] == 'custom'", ns("positioning")),
                                  numericInput(ns("uncertainty_custom"), tr("Usikkerhet (m)"),
                                               value = val("uncertainty_custom", NA), min = 0, step = 0.1)),
                 std_help_text(std, "coordinateUncertainty", l),
                 std_input(std, "peatDepthRemarks", ns, l, val("peatDepthRemarks"), multiline = TRUE)
          )
        )
      )
    })

    #################
    # 3. REVIEW     #
    #################
    output$step_review <- renderUI({
      req(has_points())
      must_mark <- asks_bottom() && identical(input$bottom, "some")
      r <- records()
      n_no <- sum(r$probeReachedBottom %in% "false")
      extras <- contribute_extra_columns(points(), std)
      tagList(
        h3(tr("Se over punktene")),
        p(sprintf(tr("Tabellen viser de %s punktene slik de blir lagret, med svarene fra forrige steg fylt inn. Den er sortert etter torvdybde, dypest først. Klikk på en kolonneoverskrift for å sortere på en annen måte."),
                  nrow(points()))),
        if (must_mark && n_no == 0) {
          div(class = "alert alert-warning", tags$b(tr("Påkrevd:")), " ",
              tr("Du svarte at dere ikke nådde bunnen i noen punkter. Velg disse punktene i tabellen eller på kartet, og trykk «Nådde ikke bunnen»."))
        } else if (must_mark) {
          div(class = "alert alert-success", tags$b(tr("Påkrevd:")), " ",
              sprintf(if (n_no == 1) tr("%s punkt er merket som «Nådde ikke bunnen». Merk flere om nødvendig, eller gå videre.")
                      else tr("%s punkter er merket som «Nådde ikke bunnen». Merk flere om nødvendig, eller gå videre."),
                      n_no))
        } else {
          p(tags$b(tr("Påkrevd:")), " ", tr("Ingenting. Gå videre hvis alt ser riktig ut."))
        },
        tags$b(tr("Valgfritt:")),
        tags$ul(
          tags$li(tr("Rett dato eller skriv merknader for enkeltpunkter: dobbeltklikk på en celle i kolonnene recordDate, peatDepthRemarks eller locationRemarks.")),
          tags$li(tr("Velg punkter ved å klikke på rader i tabellen eller på punkter i kartet. Med knappene over tabellen merker du om bunnen ble nådd, eller flytter de valgte punktene øverst i tabellen.")),
          tags$li(tr("Hold musepekeren over en kolonneoverskrift for å se hva kolonnen betyr, eller åpne «Hva betyr kolonnene?» under tabellen."))
        ),
        if (length(extras)) {
          p(tags$i(paste(tr("Ekstra kolonner fra filen din tas med:"), paste(extras, collapse = ", "))))
        }
      )
    })

    # Two rows: the selection (count first), then what to do with it
    output$review_tools <- renderUI({
      req(has_points())
      tagList(
        div(class = "contribute-toolbar-row",
            tags$b(class = "contribute-toolbar-label", textOutput(ns("selection_count"), inline = TRUE)),
            actionButton(ns("select_all"), tr("Velg alle"), icon = icon("check-double"), class = "btn-sm"),
            actionButton(ns("select_none"), tr("Fjern valg"), icon = icon("xmark"), class = "btn-sm"),
            actionButton(ns("move_to_top"), tr("Flytt valgte øverst"), icon = icon("arrow-up"), class = "btn-sm")),
        div(class = "contribute-toolbar-row",
            tags$span(class = "contribute-toolbar-label", tr("Merk valgte punkter:")),
            actionButton(ns("mark_yes"), tr("Nådde bunnen"), icon = icon("check"), class = "btn-sm"),
            actionButton(ns("mark_no"), tr("Nådde ikke bunnen"), icon = icon("arrow-down"), class = "btn-sm"),
            actionLink(ns("clear_edits"), tr("Angre alle endringer"), icon = icon("rotate-left"),
                       class = "contribute-undo"))
      )
    })

    output$selection_count <- renderText({
      req(has_points())
      sprintf(tr("%s av %s punkter valgt"), length(selected()), nrow(points()))
    })

    # Hover text of the table headings
    column_tips <- function(columns) {
      unname(vapply(columns, function(name) {
        if (name == "point") tr("Punktnummer: rekkefølgen i CSV-filen din.") else std_column_help(std, name, lang())
      }, ""))
    }

    output$review_columns <- renderUI({
      req(has_points())
      tips <- column_tips(contribute_review_columns)
      tags$details(class = "contribute-details", style = "margin-top: 10px;",
                   tags$summary(tr("Hva betyr kolonnene?")),
                   tags$dl(lapply(seq_along(tips), function(i) {
                     tagList(tags$dt(tags$code(contribute_review_columns[i])), tags$dd(tips[i]))
                   })))
    })

    # The last column is the hidden sort key for "Move selected to top"
    review_data <- reactive({
      r <- records()
      top <- on_top()
      d <- cbind(point = seq_len(nrow(r)),
                 as_display(r[, setdiff(contribute_review_columns, "point"), drop = FALSE], std))
      d[[contribute_top_column]] <- as.integer(seq_len(nrow(r)) %in% top)
      d
    })

    # Table column (0-based, as DataTables counts) of a review_data() column
    dt_column <- function(name) match(name, c(contribute_review_columns, contribute_top_column)) - 1

    output$table <- DT::renderDT({
      req(has_points())
      points()
      l <- lang()
      d <- isolate(review_data())
      sel <- isolate(selected())
      sel <- sel[sel <= nrow(d)]
      DT::datatable(
        d, rownames = FALSE,
        container = dt_header(names(d), column_tips(names(d))),
        selection = list(mode = "multiple", selected = if (length(sel)) sel),
        editable = list(target = "cell",
                        disable = list(columns = which(!names(d) %in% contribute_editable) - 1)),
        # A fixed body height keeps the headings in view when scrolling through many rows
        options = list(pageLength = 15, lengthMenu = c(15, 50, 100, 500), scrollX = TRUE,
                       scrollY = "450px", scrollCollapse = TRUE, autoWidth = FALSE,
                       order = list(list(dt_column("surfacePeatDepth"), "desc")),
                       columnDefs = list(list(visible = FALSE, targets = dt_column(contribute_top_column))),
                       language = dt_language(l)))
    })

    table_proxy <- DT::dataTableProxy("table")
    # TRUE after a cell edit that the table shows as it is stored. Then only the
    # server's copy of the data is updated: reloading would redraw the table and
    # drop an edit the user has just started in another cell.
    edit_shown <- FALSE
    observe({
      req(has_points())
      d <- review_data()
      if (edit_shown) {
        edit_shown <<- FALSE
        DT::dataTableAjax(session, d, rownames = FALSE, outputId = "table")
      } else {
        DT::replaceData(table_proxy, d, rownames = FALSE, resetPaging = FALSE,
                        clearSelection = "none")
      }
    })

    # The rows selected now go to the top (deepest first) and stay there, also
    # when the selection changes, until the button is used again or the user
    # sorts by another column
    observeEvent(input$move_to_top, {
      rows <- selected()
      if (length(rows) == 0) {
        showNotification(tr("Velg ett eller flere punkter først."), type = "message")
        return()
      }
      on_top(rows)
      DT::clearSearch(table_proxy)
      session$sendCustomMessage(ns("order"), list(
        id = ns("table"),
        order = list(list(dt_column(contribute_top_column), "desc"),
                     list(dt_column("surfacePeatDepth"), "desc"))))
    })

    observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      name <- names(review_data())[info$col + 1]
      req(name %in% contribute_editable)
      v <- trimws(as.character(info$value))
      if (name == "probeReachedBottom") {
        v <- normalize_boolean(v)
        if (is.na(v)) {
          showNotification(tr("Skriv true eller false (eller ja/nei)."), type = "warning")
          v <- NA_character_
        }
      } else if (v == "") {
        v <- NA_character_
      }
      e <- edits()
      # An emptied cell falls back to the dataset-wide answer
      e[info$row, name] <- v
      # The table shows the edit already, unless the value was changed here
      edit_shown <<- !identical(e, edits()) && !is.na(v) && identical(v, as.character(info$value))
      edits(e)
    })

    # Table clicks, map clicks and the buttons all go through `selected`
    set_selected <- function(rows, push = TRUE) {
      rows <- sort(unique(as.integer(rows)))
      selected(rows)
      if (push) DT::selectRows(table_proxy, if (length(rows)) rows)
    }
    observeEvent(input$table_rows_selected, set_selected(input$table_rows_selected, push = FALSE),
                 ignoreNULL = FALSE)
    # "Select all" respects the table's search box
    observeEvent(input$select_all, set_selected(input$table_rows_all %||% seq_len(nrow(points()))))
    observeEvent(input$select_none, set_selected(integer(0)))
    observeEvent(input$map_marker_click, {
      row <- suppressWarnings(as.integer(sub("^sel_", "", input$map_marker_click$id)))
      req(!is.na(row))
      rows <- selected()
      set_selected(if (row %in% rows) setdiff(rows, row) else c(rows, row))
    })

    mark_bottom <- function(value) {
      rows <- selected()
      if (length(rows) == 0) {
        showNotification(tr("Velg ett eller flere punkter først."), type = "message")
        return()
      }
      e <- edits()
      e[rows, "probeReachedBottom"] <- value
      edits(e)
      # Clear the selection so that the new colours show on the map
      set_selected(integer(0))
    }
    observeEvent(input$mark_no, mark_bottom("false"))
    observeEvent(input$mark_yes, mark_bottom("true"))
    observeEvent(input$clear_edits, edits(empty_edits(nrow(points()))))

    point_labels <- function(r) {
      paste0("#", seq_len(nrow(r)), ": ", r$surfacePeatDepth, " cm",
             ifelse(r$probeReachedBottom %in% "false", paste0(" (", tr("nådde ikke bunnen"), ")"), ""))
    }

    draw_points <- function(map, r) {
      map %>%
        clearGroup("points") %>%
        addCircleMarkers(lng = as.numeric(r$longitude), lat = as.numeric(r$latitude),
                         layerId = as.character(seq_len(nrow(r))), group = "points",
                         radius = 5, stroke = TRUE, weight = 1, color = "black", opacity = 1,
                         fillOpacity = 0.9, label = point_labels(r),
                         fillColor = unname(ifelse(r$probeReachedBottom %in% "false",
                                                   contribute_colors[["no"]], contribute_colors[["yes"]])))
    }

    # Selected points are drawn on top, with their own ids so both layers stay
    draw_selected <- function(map, r, rows) {
      map <- map %>% clearGroup("selected")
      rows <- rows[rows <= nrow(r)]
      if (length(rows) == 0) return(map)
      map %>%
        addCircleMarkers(lng = as.numeric(r$longitude[rows]), lat = as.numeric(r$latitude[rows]),
                         layerId = paste0("sel_", rows), group = "selected",
                         radius = 7, stroke = TRUE, weight = 2, color = "black", opacity = 1,
                         fillOpacity = 1, fillColor = contribute_colors[["selected"]],
                         label = point_labels(r)[rows])
    }

    output$map <- renderLeaflet({
      req(has_points())
      lang()
      r <- isolate(records())
      m <- leaflet() %>% addTiles()
      if (!is.null(shape())) {
        m <- m %>% addPolygons(data = sf::st_transform(shape(), 4326), fillOpacity = 0.1, weight = 1.5)
      }
      m <- m %>% addLegend("bottomright", opacity = 0.9,
                           colors = unname(contribute_colors[c("yes", "no", "selected")]),
                           labels = c(tr("Nådde bunnen"), tr("Nådde ikke bunnen"), tr("Valgt")))
      draw_selected(draw_points(m, r), r, isolate(selected()))
    })

    # Recolour after edits (and redraw the selection on top), or redraw the selection
    observe({
      req(has_points())
      r <- records()
      rows <- isolate(selected())
      draw_selected(draw_points(leafletProxy("map"), r), r, rows)
    })
    observe({
      req(has_points())
      rows <- selected()
      r <- isolate(records())
      draw_selected(leafletProxy("map"), r, rows)
    })

    #################
    # 4. METADATA   #
    #################
    output$step_metadata <- renderUI({
      l <- lang()
      lic <- val("license", "CC-BY")
      tagList(
        h3(tr("Om datasettet")),
        p(tr("Dette beskriver datasettet som helhet. Felt merket * er påkrevd.")),
        fluidRow(
          column(6,
                 std_input(std, "title", ns, l, val("title")),
                 std_input(std, "organizationName", ns, l, val("organizationName")),
                 std_input(std, "abstract", ns, l, val("abstract"), multiline = TRUE),
                 std_input(std, "purpose", ns, l, val("purpose"), multiline = TRUE)
          ),
          column(6,
                 std_input(std, "contactElectronicMailAddress", ns, l, val("contactElectronicMailAddress")),
                 std_input(std, "license", ns, l, lic),
                 conditionalPanel(sprintf("input['%s'] == 'Other'", ns("license")),
                                  std_input(std, "rights", ns, l, val("rights"))),
                 std_input(std, "location", ns, l, val("location"), required = FALSE),
                 std_input(std, "geographicDescription", ns, l, val("geographicDescription"))
          )
        )
      )
    })

    # Hidden steps keep their inputs (and values) alive, so later steps can use them
    for (o in c("step_intro", "step_measure", "step_metadata")) {
      outputOptions(output, o, suspendWhenHidden = FALSE)
    }

    #################
    # 5. VALIDATE   #
    #################
    output$step_validate <- renderUI({
      req(has_points())
      r <- records()
      iss <- issues()
      s <- summarise_issues(iss)
      n_err <- sum(s$level == "error")
      tagList(
        h3(tr("Kontroll")),
        if (n_err == 0) {
          div(class = "alert alert-success",
              sprintf(tr("Alle %s punkter følger standarden. Du kan gå videre til nedlasting."), nrow(r)))
        } else {
          div(class = "alert alert-danger",
              tr("Noe må rettes før du kan laste ned. Gå tilbake og rett feltene nedenfor."))
        },
        if (nrow(s) > 0) {
          tags$table(class = "table table-condensed",
                     tags$thead(tags$tr(tags$th(tr("Type")), tags$th(tr("Melding")),
                                        tags$th(tr("Antall punkter")), tags$th(tr("Punkter")))),
                     tags$tbody(lapply(seq_len(nrow(s)), function(i) {
                       tags$tr(tags$td(if (s$level[i] == "error") tr("Feil") else tr("Advarsel")),
                               tags$td(s$message[i]), tags$td(s$n[i]), tags$td(s$rows[i]))
                     })))
        }
      )
    })

    # Drafts are rebuilt each time the step opens, unless the user has edited them.
    # Kept apart from the summary so typing does not re-render the text boxes.
    output$validate_drafts <- renderUI({
      req(has_points(), step() == "validate")
      l <- lang()
      new <- isolate({
        r <- records()
        ans <- answers()
        m <- meta_answers()
        list(methods = draft_methods(ans, r, std, l), bibliographicCitation = draft_citation(m, r))
      })
      value <- function(name) {
        cur <- isolate(input[[name]])
        if (is.null(cur) || is_blank(cur) || identical(cur, isolate(drafts[[name]]))) new[[name]] else cur
      }
      out <- fluidRow(
        column(6,
               textAreaInput(ns("methods"), std_label(std, "methods", l),
                             value = value("methods"), rows = 6, width = "100%"),
               std_help_text(std, "methods", l)),
        column(6,
               textAreaInput(ns("bibliographicCitation"), std_label(std, "bibliographicCitation", l),
                             value = value("bibliographicCitation"), rows = 3, width = "100%"),
               std_help_text(std, "bibliographicCitation", l))
      )
      drafts$methods <- new$methods
      drafts$bibliographicCitation <- new$bibliographicCitation
      out
    })

    output$validate_metadata <- renderUI({
      req(has_points())
      m <- metadata()
      tagList(
        h4(tr("Metadata")),
        tags$table(class = "table table-condensed",
                   tags$tbody(lapply(names(m), function(name) {
                     if (is_blank(m[[name]])) return(NULL)
                     tags$tr(tags$td(tags$code(name)), tags$td(m[[name]]))
                   }))),
        h4(tr("Forhåndsvisning av peat_data.csv"))
      )
    })

    output$preview <- DT::renderDT({
      req(has_points(), step() == "validate")
      r <- records()
      d <- as_display(r, std)
      DT::datatable(d, rownames = FALSE, selection = "none",
                    container = dt_header(names(d), column_tips(names(d))),
                    options = list(pageLength = 10, scrollX = TRUE, dom = "tip",
                                   language = dt_language(lang())))
    })

    #################
    # 6. DOWNLOAD   #
    #################
    output$step_download <- renderUI({
      req(has_points())
      r <- records()
      tagList(
        h3(tr("Last ned")),
        p(tr("Zip-filen inneholder:")),
        tags$ul(
          tags$li(tags$code("peat_data.csv"), " – ",
                  sprintf(tr("%s punkter, én rad per måling"), nrow(r))),
          tags$li(tags$code("metadata.csv"), " – ", tr("beskrivelse av datasettet")),
          tags$li(tags$code("README.txt"), " – ", tr("hvilken standard og profil filene følger"))
        ),
        downloadButton(ns("download"), tr("Last ned standardiserte data")),
        br(), br(),
        helpText(tr("Ingenting er lagret på serveren. Ta vare på zip-filen. I en senere versjon kan du sende den inn direkte."))
      )
    })

    output$download <- downloadHandler(
      filename = function() paste0("peat_depths_standard_", Sys.Date(), ".zip"),
      content = function(file) {
        iss <- issues()
        if (any(iss$level == "error")) stop("The data do not pass validation.")
        r <- records()
        m <- metadata()
        dir <- tempfile("contribute_")
        on.exit(unlink(dir, recursive = TRUE), add = TRUE)
        paths <- write_standard_package(r, m, std, dir)
        utils::zip(zipfile = file, files = paths, flags = "-j9X")
      },
      contentType = "application/zip"
    )

    list(invite = invite)
  })
}
