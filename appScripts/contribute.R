## contribute.R ##
# Optional "Contribute your peat depths" module (Phase 1: standardize + download).
#
# Turns the uploaded peat depth points into records that follow the
# Data Exchange Standard for Peat Surveys v1.0 (corrected profile) with the
# CarbonViewer Norwegian profile (see standard/), and lets the user download
# them as a zip. Nothing is stored on the server.
#
# Switch off with options(carbonviewer.contribute = FALSE) before the app starts.

contribute_enabled <- function() isTRUE(getOption("carbonviewer.contribute", TRUE))

contribute_app_version <- "CarbonViewer 1.0.0"

# Wizard steps, in order
contribute_steps <- c("intro", "review", "measure", "metadata", "validate", "download")

# Columns the user can edit per row in the review table
contribute_editable <- c("recordDate", "probeReachedBottom", "peatDepthRemarks", "locationRemarks")

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

# Yes/No from free text ("ja", "true", "1", ...); NA when not recognised
normalize_yes_no <- function(x) {
  x <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(x))
  out[x %in% c("yes", "y", "ja", "j", "true", "t", "1")] <- "Yes"
  out[x %in% c("no", "n", "nei", "false", "f", "0")] <- "No"
  out
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
#' @param points sf points with `torvdybde_cm` (as in `df_reactive$points`).
#' @param answers named list: recordDate, surveyor, organizationName, location,
#'   probeType, peatDepthResolution, coordinateUncertainty, peatDepthRemarks and
#'   bottom ("all", "some" or "unsure").
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
                                   all = "Yes", some = "Yes", unsure = "No", NA_character_)

  # Values from the upload win, row by row
  derived <- c("latitude", "longitude", "easting", "northing", "geodeticDatum",
               "verbatimSRS", "surfacePeatDepth")
  up <- sf::st_drop_geometry(points)
  for (name in setdiff(attrs, derived)) {
    hit <- match(tolower(name), tolower(names(up)))
    if (is.na(hit)) next
    vals <- up[[hit]]
    vals <- if (inherits(vals, "Date")) format(vals, "%Y-%m-%d") else as.character(vals)
    if (name == "probeReachedBottom") {
      # Unrecognised or empty values become "No", as the standard advises when unsure
      vals <- normalize_yes_no(vals)
      vals[is.na(vals)] <- "No"
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
  n_no <- sum(records$probeReachedBottom == "No", na.rm = TRUE)
  probe <- answers$probeType
  probe_lab <- if (!is.null(probe) && !is_blank(probe)) {
    lab <- names(std_choices(std, "probeType", lang))[match(probe, std_choices(std, "probeType", lang))]
    tolower(sub(" \\(.*\\)$", "", lab))
  }
  pos <- Filter(function(p) identical(p$id, answers$positioning), std$no_profile$positioning)
  pos_lab <- if (length(pos)) pos[[1]]$methods[[lang]]
  unc <- answers$coordinateUncertainty
  res <- answers$peatDepthResolution
  res_lab <- if (!is.null(res) && !is_blank(res)) {
    names(std_choices(std, "peatDepthResolution", lang))[match(res, std_choices(std, "peatDepthResolution", lang))]
  }
  v <- std_versions(std)

  if (lang == "no") {
    s <- paste0("Torvdybde ble målt i ", n, " punkter")
    if (!is.na(dates[1])) {
      s <- paste0(s, if (dates[1] == dates[2]) paste0(" den ", dates[1])
                  else paste0(" mellom ", dates[1], " og ", dates[2]))
    }
    if (!is.null(probe_lab)) s <- paste0(s, " med ", probe_lab)
    s <- paste0(s, ".")
    if (!is.null(res_lab) && !is.na(res_lab)) s <- paste0(s, " Dybden ble lest av til ", tolower(res_lab), ".")
    s <- paste0(s, if (n_no == 0) " Bunnen av torva ble nådd i alle punkter."
                else if (n_no == n) " Det er usikkert om bunnen av torva ble nådd; dybdene er minsteverdier."
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
    if (!is.null(probe_lab)) s <- paste0(s, " using a ", probe_lab)
    s <- paste0(s, ".")
    if (!is.null(res_lab) && !is.na(res_lab)) s <- paste0(s, " Depths were read to the ", tolower(res_lab), ".")
    s <- paste0(s, if (n_no == 0) " The bottom of the peat was reached at all points."
                else if (n_no == n) " It is uncertain whether the bottom of the peat was reached; depths are minimum values."
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
  lic <- if (is_blank(meta$license %||% "")) "" else paste0(" Licence: ", meta$license, ".")
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
  extras <- setdiff(names(records), std_names(std, "peat_data"))
  readme <- c(
    paste("Peat depth records:", metadata$title %||% ""),
    "",
    paste0("Created with ", contribute_app_version, " on ", format(Sys.Date(), "%Y-%m-%d"), "."),
    "",
    "Standard",
    paste0("  ", std$standard$citation),
    paste0("  Corrected machine-readable profile ", v$profile,
           " (corrections C1-C6, see standard/peat_standard_changes.csv in the CarbonViewer repository)."),
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
    "  - probeReachedBottom = No means the depth is a minimum value.",
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

#############
# MODULE UI #
#############

mod_contribute_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(width = 12,
        uiOutput(ns("progress")),
        tabsetPanel(
          id = ns("wizard"), type = "hidden",
          tabPanelBody("intro", uiOutput(ns("step_intro"))),
          tabPanelBody("review",
                       uiOutput(ns("step_review")),
                       leafletOutput(ns("map"), height = 350),
                       br(),
                       uiOutput(ns("review_tools")),
                       DT::DTOutput(ns("table"))),
          tabPanelBody("measure", uiOutput(ns("step_measure"))),
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
}

#################
# MODULE SERVER #
#################

#' @param points,shape reactives returning the uploaded points and area (EPSG:25833).
#' @param upload_state reactive: "uploaded", "test", ... (test data cannot be contributed).
#' @param lang reactive: "no" or "en".
#' @param tab_name tabName of the contribute tab in the sidebar menu.
#' @param menu_id id of the sidebarMenu.
#' @return list with `invite()`: shows the invitation popup (once per session).
mod_contribute_server <- function(id, points, shape, upload_state, lang,
                                  tab_name = "contribute", menu_id = "tabs") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    std <- load_standard()
    tr <- function(key) contribute_tr(key, lang())
    val <- function(name, default = NULL) isolate(input[[name]]) %||% default

    step <- reactiveVal("intro")
    invited <- reactiveVal(FALSE)
    edits <- reactiveVal(NULL)
    drafts <- reactiveValues(methods = NULL, bibliographicCitation = NULL)

    has_points <- reactive(!is.null(points()) && nrow(points()) > 0)
    is_test <- reactive(identical(upload_state(), "test"))
    ready <- reactive(has_points() && !is_test())

    # New data: start over
    observeEvent(points(), {
      edits(empty_edits(nrow(points())))
      step("intro")
    })

    observeEvent(step(), updateTabsetPanel(session, "wizard", selected = step()))

    #################
    # INVITATION    #
    #################
    invite <- function() {
      if (isolate(invited()) || !contribute_enabled()) return(invisible())
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

    answers <- reactive(list(
      recordDate = record_date(),
      surveyor = input$surveyor,
      organizationName = input$organizationName,
      location = input$location,
      probeType = input$probeType,
      peatDepthResolution = input$peatDepthResolution,
      positioning = input$positioning,
      coordinateUncertainty = uncertainty(),
      peatDepthRemarks = input$peatDepthRemarks,
      bottom = input$bottom
    ))

    records <- reactive({
      req(has_points())
      apply_edits(standardize_points(points(), answers(), std), edits())
    })

    meta_answers <- reactive(list(
      title = input$title,
      organizationName = input$organizationName,
      abstract = input$abstract,
      purpose = input$purpose,
      contactElectronicMailAddress = input$contactElectronicMailAddress,
      license = input$license,
      rights = input$rights,
      geographicDescription = input$geographicDescription,
      methods = input$methods,
      bibliographicCitation = input$bibliographicCitation
    ))

    metadata <- reactive(build_metadata(meta_answers(), records(), std))

    issues <- reactive({
      l <- lang()
      rbind(validate_against_standard(records(), std, "peat_data", l),
            validate_against_standard(metadata(), std, "metadata", l))
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
    iv_measure$add_rule("uncertainty_custom", function(value) {
      if (identical(input$positioning, "custom") &&
          (is.null(value) || is.na(value) || value <= 0)) {
        if (lang() == "no") "Oppgi et tall større enn 0" else "Enter a number above 0"
      }
    })

    iv_meta <- shinyvalidate::InputValidator$new()
    for (name in c("title", "organizationName", "abstract", "license")) {
      local({
        nm <- name
        iv_meta$add_rule(nm, function(value) if (is_blank(value %||% "")) required_msg())
      })
    }
    iv_meta$add_rule("contactElectronicMailAddress", function(value) {
      if (is_blank(value %||% "")) return(required_msg())
      if (!grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", trimws(value))) {
        if (lang() == "no") "Ugyldig e-postadresse" else "Not a valid email address"
      }
    })
    iv_meta$add_rule("rights", function(value) {
      if (identical(input$license, "Other") && is_blank(value %||% "")) required_msg()
    })

    validators <- list(intro = iv_intro, measure = iv_measure, metadata = iv_meta)

    #################
    # NAVIGATION    #
    #################
    output$progress <- renderUI({
      labels <- c(intro = tr("Om bidraget"), review = tr("Se over punktene"),
                  measure = tr("Om målingene"), metadata = tr("Om datasettet"),
                  validate = tr("Kontroll"), download = tr("Last ned"))
      cur <- match(step(), contribute_steps)
      tags$p(lapply(seq_along(contribute_steps), function(i) {
        lab <- paste0(i, ". ", labels[[contribute_steps[i]]])
        tagList(if (i == cur) tags$b(lab) else tags$span(lab, style = "color:#888"),
                if (i < length(contribute_steps)) " → ")
      }))
    })

    output$nav <- renderUI({
      i <- match(step(), contribute_steps)
      can_next <- i < length(contribute_steps) && ready() &&
        !(step() == "validate" && any(issues()$level == "error"))
      tagList(
        if (i > 1) actionButton(ns("back"), tr("Tilbake"), icon = icon("arrow-left")),
        if (i < length(contribute_steps) && can_next)
          actionButton(ns("next_step"), tr("Neste"), icon = icon("arrow-right"), class = "btn-primary")
      )
    })

    observeEvent(input$back, {
      i <- match(step(), contribute_steps)
      if (i > 1) step(contribute_steps[i - 1])
    })

    observeEvent(input$next_step, {
      req(ready())
      iv <- validators[[step()]]
      if (!is.null(iv)) {
        iv$enable()
        if (!iv$is_valid()) {
          showNotification(tr("Noen felt må fylles ut eller rettes."), type = "warning")
          return()
        }
      }
      i <- match(step(), contribute_steps)
      step(contribute_steps[i + 1])
    })

    #################
    # 1. INTRO      #
    #################
    output$step_intro <- renderUI({
      l <- lang()
      tagList(
        h3(tr("Bidra med torvdybdene dine")),
        if (!has_points()) {
          div(class = "alert alert-info",
              tr("Last opp en zip-fil og trykk «Beregn» først. Deretter kan du gjøre målingene om til en standard datafil her."))
        } else if (is_test()) {
          div(class = "alert alert-warning",
              tr("Testdataene kan ikke brukes til å bidra. Last opp dine egne målinger for å bruke denne funksjonen."))
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
    # 2. REVIEW     #
    #################
    output$step_review <- renderUI({
      req(has_points())
      tagList(
        h3(tr("Se over punktene")),
        p(sprintf(tr("%s punkter. Koordinater, dybde og standardverdier er fylt inn. Du kan rette dato og merknader per punkt ved å dobbeltklikke i tabellen."),
                  nrow(points()))),
        if (length(contribute_extra_columns(points(), std))) {
          p(tags$i(paste(tr("Ekstra kolonner fra filen din tas med:"),
                         paste(contribute_extra_columns(points(), std), collapse = ", "))))
        }
      )
    })

    output$review_tools <- renderUI({
      req(has_points())
      some <- identical(input$bottom, "some")
      div(
        if (some) div(class = "alert alert-info",
                      tr("Merk punktene der dere ikke nådde bunnen: velg radene i tabellen og trykk «Nådde ikke bunnen».")),
        actionButton(ns("mark_no"), tr("Nådde ikke bunnen"), icon = icon("arrow-down")),
        actionButton(ns("mark_yes"), tr("Nådde bunnen"), icon = icon("check")),
        actionButton(ns("clear_edits"), tr("Fjern endringer i tabellen"), icon = icon("rotate-left")),
        helpText(tr("Knappene gjelder de valgte radene. Gule punkter på kartet nådde ikke bunnen."))
      )
    })

    review_columns <- c("recordDate", "surfacePeatDepth", "probeReachedBottom",
                        "peatDepthRemarks", "locationRemarks",
                        "latitude", "longitude", "easting", "northing")

    review_data <- reactive({
      r <- records()
      cbind(point = seq_len(nrow(r)), r[, review_columns, drop = FALSE])
    })

    output$table <- DT::renderDT({
      req(has_points())
      points()
      d <- isolate(review_data())
      locked <- which(!names(d) %in% contribute_editable) - 1
      DT::datatable(d, rownames = FALSE, selection = "multiple",
                    editable = list(target = "cell", disable = list(columns = locked)),
                    options = list(pageLength = 25, scrollX = TRUE))
    })

    table_proxy <- DT::dataTableProxy("table")
    observe({
      DT::replaceData(table_proxy, review_data(), rownames = FALSE, resetPaging = FALSE,
                      clearSelection = "none")
    })

    observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      name <- names(review_data())[info$col + 1]
      req(name %in% contribute_editable)
      v <- trimws(as.character(info$value))
      if (name == "probeReachedBottom") {
        v <- normalize_yes_no(v)
        if (is.na(v)) {
          showNotification(tr("Skriv Yes eller No (Ja/Nei)."), type = "warning")
          v <- NA_character_
        }
      } else if (v == "") {
        v <- NA_character_
      }
      e <- edits()
      # An emptied cell falls back to the dataset-wide answer
      e[info$row, name] <- v
      edits(e)
    })

    mark_bottom <- function(value) {
      rows <- input$table_rows_selected
      if (length(rows) == 0) {
        showNotification(tr("Velg en eller flere rader i tabellen først."), type = "message")
        return()
      }
      e <- edits()
      e[rows, "probeReachedBottom"] <- value
      edits(e)
    }
    observeEvent(input$mark_no, mark_bottom("No"))
    observeEvent(input$mark_yes, mark_bottom("Yes"))
    observeEvent(input$clear_edits, edits(empty_edits(nrow(points()))))

    add_points <- function(map, r) {
      ll <- data.frame(lat = as.numeric(r$latitude), lon = as.numeric(r$longitude))
      no <- r$probeReachedBottom %in% "No"
      map %>%
        clearGroup("points") %>%
        addCircleMarkers(lng = ll$lon, lat = ll$lat, group = "points", radius = 4,
                         stroke = TRUE, weight = 1, color = "black", fillOpacity = 0.8,
                         fillColor = ifelse(no, "#f0ad4e", "#3c8dbc"),
                         label = paste0("#", seq_len(nrow(r)), ": ", r$surfacePeatDepth, " cm"))
    }

    output$map <- renderLeaflet({
      req(has_points())
      m <- leaflet() %>% addTiles()
      if (!is.null(shape())) {
        m <- m %>% addPolygons(data = sf::st_transform(shape(), 4326), fillOpacity = 0.2, weight = 1.2)
      }
      add_points(m, isolate(records()))
    })

    observe({
      r <- records()
      add_points(leafletProxy("map"), r)
    })

    #################
    # 3. MEASURE    #
    #################
    output$step_measure <- renderUI({
      l <- lang()
      pts <- if (has_points()) points()
      has_bottom <- !is.null(pts) && upload_has(pts, "probeReachedBottom", complete = FALSE)
      has_date <- !is.null(pts) && upload_has(pts, "recordDate")
      has_surveyor <- !is.null(pts) && upload_has(pts, "surveyor")
      pos <- std$no_profile$positioning
      pos_choices <- stats::setNames(vapply(pos, `[[`, "", "id"),
                                     vapply(pos, function(p) p$label[[l]], ""))
      d <- val("recordDate", c(NA, NA))

      tagList(
        h3(tr("Om målingene")),
        p(tr("Svarene gjelder alle punktene. Du kan rette enkeltpunkter i tabellen i steg 2.")),
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
                 std_input(std, "peatDepthResolution", ns, l,
                           val("peatDepthResolution", std_default(std, "peatDepthResolution")),
                           required = TRUE)
          ),
          column(6,
                 if (has_bottom) {
                   tagList(tags$label(std_label(std, "probeReachedBottom", l)),
                           helpText(tr("Filen din har allerede en kolonne probeReachedBottom. Verdiene brukes; tomme eller ukjente verdier blir «No».")))
                 } else {
                   tagList(
                     radioButtons(ns("bottom"), paste(std_label(std, "probeReachedBottom", l), "*"),
                                  choiceNames = list(tr("Ja, i alle punktene"),
                                                     tr("Nei, eller usikker, i noen punkter"),
                                                     tr("Usikker for alle punktene")),
                                  choiceValues = list("all", "some", "unsure"),
                                  selected = val("bottom", character(0)), width = "100%"),
                     std_help_text(std, "probeReachedBottom", l),
                     conditionalPanel(sprintf("input['%s'] == 'some'", ns("bottom")),
                                      div(class = "alert alert-info",
                                          tr("Alle punkter får «Yes». Gå tilbake til steg 2 og merk punktene der dere ikke nådde bunnen."),
                                          br(),
                                          actionLink(ns("goto_review"), tr("Gå til tabellen"))))
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

    observeEvent(input$goto_review, step("review"))


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
      s <- summarise_issues(issues())
      n_err <- sum(s$level == "error")
      tagList(
        h3(tr("Kontroll")),
        if (n_err == 0) {
          div(class = "alert alert-success",
              sprintf(tr("Alle %s punkter følger standarden. Du kan gå videre til nedlasting."), nrow(records())))
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
      new <- isolate(list(methods = draft_methods(answers(), records(), std, l),
                          bibliographicCitation = draft_citation(meta_answers(), records())))
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
      DT::datatable(records(), rownames = FALSE, selection = "none",
                    options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
    })

    #################
    # 6. DOWNLOAD   #
    #################
    output$step_download <- renderUI({
      req(has_points())
      tagList(
        h3(tr("Last ned")),
        p(tr("Zip-filen inneholder:")),
        tags$ul(
          tags$li(tags$code("peat_data.csv"), " – ",
                  sprintf(tr("%s punkter, én rad per måling"), nrow(records()))),
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
        if (any(issues()$level == "error")) stop("The data do not pass validation.")
        dir <- tempfile("contribute_")
        on.exit(unlink(dir, recursive = TRUE), add = TRUE)
        paths <- write_standard_package(records(), metadata(), std, dir)
        utils::zip(zipfile = file, files = paths, flags = "-j9X")
      },
      contentType = "application/zip"
    )

    list(invite = invite)
  })
}
