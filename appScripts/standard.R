## standard.R ##
# Reads the Data Exchange Standard for Peat Surveys (corrected profile, YAML)
# plus the Norwegian overlay, and checks data against it.
# Used by the "Contribute data" module (contribute.R), but has no Shiny
# dependencies except the form-input helpers at the bottom.

app_dir <- getOption("carbonviewer.app_dir", "/home/rstudio/app")

# Base R has %||% from 4.4.0 (purrr/rlang also export it)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

#####################
# LOAD THE STANDARD #
#####################

#' Load the standard and the Norwegian profile overlay
#'
#' @param dir folder holding the YAML files.
#' @return The parsed base standard (a list) with the overlay in `$no_profile`.
load_standard <- function(dir = file.path(app_dir, "standard"),
                          base = "peat_standard_v1.0.yaml",
                          profile = "peat_standard_no_profile.yaml") {
  std <- yaml::read_yaml(file.path(dir, base))
  prof <- yaml::read_yaml(file.path(dir, profile))
  std$no_profile <- prof

  # Every value the overlay offers must be allowed by the base standard,
  # and each choice needs its own key (id or value) for the form
  for (name in names(prof$choices)) {
    allowed <- std_field(std, name)$allowed_values
    offered <- vapply(prof$choices[[name]], `[[`, "", "value")
    if (!is.null(allowed) && !all(offered %in% allowed)) {
      stop("Profile offers values not allowed for '", name, "': ",
           paste(setdiff(offered, allowed), collapse = ", "))
    }
    if (anyDuplicated(std_choice_keys(prof$choices[[name]]))) {
      stop("Profile choices for '", name, "' need distinct ids")
    }
  }
  std
}

# Attribute (peat_data) or term (metadata) definitions, in the standard's order
std_defs <- function(std, part = c("peat_data", "metadata")) {
  part <- match.arg(part)
  defs <- if (part == "peat_data") std$peat_data$attributes else std$metadata$terms
  stats::setNames(defs, vapply(defs, `[[`, "", "name"))
}

# Names in the standard's order, without the ones the profile hides
std_names <- function(std, part = c("peat_data", "metadata"), include_hidden = FALSE) {
  part <- match.arg(part)
  nms <- names(std_defs(std, part))
  if (!include_hidden) nms <- setdiff(nms, unlist(std$no_profile$hidden[[part]]))
  nms
}

# One definition, looked up in both parts
std_field <- function(std, name) {
  std_defs(std, "peat_data")[[name]] %||% std_defs(std, "metadata")[[name]]
}

std_rules <- function(std, part = c("peat_data", "metadata")) {
  part <- match.arg(part)
  if (part == "peat_data") std$peat_data$rules else std$metadata$rules
}

# Label and help text from the overlay, falling back to the field name / description
std_label <- function(std, name, lang = "no") {
  lab <- std$no_profile$fields[[name]]$label
  lab[[lang]] %||% lab[["en"]] %||% name
}

std_help <- function(std, name, lang = "no") {
  h <- std$no_profile$fields[[name]]$help
  h[[lang]] %||% h[["en"]] %||% std_field(std, name)$description %||% ""
}

# Form key of each profile choice: its id, or else its value
std_choice_keys <- function(choices) {
  vapply(choices, function(x) x$id %||% x$value, "")
}

# Named vector (label = key) for select/radio inputs
std_choices <- function(std, name, lang = "no") {
  ch <- std$no_profile$choices[[name]]
  if (is.null(ch)) {
    vals <- unlist(std_field(std, name)$allowed_values)
    return(stats::setNames(vals, vals))
  }
  stats::setNames(std_choice_keys(ch),
                  vapply(ch, function(x) x$label[[lang]] %||% x$value, ""))
}

# The profile choice behind a form key (list with value, label and maybe rights)
std_choice <- function(std, name, key) {
  ch <- std$no_profile$choices[[name]]
  hit <- match(key, std_choice_keys(ch))
  if (is.na(hit)) NULL else ch[[hit]]
}

# Description of a peat data column, for table headings
std_column_help <- function(std, name, lang = "no") {
  h <- std$no_profile$columns[[name]]
  h[[lang]] %||% h[["en"]] %||% std_field(std, name)$description %||% ""
}

std_default <- function(std, name, part = "peat_data") {
  std$no_profile$defaults[[part]][[name]]
}

std_versions <- function(std) {
  list(standard = std$standard$version,
       profile = std$profile$profile_version,
       no_profile = std$no_profile$profile$profile_version)
}

##############
# VALIDATION #
##############

is_blank <- function(x) {
  if (is.null(x)) return(logical(0))
  is.na(x) | trimws(as.character(x)) == ""
}

# ISO 8601 date, optional time, or an interval "start/end"
is_iso_date <- function(x) {
  one <- "\\d{4}-\\d{2}-\\d{2}(T\\d{2}:\\d{2}(:\\d{2}(\\.\\d+)?)?(Z|[+-]\\d{2}:?\\d{2})?)?"
  ok <- grepl(paste0("^", one, "(/", one, ")?$"), x)
  parts <- strsplit(ifelse(ok, x, ""), "/", fixed = TRUE)
  ok & vapply(parts, function(p) {
    length(p) > 0 && all(!is.na(as.Date(substr(p, 1, 10), format = "%Y-%m-%d")))
  }, TRUE)
}

.std_msg <- list(
  missing_column = c(en = "Required column '%s' is missing.",
                     no = "Den påkrevde kolonnen «%s» mangler."),
  required = c(en = "'%s' is required but empty.",
               no = "«%s» er påkrevd, men er tom."),
  type_number = c(en = "'%s' must be a number.",
                  no = "«%s» må være et tall."),
  type_date = c(en = "'%s' must be a date as YYYY-MM-DD (or a range YYYY-MM-DD/YYYY-MM-DD).",
                no = "«%s» må være en dato som ÅÅÅÅ-MM-DD (eller en periode ÅÅÅÅ-MM-DD/ÅÅÅÅ-MM-DD)."),
  enum = c(en = "'%s' must be one of: %s.",
           no = "«%s» må være en av: %s."),
  at_least_one_group_complete = c(en = "A location is needed: give %s.",
                                  no = "Posisjon mangler: oppgi %s."),
  required_if_present = c(en = "'%s' is required when %s is given.",
                          no = "«%s» er påkrevd når %s er oppgitt."),
  default_if_absent = c(en = "'%s' is empty, so %s will be assumed for %s.",
                        no = "«%s» er tom, så %s blir antatt for %s."),
  desirable_if_present = c(en = "'%s' is recommended when %s is given.",
                           no = "«%s» anbefales når %s er oppgitt."),
  at_least_one_present = c(en = "At least one of %s must be given.",
                           no = "Minst én av %s må oppgis.")
)

std_msg <- function(key, lang, ...) {
  tmpl <- .std_msg[[key]][[lang]] %||% .std_msg[[key]][["en"]]
  sprintf(tmpl, ...)
}

#' Check a data frame against the standard
#'
#' Implements requirement levels, types and enums from the attribute/term
#' definitions, plus the rule types `at_least_one_group_complete`,
#' `required_if_present`, `default_if_absent`, `desirable_if_present` and
#' `at_least_one_present`. Other rule types (priority, crs, note) are
#' informative and not checked. Missing columns count as empty.
#'
#' @param data data frame of records (peat data) or the one-row metadata table.
#' @param std standard from `load_standard()`.
#' @param part "peat_data" or "metadata".
#' @param lang language of the messages ("no" or "en").
#' @return data frame with one row per issue: level ("error"/"warning"),
#'   field, rule, row (NA for column-level issues) and message.
validate_against_standard <- function(data, std, part = c("peat_data", "metadata"),
                                      lang = "en") {
  part <- match.arg(part)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  n <- nrow(data)
  defs <- std_defs(std, part)
  issues <- list()

  add <- function(level, field, rule, rows, message) {
    if (length(rows) == 0) return(invisible())
    issues[[length(issues) + 1]] <<- data.frame(
      level = level, field = field, rule = rule, row = rows, message = message,
      stringsAsFactors = FALSE)
  }
  col <- function(name) if (name %in% names(data)) data[[name]] else rep(NA, n)
  present <- function(name) !is_blank(col(name))
  quoted <- function(x) paste0("'", x, "'")
  and_list <- function(x) paste(quoted(x), collapse = " + ")

  # Requirement levels, types and allowed values
  for (def in defs) {
    name <- def$name
    if (identical(def$requirement, "required")) {
      if (!name %in% names(data)) {
        add("error", name, "required", NA_integer_, std_msg("missing_column", lang, name))
        next
      }
      add("error", name, "required", which(!present(name)), std_msg("required", lang, name))
    }
    if (!name %in% names(data)) next
    x <- as.character(data[[name]])
    filled <- !is_blank(x)
    bad <- switch(def$type %||% "string",
      number = filled & is.na(suppressWarnings(as.numeric(x))),
      date = filled & !is_iso_date(x),
      enum = filled & !x %in% unlist(def$allowed_values),
      rep(FALSE, n))
    if (any(bad)) {
      msg <- switch(def$type,
        number = std_msg("type_number", lang, name),
        date = std_msg("type_date", lang, name),
        enum = std_msg("enum", lang, name, paste(unlist(def$allowed_values), collapse = ", ")))
      add("error", name, paste0("type_", def$type), which(bad), msg)
    }
  }

  # Cross-field rules
  for (rule in std_rules(std, part)) {
    switch(rule$type,
      at_least_one_group_complete = {
        ok <- Reduce(`|`, lapply(rule$groups, function(g) {
          Reduce(`&`, lapply(unlist(g), present))
        }), rep(FALSE, n))
        groups <- vapply(rule$groups, function(g) and_list(unlist(g)), "")
        add("error", paste(unlist(rule$groups), collapse = ","), rule$id, which(!ok),
            std_msg("at_least_one_group_complete", lang,
                    paste(groups, collapse = if (lang == "no") " eller " else " or ")))
      },
      required_if_present = {
        trig <- Reduce(`|`, lapply(unlist(rule$if_present), present))
        add("error", rule$field, rule$id, which(trig & !present(rule$field)),
            std_msg("required_if_present", lang, rule$field, and_list(unlist(rule$if_present))))
      },
      desirable_if_present = {
        trig <- Reduce(`|`, lapply(unlist(rule$if_present), present))
        add("warning", rule$field, rule$id, which(trig & !present(rule$field)),
            std_msg("desirable_if_present", lang, rule$field, and_list(unlist(rule$if_present))))
      },
      default_if_absent = {
        trig <- Reduce(`|`, lapply(unlist(rule$if_present), present))
        add("warning", rule$field, rule$id, which(trig & !present(rule$field)),
            std_msg("default_if_absent", lang, rule$field, rule$default,
                    and_list(unlist(rule$if_present))))
      },
      at_least_one_present = {
        ok <- Reduce(`|`, lapply(unlist(rule$fields), present))
        add("error", paste(unlist(rule$fields), collapse = ","), rule$id, which(!ok),
            std_msg("at_least_one_present", lang, and_list(unlist(rule$fields))))
      },
      NULL)
  }

  if (length(issues) == 0) {
    return(data.frame(level = character(), field = character(), rule = character(),
                      row = integer(), message = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, issues)
}

#' Collapse row-level issues into one line per message
#'
#' @return data frame with level, field, message, n (number of rows) and rows
#'   (the first few row numbers as text), errors first.
summarise_issues <- function(issues, max_rows = 10) {
  if (nrow(issues) == 0) {
    return(data.frame(level = character(), field = character(), message = character(),
                      n = integer(), rows = character(), stringsAsFactors = FALSE))
  }
  key <- paste(issues$level, issues$field, issues$message, sep = "\r")
  out <- do.call(rbind, lapply(split(issues, factor(key, levels = unique(key))), function(d) {
    rows <- d$row[!is.na(d$row)]
    data.frame(level = d$level[1], field = d$field[1], message = d$message[1],
               n = length(rows),
               rows = if (length(rows) == 0) "" else paste0(
                 paste(utils::head(rows, max_rows), collapse = ", "),
                 if (length(rows) > max_rows) ", ..." else ""),
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out[order(out$level != "error"), ]
}

##########################################
# FORM INPUTS BUILT FROM THE SCHEMA      #
##########################################

# Help text shown under an input
std_help_text <- function(std, name, lang) {
  shiny::helpText(std_help(std, name, lang))
}

#' Build a Shiny input for a field from its type, label, help and choices
#'
#' @param ns namespace function of the calling module.
#' @param value current value (pass the isolated input value to keep it on re-render).
#' @param multiline use a text area for strings.
std_input <- function(std, name, ns, lang = "no", value = NULL, multiline = FALSE,
                      required = NULL) {
  def <- std_field(std, name)
  required <- required %||% identical(def$requirement, "required")
  label <- std_label(std, name, lang)
  if (required) label <- paste(label, "*")
  id <- ns(name)
  has_choices <- !is.null(std$no_profile$choices[[name]]) ||
    identical(def$type, "enum")

  input <- if (has_choices) {
    ch <- std_choices(std, name, lang)
    if (!required) ch <- c(stats::setNames("", if (lang == "no") "(ikke oppgitt)" else "(not given)"), ch)
    shiny::selectInput(id, label, choices = ch, selected = value %||% ch[1], width = "100%")
  } else if (identical(def$type, "number")) {
    shiny::numericInput(id, label, value = value %||% NA, width = "100%")
  } else if (multiline) {
    shiny::textAreaInput(id, label, value = value %||% "", width = "100%", rows = 3)
  } else {
    shiny::textInput(id, label, value = value %||% "", width = "100%")
  }
  shiny::tagList(input, std_help_text(std, name, lang))
}
