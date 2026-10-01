# helpers.R
#
# Shared constants and pure helper functions used by both app.R and
# make-diagram.R. Nothing here depends on shiny or ggplot2, so it can be
# sourced and unit-tested on its own (see tests/test-helpers.R).

# A handful of R versions still lack the base `%||%` operator (only added
# to base R in 4.4.0). Define it defensively so the app doesn't rely on it
# being re-exported by one of the plotting packages.
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

# First-order anchor choices (index dates, e.g. cohort entry / event date)
anchor1_choices <- c(
  "Cohort entry date" = "Cohort entry date",
  "Event date"        = "Event date"
)

# Second-order anchor choices (interval / window labels)
anchor2_choices <- c(
  "Please choose" = "",
  "Follow-up"     = "Follow-up window",
  "Eligibility"   = "Eligibility assessment window",
  "Exclusion"     = "Exclusion assessment window",
  "Covariates"    = "Covariate assessment window",
  "Washout"       = "Washout window",
  "Exposure"      = "Exposure assessment window"
)

# Box color palette. All colors are dark enough for white text on top.
box_colors <- c(
  "Blue"     = "#0b5e92",
  "Teal"     = "#1b947a",
  "Green"    = "#1d811d",
  "Purple"   = "#64309b",
  "Plum"     = "#5f1848",
  "Orange"   = "#c46a1c",
  "Brown"    = "#8f6949",
  "Red"      = "#A23E3E",
  "Slate"    = "#4D5963"
)

#' Resolve a stored color value to a valid hex code.
#'
#' Handles saved project files from any generation of the app:
#'   - current:      a hex code straight out of `box_colors` (e.g. "#0b5e92")
#'   - legacy:        a numeric index into a fixed palette (e.g. "3")
#'   - unknown/blank: falls back to the first palette color
#'
#' @param value   a single stored color value (character or numeric)
#' @param palette named vector of hex colors, defaults to `box_colors`
#' @return a single hex color string, always present in `palette`
resolve_box_color <- function(value, palette = box_colors) {
  value <- as.character(value %||% palette[[1]])

  if (grepl("^[0-9]+$", value)) {
    idx <- suppressWarnings(as.integer(value))
    if (!is.na(idx) && idx >= 1 && idx <= length(palette)) {
      return(unname(palette[[idx]]))
    }
    return(unname(palette[[1]]))
  }

  if (value %in% unname(palette)) {
    return(value)
  }

  unname(palette[[1]])
}

#' Coerce an untrusted value to a single number / string.
#'
#' Values from Shiny inputs and saved project files can be NULL, NA, empty,
#' the wrong type or a JSON object/array. These always return exactly one
#' value of the expected type, falling back to `default`.
as_num1 <- function(x, default = NA_real_) {
  if (!is.atomic(x) || length(x) != 1) return(default)
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v)) default else v
}

as_chr1 <- function(x, default = "") {
  if (!is.atomic(x) || length(x) != 1 || is.na(x)) return(default)
  as.character(x)
}

#' Validate and clean a parsed project file.
#'
#' Takes the output of `jsonlite::fromJSON(text, simplifyVector = FALSE)` and
#' returns a fixed shape with one value of the right type per field, so that
#' restoring it can't fail halfway through. Rows come back top-to-bottom
#' (highest saved `line` first). Errors if `state` isn't a project at all.
#'
#' `[[` is used rather than `$` throughout, since `$` partially matches on
#' lists (a file missing `start` would otherwise silently read `start_lbl`).
normalize_state <- function(state) {
  if (!is.list(state) || is.null(names(state)) ||
      !any(c("rows", "indices") %in% names(state))) {
    stop("this does not look like a project file")
  }

  list_of_lists <- function(x) if (is.list(x)) Filter(is.list, x) else list()
  rows    <- list_of_lists(state[["rows"]])
  indices <- list_of_lists(state[["indices"]])

  if (length(rows) > 1) {
    line_vals <- vapply(rows, function(r) as_num1(r[["line"]]), numeric(1))
    if (!all(is.na(line_vals))) {
      rows <- rows[order(line_vals, decreasing = TRUE, na.last = TRUE)]
    }
  }

  list(
    textsize         = as_num1(state[["textsize"]], 14),
    xpad             = as_num1(state[["xpad"]], 50),
    xlim_min         = as_num1(state[["xlim_min"]]),
    xlim_max         = as_num1(state[["xlim_max"]]),
    hide_x_labels    = isTRUE(state[["hide_x_labels"]]),
    hide_index_dates = isTRUE(state[["hide_index_dates"]]),
    # Older project files lack these; a saved "" (deliberately blank) is kept
    x_label          = as_chr1(state[["x_label"]], "Time (days)"),
    aratio          = if (identical(as_chr1(state[["aratio"]]), "2")) "2" else "1",
    rows = lapply(rows, function(r) {
      outbox <- as_chr1(r[["outbox"]], "0")
      list(
        lbl1      = as_chr1(r[["lbl1"]]),
        lbl2      = as_chr1(r[["lbl2"]]),
        start     = as_num1(r[["start"]], 0),
        end       = as_num1(r[["end"]], 0),
        end_inf   = isTRUE(r[["end_inf"]]),
        start_lbl = as_chr1(r[["start_lbl"]]),
        end_lbl   = as_chr1(r[["end_lbl"]]),
        days_lbl  = as_chr1(r[["days_lbl"]]),
        outbox    = if (outbox %in% c("0", "1", "2")) outbox else "0",
        boxcolor  = resolve_box_color(as_chr1(r[["boxcolor"]]))
      )
    }),
    indices = lapply(indices, function(i) {
      list(
        indexlbl1 = as_chr1(i[["indexlbl1"]], anchor1_choices[[1]]),
        indexlbl2 = as_chr1(i[["indexlbl2"]]),
        indexdatelbl = as_chr1(i[["indexdatelbl"]], "Day"),
        index     = as_num1(i[["index"]], 0)
      )
    })
  )
}
