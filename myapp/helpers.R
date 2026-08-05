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
