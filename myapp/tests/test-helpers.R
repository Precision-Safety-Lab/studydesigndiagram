# tests/test-helpers.R
#
# Minimal smoke tests for the pure functions in helpers.R. These deliberately
# avoid testthat/shiny/ggplot2 so they can run with plain `Rscript` and no
# extra installed packages.
#
# Run with:  Rscript tests/test-helpers.R

this_file <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) sub("^--file=", "", file_arg[1]) else NULL
}

script_dir <- {
  f <- this_file()
  if (!is.null(f)) dirname(normalizePath(f)) else getwd()
}

source(file.path(dirname(script_dir), "helpers.R"))

# --- resolve_box_color -------------------------------------------------

stopifnot(resolve_box_color("#1b947a") == "#1b947a")          # already valid hex
stopifnot(resolve_box_color("3") == unname(box_colors[[3]]))   # legacy numeric index
stopifnot(resolve_box_color(3) == unname(box_colors[[3]]))      # legacy numeric (not character)
stopifnot(resolve_box_color("not-a-color") == unname(box_colors[[1]])) # unknown -> fallback
stopifnot(resolve_box_color(NULL) == unname(box_colors[[1]]))   # missing -> fallback
stopifnot(resolve_box_color("999") == unname(box_colors[[1]]))  # out-of-range index -> fallback

cat("All helper tests passed.\n")
