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

# --- as_num1 / as_chr1 -------------------------------------------------

stopifnot(identical(as_num1(5L), 5))
stopifnot(identical(as_num1("-30"), -30))
stopifnot(is.na(as_num1(NA)))
stopifnot(is.na(as_num1(NULL)))
stopifnot(identical(as_num1("abc", 0), 0))
stopifnot(identical(as_num1(c(1, 2), 0), 0))         # not a single value
stopifnot(identical(as_num1(list(1), 0), 0))         # JSON array/object
stopifnot(identical(as_chr1(NULL), ""))
stopifnot(identical(as_chr1(NA, "x"), "x"))
stopifnot(identical(as_chr1(3), "3"))
stopifnot(identical(as_chr1(list("a")), ""))

# --- normalize_state ---------------------------------------------------

stopifnot(inherits(try(normalize_state("abc"), silent = TRUE), "try-error"))
stopifnot(inherits(try(normalize_state(list(1, 2)), silent = TRUE), "try-error"))
stopifnot(inherits(try(normalize_state(list(foo = 1)), silent = TRUE), "try-error"))

s <- normalize_state(list(
  textsize = "abc",
  aratio = 2,
  rows = list(
    list(line = 1, lbl1 = NULL, start = NULL, start_lbl = "5", outbox = "7", boxcolor = 3),
    "not a row",
    list(line = 2, lbl1 = "Washout window", start = -30, end = 0, end_inf = TRUE)
  ),
  indices = list(list(indexlbl1 = NULL, index = "abc"))
))
stopifnot(s$textsize == 14, s$xpad == 50, is.na(s$xlim_min), s$aratio == "2")
stopifnot(length(s$rows) == 2)
stopifnot(s$rows[[1]]$lbl1 == "Washout window", s$rows[[1]]$end_inf)   # sorted by line, highest first
stopifnot(s$rows[[2]]$lbl1 == "", s$rows[[2]]$start == 0)               # `start` not read from `start_lbl`
stopifnot(s$rows[[2]]$outbox == "0", s$rows[[2]]$boxcolor == unname(box_colors[[3]]))
stopifnot(s$indices[[1]]$indexlbl1 == anchor1_choices[[1]], s$indices[[1]]$index == 0)
stopifnot(s$x_label == "Time (days)", s$indices[[1]]$indexdatelbl == "Day")   # older files: defaults
s2 <- normalize_state(list(x_label = "", indices = list(list(indexdatelbl = "Week"))))
stopifnot(s2$x_label == "", s2$indices[[1]]$indexdatelbl == "Week")            # saved values kept, even blank

cat("All helper tests passed.\n")
