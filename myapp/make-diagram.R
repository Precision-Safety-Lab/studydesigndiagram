# make-diagram.R
#
# Builds the longitudinal study-design ggplot from two data frames:
#   plt     - one row per second-order anchor (interval / window box)
#   indices - one row per first-order anchor (index date / vertical line)
#
# Expects helpers.R to have been sourced first (see app.R).

gen_long_diag <- function(
  plt,
  indices,
  txt_size = 14,
  addpad = 0,
  xlim_min,
  xlim_max,
  hide_x_labels = FALSE,
  hide_index_dates = FALSE,
  x_label = "Time (days)",
  x_tick_step = NA
) {
  # --- Color handling (current hex values and legacy numeric indices) -----

  plt$color <- vapply(plt$color, resolve_box_color, character(1))

  # Backward compatibility if columns are absent from an older saved project
  if (is.null(plt$end_inf))  plt$end_inf  <- FALSE
  if (is.null(plt$days_lbl)) plt$days_lbl <- ""

  plt$end_inf[is.na(plt$end_inf)]   <- FALSE
  plt$days_lbl[is.na(plt$days_lbl)] <- ""

  # Label indefinite ends before the generic lbl-filling below overwrites them
  plt$end_lbl[plt$end_inf & plt$end_lbl == ""] <- "\u221e"  # infinity symbol
  plt$end[plt$end_inf] <- Inf

  # --- X coordinates for interval boxes ------------------------------------

  plt$minx <- plt$start
  plt$minx[!is.finite(plt$minx)] <- min(plt$minx[is.finite(plt$minx)])

  plt$maxx <- plt$end
  # Starts are included so the right edge is still defined when every window
  # is indefinite (all ends are Inf) and no explicit axis end is given
  x_max_finite <- max(plt$maxx[is.finite(plt$maxx)], plt$minx[is.finite(plt$minx)])
  x_min_finite <- min(plt$minx[is.finite(plt$minx)])

  # Axis limits, computed first so infinite boxes can be clipped exactly to them
  x_left  <- if (!is.na(xlim_min)) xlim_min else x_min_finite - addpad
  x_right <- if (!is.na(xlim_max)) xlim_max else x_max_finite + addpad
  x_range <- x_right - x_left

  # Infinite boxes extend exactly to the right axis edge
  plt$maxx[plt$end_inf] <- x_right

  # Optional fixed tick interval: ticks at its multiples, so day 0 is always
  # one (e.g. 365 gives ..., -365, 0, 365, ...). Ignored in favour of the
  # automatic ticks when blank, not positive, or so small it would crowd the
  # axis with more than 100 ticks.
  use_tick_step <- isTRUE(is.finite(x_tick_step) && x_tick_step > 0 && x_range / x_tick_step <= 100)

  # First-order anchor band half-width. Scaled to the plotted x-range so it
  # stays visible whether the diagram spans 10 days or 10 years, rather than
  # a fixed number of days.
  anchor_halfwidth <- max(x_range * 0.006, 1)

  # Zero-length intervals: drawn as wide as an anchor band, so they stay
  # visible at any x-range
  lwdth <- 0.8
  zero_len <- is.finite(plt$end) & (plt$end - plt$start == 0)

  # Intervals touching time zero: extend slightly past the axis line
  plt$maxx[!zero_len & is.finite(plt$end)   & plt$end   == 0] <-  lwdth
  plt$minx[!zero_len & is.finite(plt$start) & plt$start == 0] <- -lwdth

  plt$minx[zero_len] <- plt$start[zero_len] - anchor_halfwidth
  plt$maxx[zero_len] <- plt$start[zero_len] + anchor_halfwidth

  # Keep boxes clear of first-order anchors outside their date range: e.g.
  # [-365; -1] stops at the left edge of the band drawn for day 0, rather
  # than running into it just because the band is wider than a day. (Boxes
  # whose range does include an anchor day are widened to cover its band
  # further down.) A box squeezed below the zero-length width is kept at
  # that width, extending away from the anchor.
  min_box_width <- 2 * anchor_halfwidth

  for (a in indices$vlines) {
    band_l <- a - anchor_halfwidth
    band_r <- a + anchor_halfwidth

    before <- plt$end < a & plt$maxx > band_l
    plt$maxx[before] <- band_l
    plt$minx[before] <- pmin(plt$minx[before], band_l - min_box_width)

    after <- plt$start > a & plt$minx < band_r
    plt$minx[after] <- band_r
    plt$maxx[after] <- pmax(plt$maxx[after], band_r + min_box_width)
  }

  # Label x position: center of box
  plt$lblx <- plt$minx + abs(plt$maxx - plt$minx) / 2

  # --- Interval labels ------------------------------------------------------

  plt$start_lbl[plt$start_lbl == ""] <- as.character(plt$start[plt$start_lbl == ""])
  plt$end_lbl[plt$end_lbl == ""]     <- as.character(plt$end[plt$end_lbl == ""])

  default_days_lbl <- paste0("Days [", plt$start_lbl, ";", plt$end_lbl, "]")
  final_days_lbl <- ifelse(nzchar(trimws(plt$days_lbl)), plt$days_lbl, default_days_lbl)

  plt$lbl2[plt$lbl2 != ""] <- paste0("\n", plt$lbl2[plt$lbl2 != ""])
  plt$lbl <- paste0(plt$lbl, plt$lbl2, "\n", final_days_lbl)

  # --- Index date (first-order anchor) labels -------------------------------

  indices$indexlabel2[indices$indexlabel2 != ""] <- paste0("\n", indices$indexlabel2[indices$indexlabel2 != ""])

  # Last line: "<date label> <index date>", e.g. "Day 0"; a blank date label
  # leaves just the number
  if (is.null(indices$datelabel)) indices$datelabel <- "Day"
  date_line <- trimws(paste(trimws(indices$datelabel), indices$vlines))

  if (isTRUE(hide_index_dates)) {
    indices$indexlabel <- paste0(indices$indexlabel, indices$indexlabel2)
  } else {
    indices$indexlabel <- paste0(indices$indexlabel, indices$indexlabel2, "\n", date_line)
  }

  # --- Split into inside-box and outside-box label sets ---------------------

  plt_big <- plt[plt$outside_box == 0, ]
  plt_sml <- plt[plt$outside_box > 0,  ]

  abs_nudge <- max(floor(x_range / 100), 2)

  plt_sml_l <- plt_sml[plt_sml$outside_box == 1, ]
  plt_sml_r <- plt_sml[plt_sml$outside_box == 2, ]

  # Placed off the drawn box edges (which may have been clipped clear of an
  # anchor above); an indefinite box's right label stays pinned at the edge
  plt_sml_l$lbl_x <- plt_sml_l$minx - abs_nudge
  plt_sml_r$lbl_x <- ifelse(plt_sml_r$end_inf, Inf, plt_sml_r$maxx + abs_nudge)

  lbl_size     <- (txt_size / ggplot2::.pt)
  idx_lbl_size <- (txt_size / (14 / 5)) + 0.25

  # --- First-order anchor coverage -----------------------------------------
  #
  # Where a box's date range includes a first-order anchor day, widen its
  # drawn *fill* extent to at least the anchor's own width, so the box's color
  # completely covers the anchor there - no sliver of the anchor band left
  # poking out, and (being fully opaque, see below) no bleed-through tint
  # either. Only the fill rectangle is widened: label/text positions still
  # use the box's own original extent, so text stays centered correctly.
  # This goes by dates, not drawn extent: boxes that merely end next to an
  # anchor were clipped clear of it above and must not be widened over it.
  plt$fill_minx <- plt$minx
  plt$fill_maxx <- plt$maxx

  for (a in indices$vlines) {
    covers <- plt$start <= a & plt$end >= a
    plt$fill_minx[covers] <- pmin(plt$fill_minx[covers], a - anchor_halfwidth)
    plt$fill_maxx[covers] <- pmax(plt$fill_maxx[covers], a + anchor_halfwidth)
  }

  # --- Index label staggering (avoid overlapping labels at the top) --------

  char_frac    <- 0.01
  stagger_step <- idx_lbl_size * 0.08
  base_y       <- max(plt$line) + 0.1

  indices <- indices[order(indices$vlines), , drop = FALSE]
  n <- nrow(indices)

  if (n <= 1) {
    indices$lbl_y <- base_y
  } else {
    label_main <- sub("\n.*", "", indices$indexlabel)
    half_w <- nchar(label_main) * char_frac * x_range * txt_size / 20

    stagger <- numeric(n)
    for (i in 2:n) {
      for (j in seq_len(i - 1)) {
        overlap <- abs(indices$vlines[i] - indices$vlines[j]) < (half_w[i] + half_w[j])
        if (overlap) {
          stagger[i] <- max(stagger[i], stagger[j] + stagger_step)
        }
      }
    }
    indices$lbl_y <- base_y + stagger
  }

  # --- Plot -----------------------------------------------------------------

  ggplot(
    plt,
    aes(
      x     = lblx,
      y     = line - 0.45,
      xmin  = minx,
      xmax  = maxx,
      ymin  = line - 0.9,
      ymax  = line,
      label = lbl
    )
  ) +
    # First-order anchor bands (index dates), drawn underneath the boxes
    geom_rect(
      data = indices,
      aes(xmin = vlines - anchor_halfwidth, xmax = vlines + anchor_halfwidth, ymin = 0, ymax = max(plt$line)),
      fill = "grey60",
      inherit.aes = FALSE
    ) +
    # Interval boxes, drawn on top and fully opaque (so nothing bleeds
    # through from the anchor band below), widened via fill_minx/fill_maxx
    # where needed so the box's own color fully covers any anchor it touches
    geom_rect(
      aes(xmin = fill_minx, xmax = fill_maxx),
      fill = plt$color, alpha = 1
    ) +
    # Text inside boxes
    ggfittext::geom_fit_text(
      data     = plt_big,
      color    = "white",
      size     = txt_size,
      fontface = "bold",
      reflow   = TRUE,
      grow     = FALSE
    ) +
    # Text outside boxes - left
    geom_text(
      data  = plt_sml_l,
      aes(x = lbl_x, y = line - 0.45, label = lbl),
      hjust       = 1,
      color       = plt_sml_l$color,
      size        = lbl_size,
      fontface    = "bold",
      inherit.aes = FALSE
    ) +
    # Text outside boxes - right
    geom_text(
      data  = plt_sml_r,
      aes(x = lbl_x, y = line - 0.45, label = lbl),
      hjust       = 0,
      color       = plt_sml_r$color,
      size        = lbl_size,
      fontface    = "bold",
      inherit.aes = FALSE
    ) +
    # Index date labels at top
    geom_label(
      data = indices,
      aes(x = vlines, y = lbl_y, label = indexlabel),
      vjust         = "bottom",
      hjust         = ifelse(max(plt$end, na.rm = TRUE) <= 0, "right", "center"),
      fontface      = "bold",
      size          = idx_lbl_size,
      color         = "grey60",
      label.r       = unit(0, "lines"),
      linewidth     = 0,
      label.padding = unit(0.25, "lines"),
      inherit.aes   = FALSE
    ) +
    # A blank label removes the axis title (and its space) entirely
    xlab(if (nzchar(trimws(x_label))) x_label else NULL) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(x_left, x_right),
      breaks = function(x) {
        b <- if (use_tick_step) {
          from <- ceiling(x_left / x_tick_step)
          to   <- floor(x_right / x_tick_step)
          if (from > to) numeric(0) else round(seq(from, to) * x_tick_step, 10)
        } else {
          scales::breaks_pretty()(x)
        }
        b[b < x_right]
      }
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, max(indices$lbl_y) + ifelse(any(grepl("\n", indices$indexlabel)), 1, 0.5))
    ) +
    coord_cartesian(clip = "off") +
    theme_classic() +
    theme(
      legend.position = "none",

      axis.line.x = element_line(arrow = arrow(length = unit(8, "pt"), type = "closed")),
      axis.ticks.x = element_blank(),
      axis.text.x = if (isTRUE(hide_x_labels)) element_blank() else element_text(face = "bold", size = txt_size),
      axis.title.x = element_text(face = "bold", size = txt_size),

      axis.line.y  = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank()
    )
}
