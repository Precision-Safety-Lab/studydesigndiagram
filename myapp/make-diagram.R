gen_long_diag <- function(
  plt,
  indices,
  txt_size = 14,
  addpad = 0,
  xlim_min,
  xlim_max,
  hide_x_labels = FALSE,
  hide_index_dates = FALSE
) {
  plt_colors <- c(
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

  # Backward compatible color handling:
  # - old app: numeric values 1:8
  # - new app: hex values from selectInput()
  if (is.numeric(plt$color)) {
    plt$color <- plt_colors[plt$color]
  } else {
    color_chr <- as.character(plt$color)

    numeric_like <- grepl("^[0-9]+$", color_chr)

    color_chr[numeric_like] <- vapply(
      as.integer(color_chr[numeric_like]),
      function(i) {
        if (!is.na(i) && i >= 1 && i <= length(plt_colors)) {
          plt_colors[[i]]
        } else {
          plt_colors[[1]]
        }
      },
      character(1)
    )

    valid_hex <- grepl("^#[0-9A-Fa-f]{6}$", color_chr)

    # If something unexpected is present, fall back to blue.
    color_chr[!valid_hex] <- plt_colors[[1]]

    plt$color <- color_chr
  }
  # Backward compatibility if columns are absent
  if (is.null(plt$end_inf)) plt$end_inf <- FALSE
  if (is.null(plt$days_lbl)) plt$days_lbl <- ""

  plt$end_inf[is.na(plt$end_inf)] <- FALSE
  plt$days_lbl[is.na(plt$days_lbl)] <- ""

  # Label indefinite ends before the generic lbl-filling below overwrites them
  plt$end_lbl[plt$end_inf & plt$end_lbl == ""] <- "\u221e"  # ∞
  plt$end[plt$end_inf] <- Inf

  # X coordinates for interval boxes
  plt$minx <- plt$start
  plt$minx[!is.finite(plt$minx)] <- min(plt$minx[is.finite(plt$minx)])

  plt$maxx <- plt$end
  x_max_finite <- max(plt$maxx[is.finite(plt$maxx)])
  x_min_finite <- min(plt$minx[is.finite(plt$minx)])

  # Compute axis limits first, so infinite boxes can be clipped exactly to them
  x_left  <- if (!is.na(xlim_min)) xlim_min else x_min_finite - addpad
  x_right <- if (!is.na(xlim_max)) xlim_max else x_max_finite + addpad

  # Infinite boxes extend exactly to the right axis edge
  plt$maxx[plt$end_inf] <- x_right

  # Zero-length intervals: give them a small visible width
  lwdth <- 0.8
  zero_len <- is.finite(plt$end) & (plt$end - plt$start == 0)
  plt$minx[zero_len] <- plt$start[zero_len] - lwdth
  plt$maxx[zero_len] <- plt$start[zero_len] + lwdth

  # Intervals touching time zero: extend slightly past the axis line
  plt$maxx[is.finite(plt$end)   & plt$end   == 0] <-  lwdth
  plt$minx[is.finite(plt$start) & plt$start == 0] <- -lwdth

  # Label x position: center of box
  plt$lblx <- plt$minx + abs(plt$maxx - plt$minx) / 2

  # Interval labels
  plt$start_lbl[plt$start_lbl == ""] <- as.character(plt$start[plt$start_lbl == ""])
  plt$end_lbl[plt$end_lbl == ""]     <- as.character(plt$end[plt$end_lbl == ""])

  default_days_lbl <- paste0("Days [", plt$start_lbl, ";", plt$end_lbl, "]")

  final_days_lbl <- ifelse(
    nzchar(trimws(plt$days_lbl)),
    plt$days_lbl,
    default_days_lbl
  )

  plt$lbl2[plt$lbl2 != ""] <- paste0("\n", plt$lbl2[plt$lbl2 != ""])
  plt$lbl <- paste0(plt$lbl, plt$lbl2, "\n", final_days_lbl)

  # Index date labels
  # Index date labels
  indices$indexlabel2[indices$indexlabel2 != ""] <- paste0(
    "\n",
    indices$indexlabel2[indices$indexlabel2 != ""]
  )

  if (isTRUE(hide_index_dates)) {
    indices$indexlabel <- paste0(indices$indexlabel, indices$indexlabel2)
  } else {
    indices$indexlabel <- paste0(
      indices$indexlabel,
      indices$indexlabel2,
      "\nDay ",
      indices$vlines
    )
  }

  # Split into inside-box and outside-box label sets
  plt_big <- plt[plt$outside_box == 0, ]
  plt_sml <- plt[plt$outside_box > 0,  ]

  x_range   <- max(plt$maxx) - min(plt$minx)
  abs_nudge <- max(floor(x_range / 100), 2)

  plt_sml_l <- plt_sml[plt_sml$outside_box == 1, ]
  plt_sml_r <- plt_sml[plt_sml$outside_box == 2, ]

  plt_sml_l$lbl_x <- plt_sml_l$start - abs_nudge
  plt_sml_r$lbl_x <- plt_sml_r$end   + abs_nudge

  lbl_size     <-  (txt_size / ggplot2::.pt)
  idx_lbl_size <- (txt_size / (14 / 5)) + 0.25

  # --- Parameters -----------------------------------------------------------

  x_range <- x_right - x_left

  # tuning constants
  char_frac    <- 0.01
  stagger_step <- idx_lbl_size * 0.08
  base_y       <- max(plt$line) + 0.1

  # --- Preprocessing --------------------------------------------------------

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
    # Vertical lines / index dates
    geom_rect(
      data = indices,
      aes(
        xmin = vlines - 0.8,
        xmax = vlines + 0.8,
        ymin = 0,
        ymax = max(plt$line)
      ),
      fill = "grey60",
      inherit.aes = FALSE
    ) +
    # Interval boxes
    geom_rect(alpha = 0.9, fill = plt$color) +
    # Text inside boxes
    ggfittext::geom_fit_text(
      data     = plt_big,
      color    = "white",
      size     = txt_size,
      fontface = "bold",
      reflow   = TRUE,
      grow     = FALSE
    ) +
    # Text outside boxes — left
    geom_text(
      data  = plt_sml_l,
      aes(x = lbl_x, y = line - 0.45, label = lbl),
      hjust       = 1,
      color       = plt_sml_l$color,
      size        = lbl_size,
      fontface    = "bold",
      inherit.aes = FALSE
    ) +
    # Text outside boxes — right
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
      aes(
        x     = vlines,
        y     = lbl_y,
        label = indexlabel
      ),
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
    xlab("Time (days)") +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(x_left, x_right),
      breaks = function(x) {
        b <- scales::breaks_pretty()(x)
        b[b < x_right]
      }
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(
        0,
        max(indices$lbl_y) + ifelse(any(grepl("\n", indices$indexlabel)), 1, 0.5)
      )
    ) +
    coord_cartesian(clip = "off") +
    theme_classic() +
    theme(
      legend.position = "none",

      axis.line.x = element_line(
        arrow = arrow(length = unit(8, "pt"), type = "closed")
      ),
      axis.ticks.x = element_blank(),
      axis.text.x = if (isTRUE(hide_x_labels)) {
        element_blank()
      } else {
        element_text(face = "bold", size = txt_size)
      },
      axis.title.x = element_text(face = "bold", size = txt_size),

      axis.line.y  = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank()
    )
}