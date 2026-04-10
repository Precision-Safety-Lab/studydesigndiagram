# # Plotting function

gen_long_diag <- function(plt, indices, txt_size = 14, addpad = 0) {
  plt_colors <- c("#E69F00", "#56B4E9", "#009E73",
                  "#F0E442", "#0072B2", "#D55E00",
                  "#CC79A7", "#999999")

  plt$color <- plt_colors[plt$color]

  # X coordinates for interval boxes
  plt$minx <- plt$start
  plt$minx[plt$minx == -Inf] <- min(plt$minx[is.finite(plt$minx)])

  plt$maxx <- plt$end
  plt$maxx[plt$maxx == Inf] <- max(plt$maxx[is.finite(plt$maxx)], 365)

  # Zero-length intervals: give them a small visible width
  lwdth <- 0.8
  zero_len <- plt$end - plt$start == 0
  plt$minx[zero_len] <- plt$start[zero_len] - lwdth
  plt$maxx[zero_len] <- plt$start[zero_len] + lwdth

  # Intervals touching time zero: extend slightly past the axis line
  plt$maxx[plt$end == 0]   <-  lwdth
  plt$minx[plt$start == 0] <- -lwdth

  # Label x position: center of box
  plt$lblx <- plt$minx + abs(plt$maxx - plt$minx) / 2

  # Interval labels
  plt$start_lbl[plt$start_lbl == ""] <- as.character(plt$start[plt$start_lbl == ""])
  plt$end_lbl[plt$end_lbl == ""]     <- as.character(plt$end[plt$end_lbl == ""])
  plt$lbl2[plt$lbl2 != ""] <- paste0("\n", plt$lbl2[plt$lbl2 != ""])
  plt$lbl <- paste0(plt$lbl, plt$lbl2, "\n", "Days [", plt$start_lbl, ";", plt$end_lbl, "]")

  # Index date labels
  indices$indexlabel2[indices$indexlabel2 != ""] <- paste0("\n", indices$indexlabel2[indices$indexlabel2 != ""])
  indices$indexlabel <- paste0(indices$indexlabel, indices$indexlabel2, "\nDay ", indices$vlines)

  # Split into inside-box and outside-box label sets
  plt_big <- plt[plt$outside_box == 0, ]
  plt_sml <- plt[plt$outside_box > 0,  ]

  # Nudge outside labels away from box edge (minimum 2 units to stay visible)
  x_range  <- max(plt$maxx) - min(plt$minx)
  abs_nudge <- max(floor(x_range / 100), 2)

  plt_sml_l <- plt_sml[plt_sml$outside_box == 1, ]  # label to the left
  plt_sml_r <- plt_sml[plt_sml$outside_box == 2, ]  # label to the right

  plt_sml_l$lbl_x <- plt_sml_l$start - abs_nudge
  plt_sml_r$lbl_x <- plt_sml_r$end   + abs_nudge

  lbl_size     <- (txt_size / 14) * 3.5
  idx_lbl_size <- (txt_size / (14 / 5)) + 0.25

  ggplot(plt, aes(x    = lblx,
                  y    = line - 0.45,
                  xmin = minx,
                  xmax = maxx,
                  ymin = line - 0.9,
                  ymax = line,
                  label = lbl)) +
    # Vertical lines / index dates
    geom_rect(data = indices,
              aes(xmin = vlines - 0.8,
                  xmax = vlines + 0.8,
                  ymin = 0,
                  ymax = max(plt$line)),
              fill = "grey60",
              inherit.aes = FALSE) +
    # Interval boxes
    geom_rect(alpha = 0.9, fill = plt$color) +
    # Text inside boxes
    geom_text(data     = plt_big,
              color    = "white",
              size     = lbl_size,
              fontface = "bold",
              hjust    = 0.5,
              vjust    = 0.5) +
    # Text outside boxes — left
    geom_text(data     = plt_sml_l,
              aes(x = lbl_x, y = line - 0.45, label = lbl),
              hjust    = 1,
              color    = plt_sml_l$color,
              size     = lbl_size,
              fontface = "bold",
              inherit.aes = FALSE) +
    # Text outside boxes — right
    geom_text(data     = plt_sml_r,
              aes(x = lbl_x, y = line - 0.45, label = lbl),
              hjust    = 0,
              color    = plt_sml_r$color,
              size     = lbl_size,
              fontface = "bold",
              inherit.aes = FALSE) +
    # Index date labels at top
    geom_label(data    = indices,
               aes(x     = vlines,
                   y     = max(plt$line) + 0.1,
                   label = indexlabel),
               vjust         = "bottom",
               hjust         = ifelse(max(plt$end) <= 0, "right", "center"),
               fontface      = "bold",
               size          = idx_lbl_size,
               color         = "grey60",
               label.r       = unit(0, "lines"),
               label.size    = 0,
               label.padding = unit(1, "lines"),
               inherit.aes   = FALSE) +
    xlab("Time (days)") +
    scale_x_continuous(expand = expansion(add = addpad)) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, max(plt$line) + ifelse(any(grepl("\\n", indices$indexlabel)), 1, 0.5))) +
    coord_cartesian(clip = "off") +
    theme_classic() +
    theme(legend.position  = "none",
          axis.line.x      = element_line(arrow = arrow(length = unit(8, "pt"), type = "closed")),
          axis.ticks.x     = element_blank(),
          axis.text.x      = element_text(face = "bold", size = txt_size),
          axis.title.x     = element_text(face = "bold", size = txt_size),
          axis.line.y      = element_blank(),
          axis.text.y      = element_blank(),
          axis.ticks.y     = element_blank(),
          axis.title.y     = element_blank())
}

# gen_long_diag <- function(plt, indices, txt_size = 14, addpad = 0) {
#   plt_colors <- c("#E69F00", "#56B4E9", "#009E73",
#                   "#F0E442", "#0072B2", "#D55E00",
#                   "#CC79A7", "#999999")

#   plt$color <- plt_colors[plt$color]

#   plt$minx <- plt$start
#   plt$minx[plt$minx == -Inf] <- min(plt$minx[plt$minx != -Inf])

#   plt$maxx <- plt$end
#   plt$maxx[plt$maxx == Inf] <- max(plt$maxx[plt$maxx != Inf], 365)

#   lwdth <- 0.8
#   plt$minx[plt$end - plt$start == 0] <- plt$start[plt$end - plt$start == 0] - lwdth
#   plt$maxx[plt$end - plt$start == 0] <- plt$start[plt$end - plt$start == 0] + lwdth
#   plt$maxx[plt$end == 0] <- lwdth
#   plt$minx[plt$start == 0] <- -lwdth

#   plt$lblx <- plt$maxx - abs(plt$maxx - plt$minx) / 2

#   plt$start_lbl[plt$start_lbl == ""] <- plt$start[plt$start_lbl == ""]
#   plt$end_lbl[plt$end_lbl == ""] <- plt$end[plt$end_lbl == ""]
#   plt$lbl2[plt$lbl2 != ""] <- paste0("\n", plt$lbl2[plt$lbl2 != ""])
#   plt$lbl <- paste0(plt$lbl, plt$lbl2, "\n", "Days [", plt$start_lbl, ";", plt$end_lbl, "]")

#   indices$indexlabel2[indices$indexlabel2 != ""] <- paste0("\n", indices$indexlabel2[indices$indexlabel2 != ""])
#   indices$indexlabel <- paste0(indices$indexlabel, indices$indexlabel2, "\nDay ", indices$vlines)

#   plt_big <- plt[plt$outside_box == 0, ]
#   plt_sml <- plt[plt$outside_box > 0, ]

#   abs_nudge <- floor((max(plt$maxx) - min(plt$minx)) / 100)

#   max_neg <- max(abs(plt[plt$start < 0, ]$end - plt[plt$start < 0, ]$start))
#   max_pos <- max(abs(plt[plt$start >= 0, ]$end - plt[plt$start >= 0, ]$start))

#   # For outside labels, compute a nudged x position and hjust directly
#   plt_sml$lbl_x <- ifelse(
#     plt_sml$outside_box == 1,
#     plt_sml$start - abs_nudge,   # label sits to the left, right-aligned
#     plt_sml$end + abs_nudge      # label sits to the right, left-aligned
#   )
#   plt_sml$lbl_hjust <- ifelse(plt_sml$outside_box == 1, 1, 0)

#   lbl_size <- (txt_size / 14) * 3.5  # convert pt-like size to geom_text size units

#   ggplot(plt, aes(x = lblx,
#                   y = line - 0.45,
#                   xmin = minx,
#                   xmax = maxx,
#                   ymin = line - 0.9,
#                   ymax = line,
#                   label = lbl)) +
#     # Vertical lines / index dates
#     geom_rect(data = indices,
#               mapping = aes(xmin = vlines - 0.8,
#                             xmax = vlines + 0.8,
#                             ymin = 0,
#                             ymax = max(plt$line)),
#               fill = "grey60",
#               inherit.aes = FALSE) +
#     # Interval boxes
#     geom_rect(alpha = 0.9, fill = plt$color) +
#     # Text inside boxes (plt_big)
#     geom_text(data = plt_big,
#               color = "white",
#               size = lbl_size,
#               fontface = "bold",
#               hjust = 0.5,
#               vjust = 0.5) +
#     # Text outside boxes — left (outside_box == 1)
#     geom_text(data = plt_sml[plt_sml$outside_box == 1, ],
#               aes(x = lbl_x, y = line - 0.45),
#               hjust = 1,
#               color = plt_sml[plt_sml$outside_box == 1, ]$color,
#               size = lbl_size,
#               fontface = "bold",
#               inherit.aes = FALSE) +
#     # Text outside boxes — right (outside_box == 2)
#     geom_text(data = plt_sml[plt_sml$outside_box == 2, ],
#               aes(x = lbl_x, y = line - 0.45, label = lbl),
#               hjust = 0,
#               color = plt_sml[plt_sml$outside_box == 2, ]$color,
#               size = lbl_size,
#               fontface = "bold",
#               inherit.aes = FALSE) +
#     # Index date labels at top
#     geom_label(data = indices,
#                mapping = aes(x = vlines,
#                              y = max(plt$line) + 0.1,
#                              label = indexlabel),
#                vjust = "bottom",
#                hjust = ifelse(max(plt$end) <= 0, "right", "center"),
#                fontface = "bold",
#                size = (txt_size / (14 / 5)) + 0.25,
#                color = "grey60",
#                label.r = unit(0, "lines"),
#                label.size = 0,
#                label.padding = unit(1, "lines"),
#                inherit.aes = FALSE) +
#     xlab("Time (days)") +
#     scale_x_continuous(expand = expansion(add = addpad)) +
#     scale_y_continuous(expand = c(0, 0),
#                        limits = c(0, max(plt$line) + ifelse(any(grepl("\\n", indices$indexlabel)), 1, 0.5))) +
#     coord_cartesian(clip = "off") +   # allow outside-box labels to render beyond plot area
#     theme_classic() +
#     theme(legend.position = "none",
#           axis.line.x = element_line(arrow = arrow(length = unit(0.6, "npc"), type = "closed")),
#           axis.ticks.x = element_blank(),
#           axis.text.x = element_text(face = "bold", size = txt_size),
#           axis.title.x = element_text(face = "bold", size = txt_size),
#           axis.line.y = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank(),
#           axis.title.y = element_blank())
# }
