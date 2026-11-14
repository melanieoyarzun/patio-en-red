# =============================================================================
# 00_style.R
#
# Colors and a plain ggplot theme shared by all figures, so that every panel
# looks the same. Sourced at the top of the figure scripts.
# =============================================================================

## ---- style ----

# One color per relational context (same as the paper)
color_non_friends <- "#543287"   # purple
color_mutual      <- "#00A388"   # green
color_asymmetric  <- "#00B6EB"   # light blue

# Sex, used in the sociograms
color_female <- "#6B4359"
color_male   <- "#9CB27C"

# Supporting Information figures: slate bars and points, orange for the
# element each figure highlights (the cap, the baseline, the primary measure...)
color_slate     <- "#8A93A8"
color_highlight <- "#C95A3A"
color_text      <- "#383838"
color_slate_light <- "#B9C0D0"   # box plots and densities
color_red       <- "#D7263D"     # fit lines of the behavioral-rank figure (S30)

theme_paper <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_rect(color = "grey25"),
          legend.position = "none")
}

# Theme of the SI figures: white background, light horizontal grid, a thin
# frame and bold panel titles ("A. ...") aligned to the left
theme_si <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
          panel.border       = element_rect(color = "grey80", fill = NA, linewidth = 0.4),
          axis.ticks         = element_line(color = "grey60", linewidth = 0.3),
          axis.text          = element_text(color = color_text),
          axis.title         = element_text(color = color_text),
          plot.title         = element_text(face = "bold", size = base_size + 1, color = "#222222"),
          plot.title.position = "plot",
          plot.subtitle      = element_text(size = base_size - 1, color = "grey35"),
          strip.text         = element_text(face = "bold", size = base_size, color = "#222222", hjust = 0),
          legend.position    = "none")
}

# Variant used by the Matplotlib-style SI panels: no grid, a dark frame (or
# only the left and bottom axes with `l_axes = TRUE`) and centered titles
theme_si_frame <- function(base_size = 10, l_axes = FALSE) {
  frame <- if (l_axes) element_blank() else element_rect(color = "grey20", fill = NA, linewidth = 0.5)
  theme_minimal(base_size = base_size) +
    theme(panel.grid   = element_blank(),
          panel.border = frame,
          axis.line    = if (l_axes) element_line(color = "grey20", linewidth = 0.4) else element_blank(),
          axis.ticks   = element_line(color = "grey20", linewidth = 0.3),
          axis.text    = element_text(color = color_text),
          axis.title   = element_text(color = color_text),
          plot.title   = element_text(face = "bold", size = base_size + 1, hjust = 0.5, color = "#222222"),
          plot.subtitle = element_text(size = base_size - 1, hjust = 0.5, color = "grey35"),
          plot.background = element_rect(fill = "white", color = NA),
          legend.position = "none")
}

# Overall bold title above a patchwork of panels
figure_title <- function(title, size = 11) {
  plot_annotation(title = title,
                  theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = size)))
}

# Pieces repeated in several SI figures ----------------------------------

# Diverging palette (blue = negative, orange = positive) for the heatmaps of
# the Cooperation GAP by rank quartile (Figures S24 and S27)
scale_fill_coop_gap <- function() {
  scale_fill_gradientn(colors = c("#173A5E", "#4E7EA8", "#A9C2D6", "#F1EFE8",
                                  "#E7B57E", "#D4864A", "#C95A3A"),
                       limits = c(-2, 2), oob = scales::squish,
                       breaks = seq(-2, 2, 0.5), labels = function(x) sprintf("%.1f", x),
                       name = "Cooperation GAP (tokens)")
}
# Look of those heatmaps: dark frame, no grid, bold left-aligned titles
theme_heatmap <- function(base_size = 9) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.5),
          axis.text = element_text(color = "grey15", size = base_size),
          plot.title = element_text(face = "bold", size = base_size + 2),
          plot.subtitle = element_text(size = base_size - 0.5, color = "grey25"),
          legend.key.height = unit(1.6, "cm"), legend.key.width = unit(0.3, "cm"),
          legend.text = element_text(size = base_size - 1))
}
quartile_labels <- c("Q1\n(low)", "Q2", "Q3", "Q4\n(high)")

# A colored bar with a white title, used as a row or column header
header_bar <- function(label, color) {
  ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = color) +
    annotate("text", x = 0.5, y = 0.5, label = label, color = "white",
             fontface = "bold", size = 3.5) +
    theme_void()
}

# "p < 2.2e-16" or "p = 0.0012", as printed by cor.test()
format_p <- function(p) {
  if (p < 2.2e-16) "p < 2.2e-16" else paste("p =", format(signif(p, 2)))
}

# Save every figure as pdf and png with the same name
save_figure <- function(plot, name, width, height) {
  ggsave(here("output", "figures", paste0(name, ".pdf")), plot,
         width = width, height = height, bg = "white")
  ggsave(here("output", "figures", paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300, bg = "white")
}
