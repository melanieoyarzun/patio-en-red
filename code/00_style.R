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

theme_paper <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_rect(color = "grey25"),
          legend.position = "none")
}

# Save every figure as pdf and png with the same name
save_figure <- function(plot, name, width, height) {
  ggsave(here("output", "figures", paste0(name, ".pdf")), plot,
         width = width, height = height, bg = "white")
  ggsave(here("output", "figures", paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300, bg = "white")
}
