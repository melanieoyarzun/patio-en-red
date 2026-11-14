# =============================================================================
# S7_figures.R
#
# Figures of Supporting Information, Section 7 (main regressions), with the SI layout, colors, panel titles and annotations.
#
# The analysis script code/si/S7_*.R computes the data and calls these
# functions. The notebooks show a quick version of each figure and keep this
# code in a collapsed box.
# =============================================================================

## ---- plot-figure-s22 ----
plot_figure_s22 <- function(dyads, abs_gap) {
  p90 <- quantile(abs_gap, 0.9)
  reference_lines <- tibble(x = c(-p90, p90, -1, 1),
                            kind = rep(c(sprintf("90th pct |GAP| = %.2f", p90), "\u00b11 (extreme top-vs-bottom)"), each = 2))

  figure_s22 <- ggplot(dyads, aes(rank_gap)) +
    geom_histogram(binwidth = 0.033, fill = color_slate, color = "white", linewidth = 0.2) +
    geom_vline(data = reference_lines, aes(xintercept = x, color = kind, linetype = kind)) +
    scale_color_manual(values = c(color_highlight, "grey20"), name = NULL) +
    scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
    scale_x_continuous(breaks = seq(-1, 1, 0.25)) +
    labs(title = "Observed Social Rank GAP across all dyads",
         x = "Social Rank GAP (sender - receiver, normalized PageRank)", y = "Dyads") +
    theme_si(10) + theme(legend.position = c(0.82, 0.9))
  figure_s22
}


## ---- plot-figure-s23 ----
plot_figure_s23 <- function(dyads, beta_main) {
  # Figure S23: binned means and the linear prediction over the whole range,
  # with the central 50%, 80% and 90% of dyads shaded
  binned <- dyads %>%
    mutate(bin = ntile(rank_gap, 20)) %>%
    group_by(bin) %>%
    summarise(gap = mean(rank_gap), mean = mean(coop_gap), se = sd(coop_gap) / sqrt(n()))

  bands <- tibble(share = c("90% of dyads", "80% of dyads", "50% of dyads"),
                  low   = quantile(dyads$rank_gap, c(0.05, 0.10, 0.25)),
                  high  = quantile(dyads$rank_gap, c(0.95, 0.90, 0.75))) %>%
    mutate(share = factor(share, levels = c("50% of dyads", "80% of dyads", "90% of dyads")))

  figure_s23 <- ggplot() +
    geom_rect(data = bands, aes(xmin = low, xmax = high, ymin = -Inf, ymax = Inf, fill = share)) +
    geom_vline(xintercept = 0, color = "grey45", linewidth = 0.3) +
    geom_abline(aes(slope = beta_main, intercept = 0, linetype = "prediction"),
                color = color_highlight, linewidth = 1, key_glyph = "path") +
    geom_errorbar(data = binned, aes(gap, ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                  width = 0.015, color = "grey25") +
    geom_point(data = binned, aes(gap, mean, shape = "Binned means (95% CI)"), color = "grey20", size = 1.8) +
    scale_fill_manual(values = c("50% of dyads" = "#C3CAD8", "80% of dyads" = "#D9DEE7",
                                 "90% of dyads" = "#ECEEF3"), name = NULL) +
    scale_linetype_manual(values = "solid", name = NULL,
                          labels = as.expression(list(
                            bquote("Linear prediction (" * hat(beta) == .(sprintf("%.2f", beta_main)) * ")")))) +
    scale_shape_manual(values = 16, name = NULL) +
    scale_x_continuous(breaks = seq(-1, 1, 0.25), labels = function(x) sprintf("%.2f", x)) +
    scale_y_continuous(breaks = -2:2) +
    coord_cartesian(xlim = c(-1, 1), ylim = c(-2.8, 2.8), expand = FALSE) +
    labs(title = "Predicted Cooperation GAP over the observed range",
         x = "Social Rank GAP (sender - receiver)", y = "Predicted Cooperation GAP (tokens)") +
    theme_si(10) +
    theme(legend.position = c(0.005, 0.995), legend.justification = c(0, 1),
          legend.spacing.y = unit(0, "mm"), legend.key.size = unit(3.5, "mm"),
          legend.text = element_text(size = 7.5),
          legend.background = element_blank(), legend.margin = margin(0, 4, 0, 4),
          legend.box.background = element_rect(fill = alpha("white", 0.8), color = "grey80"),
          plot.margin = margin(5, 12, 5, 5),
          axis.line.x = element_line(color = "grey15")) +
    guides(linetype = guide_legend(order = 1), shape = guide_legend(order = 2),
           fill = guide_legend(order = 3))
  figure_s23
}


## ---- plot-figure-s24 ----
plot_figure_s24 <- function(quartile_cells) {
  # Figure S24: Cooperation GAP by sender and receiver rank quartile
  quartile_panel <- function(value, title, legend = FALSE) {
    ggplot(quartile_cells, aes(factor(receiver_q, labels = quartile_labels),
                               factor(sender_q, labels = quartile_labels), fill = .data[[value]])) +
      geom_tile(color = "white", linewidth = 0.6) +
      geom_text(aes(label = sprintf("%+.2f", .data[[value]]),
                    color = abs(.data[[value]]) > 1.1), fontface = "bold", size = 3.4, vjust = -0.2) +
      geom_text(aes(label = sprintf("%.0f%% of dyads", share),
                    color = abs(.data[[value]]) > 1.1), size = 2.1, vjust = 1.8) +
      scale_fill_coop_gap() +
      scale_color_manual(values = c(`FALSE` = "grey15", `TRUE` = "white"), guide = "none") +
      coord_fixed() +
      scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
      labs(title = title, x = "Receiver's rank quartile",
           y = if (legend) NULL else "Sender's rank quartile") +
      theme_heatmap(9) +
      theme(legend.position = if (legend) "right" else "none") +
      guides(fill = guide_colorbar(title.position = "right",
                                   title.theme = element_text(angle = 90, size = 8)))
  }
  figure_s24 <- (quartile_panel("predicted", "A. Model-predicted") |
                 quartile_panel("observed", "B. Observed", legend = TRUE)) +
    plot_annotation(title = "Cooperation GAP by sender and receiver Social Rank quartile",
                    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5)))
  figure_s24
}
