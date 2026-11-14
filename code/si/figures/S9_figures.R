# =============================================================================
# S9_figures.R
#
# Figures of Supporting Information, Section 9 (Behavioral Social Rank), with the SI layout, colors, panel titles and annotations.
#
# The analysis script code/si/S9_*.R computes the data and calls these
# functions. The notebooks show a quick version of each figure and keep this
# code in a collapsed box.
# =============================================================================

## ---- plot-figure-s29 ----
# Figure S29: Figure 3 panels B and C with the Behavioral Social Rank GAP,
# one column per friendship type.
plot_figure_s29 <- function(bsr_bins, dyads) {
  s29_groups <- tribble(
    ~friendship,       ~title,        ~color,
    "Non-friends",     "Non Friends", color_non_friends,
    "Mutual",          "Mutual",      color_mutual,
    "Sender declares", "Ego declares", color_asymmetric
  )

  rank_side_labels <- annotate("text", x = c(-0.55, 0.55), y = -Inf, vjust = -0.6, size = 2,
                               fontface = "bold", label = c("Receiver has higher rank", "Sender has higher rank"))

  s29_column <- function(friendship, title, color) {
    bins <- bsr_bins %>% filter(friendship == !!friendship)
    header <- header_bar(title, color)

    tokens <- ggplot(bins, aes(gap)) +
      geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
      geom_hline(yintercept = mean(dyads$tokens_sent), linetype = "dotted", color = "grey40") +
      geom_ribbon(aes(ymin = received, ymax = sent), fill = "#F0E5A8", alpha = 0.7) +
      geom_line(aes(y = sent), color = color, linewidth = 0.8) +
      geom_errorbar(aes(ymin = sent - sent_ci, ymax = sent + sent_ci), color = color, width = 0.03) +
      geom_point(aes(y = sent), color = color, shape = 18, size = 2.6) +
      geom_line(aes(y = received), color = color, linewidth = 0.8) +
      geom_errorbar(aes(ymin = received - received_ci, ymax = received + received_ci), color = color, width = 0.03) +
      geom_point(aes(y = received), color = color, shape = 15, size = 2) +
      rank_side_labels +
      scale_x_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, 0.5)) +
      coord_cartesian(ylim = c(-0.5, 11.5)) +
      labs(x = NULL, y = if (friendship == "Non-friends") "Tokens" else NULL) +
      theme_si(7)

    gaps <- bins %>%
      pivot_longer(c(Observed, Filtered), names_to = "version", values_to = "coop_gap") %>%
      mutate(ci = if_else(version == "Observed", Observed_ci, Filtered_ci)) %>%
      ggplot(aes(gap, coop_gap, alpha = version)) +
      geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
      geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
      geom_line(color = color, linewidth = 0.9) +
      geom_errorbar(aes(ymin = coop_gap - ci, ymax = coop_gap + ci), color = color, width = 0.03) +
      geom_point(color = color, size = 2) +
      annotate("text", x = 1.05, y = -4.6, hjust = 1, size = 2, fontface = "bold", color = color,
               label = "Non random") +
      annotate("text", x = 1.05, y = -5.4, hjust = 1, size = 2, fontface = "bold", color = color,
               alpha = 0.4, label = "Observed") +
      rank_side_labels +
      scale_alpha_manual(values = c(Filtered = 1, Observed = 0.4)) +
      scale_x_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, 0.5)) +
      coord_cartesian(ylim = c(-6.5, 6)) +
      labs(x = NULL, y = if (friendship == "Non-friends") "GAP [Received - Sent]" else NULL) +
      theme_si(7)

    header / tokens / header / gaps + plot_layout(heights = c(0.12, 1, 0.12, 1))
  }

  figure_s29 <- pmap(s29_groups, s29_column) %>%
    wrap_plots(nrow = 1) +
    plot_annotation(caption = "Behavioral Social Ranking GAP [game PageRank sender - receiver]",
                    theme = theme(plot.caption = element_text(hjust = 0.5, size = 9)))
  figure_s29
}


## ---- plot-figure-s30 ----
# Figure S30: (A) the two PageRank scores on log scales; (B-H) Behavioral
# Social Rank against other characteristics, with binned means, a linear fit
# and the pooled Pearson correlation.
plot_figure_s30 <- function(students, bsr_correlates, s30_labels, s30_titles) {
  correlate_panels <- function(panels, ncol) {
    data <- bsr_correlates %>% filter(panel %in% panels)
    ggplot(data, aes(bsr, value)) +
    geom_jitter(color = color_slate, alpha = 0.3, size = 0.5, width = 0.005, height = 0.05) +
    geom_smooth(method = "lm", color = color_red, fill = color_red, alpha = 0.15, linewidth = 0.6) +
    stat_summary_bin(fun.data = mean_cl_normal, bins = 10, size = 0.15, linewidth = 0.3) +
    geom_label(data = filter(s30_labels, panel %in% panels), aes(x = 0.5, y = -2.7, label = label),
               color = color_red, size = 2.2, label.size = 0.3) +
    facet_wrap(~ panel, ncol = ncol) +
    coord_cartesian(ylim = c(-3, 3)) +
    labs(x = NULL, y = "Z-score") +
    theme_si(7)
  }

  pagerank_test <- cor.test(log(students$bsr_pagerank), log(students$pagerank))
  panel_a <- ggplot(students, aes(bsr_pagerank, pagerank)) +
    geom_point(color = color_slate, alpha = 0.35, size = 0.6) +
    geom_smooth(method = "lm", color = color_red, se = FALSE, linewidth = 0.6, fullrange = TRUE) +
    stat_summary_bin(fun.data = mean_cl_normal, bins = 10, size = 0.15, linewidth = 0.3) +
    annotate("label", x = 0.06, y = 0.012, size = 2.2, color = color_red, label.size = 0.3,
             label = sprintf("R = %.2f, %s", pagerank_test$estimate, format_p(pagerank_test$p.value))) +
    scale_x_log10() + scale_y_log10() +
    labs(title = "A. Social Rank measures\ncomparison (log scale)",
         x = "Behavioral Social Rank\n(game PageRank)", y = "Popularity nomination PageRank") +
    theme_si(7)

  figure_s30 <- ((panel_a | correlate_panels(s30_titles[1:3], 3)) + plot_layout(widths = c(1, 3))) /
    correlate_panels(s30_titles[4:7], 4) +
    plot_annotation(caption = "Behavioral Social Rank (normalized Social Dilemma Game PageRank)",
                    theme = theme(plot.caption = element_text(hjust = 0.5, size = 9)))
  figure_s30
}
