# =============================================================================
# S8_figures.R
#
# Figures of Supporting Information, Section 8 (robustness), with the SI layout, colors, panel titles and annotations.
#
# The analysis script code/si/S8_*.R computes the data and calls these
# functions. The notebooks show a quick version of each figure and keep this
# code in a collapsed box.
# =============================================================================

## ---- plot-figure-s25 ----
plot_figure_s25 <- function() {
  # The panels reuse the Figure 3 plotting functions (code/05_figure3_cooperation.R),
  # drawn with filtered tokens: (A) filtered tokens sent and received,
  # (B) filtered Cooperation GAP in front, observed behind
  figure3 <- new.env()
  sys.source(here("code", "05_figure3_cooperation.R"), envir = figure3)

  s25_rows <- tribble(
    ~label,                         ~group,            ~color,            ~y_min,
    "I    Non-friends",             "Non-friends",     color_non_friends, -6,
    "II    Mutual friends",         "Mutual",          color_mutual,      -6,
    "III    Sender-declared friendship", "Sender declares", color_asymmetric, -8.5
  )

  figure_s25 <- pmap(s25_rows, function(label, group, color, y_min) {
    header_bar(label, color) /
      (figure3$plot_tokens(group, color, version = "Filtered") + labs(title = "A") |
       figure3$plot_coop_gap(group, color, y_limits = c(y_min, 6), main = "Filtered") + labs(title = "B")) +
      plot_layout(heights = c(0.1, 1))
  }) %>%
    wrap_plots(ncol = 1)
  figure_s25
}


## ---- plot-figure-asymmetric-thresholds ----
# Asymmetric friendships by direction across the retention grid: (A) Social
# Rank GAP coefficient with 95% CI, three specifications; (B) data left.
plot_figure_asymmetric_thresholds <- function(asymmetric_thresholds) {
  direction_colors <- c("Low to high" = color_highlight, "High to low" = color_slate)
  dodge <- position_dodge(width = 4)
  retention_labels <- function(x) if_else(x == 100, "100%\n(observed)", paste0(x, "%"))

  # the figure shows the range where the samples still support estimation
  figure_range <- asymmetric_thresholds %>% filter(retention >= 0.4)

  coef_panel <- figure_range %>%
    filter(!is.na(estimate), spec %in% levels(spec)[1:3]) %>%
    ggplot(aes(100 * retention, estimate, color = direction)) +
    annotate("rect", xmin = 45, xmax = 55, ymin = -Inf, ymax = Inf, fill = "grey92") +
    geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
    geom_line(position = dodge, linewidth = 0.4, alpha = 0.6) +
    geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                  width = 0, position = dodge, linewidth = 0.5) +
    geom_point(position = dodge, size = 1.8) +
    facet_wrap(~ spec, nrow = 1) +
    scale_x_reverse(breaks = c(100, 70, 60, 50, 40), labels = retention_labels) +
    scale_color_manual(values = direction_colors, name = NULL) +
    labs(title = "A. Social Rank GAP coefficient (95% CI)", x = NULL, y = "Coefficient") +
    theme_si() +
    theme(legend.position = "top", legend.justification = "left")

  support_panel <- figure_range %>%
    distinct(retention, direction, n, senders_2plus) %>%
    pivot_longer(c(n, senders_2plus), names_to = "measure", values_to = "count") %>%
    mutate(measure = recode(measure, n = "Decisions retained",
                            senders_2plus = "Senders with 2+ retained decisions")) %>%
    ggplot(aes(100 * retention, count, color = direction)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.6) +
    facet_wrap(~ measure, nrow = 1, scales = "free_y") +
    scale_x_reverse(breaks = c(100, 70, 60, 50, 40), labels = retention_labels) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_color_manual(values = direction_colors, guide = "none") +
    labs(title = "B. Data left for estimation", x = "Share of transfers retained by the filter", y = NULL) +
    theme_si()

  figure_asymmetric_thresholds <- coef_panel / support_panel + plot_layout(heights = c(1.2, 1))
  figure_asymmetric_thresholds
}


## ---- plot-figure-s26 ----
# Figure S26: Social Rank GAP coefficient under each nomination-cap
# construction; the baseline in orange.
plot_figure_s26 <- function(table_s35) {
  figure_s26 <- table_s35 %>%
    mutate(label = c("Baseline\n(all nominations)", "Fixed cap (k=3)", "Fixed cap (k=4)",
                     "Exclude cap-reachers (\u22655)"),
           label = fct_rev(fct_inorder(label)),
           baseline = row_number() == 1) %>%
    ggplot(aes(estimate, label, color = baseline)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se), height = 0.15) +
    geom_point(aes(size = baseline)) +
    scale_color_manual(values = c(`FALSE` = "grey25", `TRUE` = color_highlight)) +
    scale_size_manual(values = c(`FALSE` = 2, `TRUE` = 3)) +
    scale_x_continuous(limits = c(-0.2, 3.5), breaks = seq(0, 3.5, 0.5)) +
    labs(title = "Nomination-cap sensitivity", x = "Social Rank GAP coefficient (received - sent)", y = NULL) +
    theme_si(10) +
    theme(plot.title = element_text(hjust = 0.5), plot.title.position = "panel",
          axis.text.y = element_text(face = c("plain", "plain", "plain", "bold"), size = 10),
          panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
          panel.grid.major.y = element_blank())
  figure_s26
}


## ---- plot-figure-s27 ----
# Figure S27: observed Cooperation GAP by sender and receiver rank quartile,
# in small, medium and large classrooms.
plot_figure_s27 <- function(dyads, size_slopes) {
  quartile_cuts <- quantile(c(dyads$sender_rank, dyads$receiver_rank), c(0.25, 0.5, 0.75))
  size_titles <- size_slopes %>%
    left_join(dyads %>% distinct(class_id, size_group) %>% count(size_group, name = "classes"), by = "size_group") %>%
    mutate(panel = paste0(c("A. Small (n \u2264 15)", "B. Medium (16-21)", "C. Large (n > 21)"),
                          "\n", sprintf("beta = %.2f \u00b7 %d classes", estimate, classes)))

  figure_s27 <- dyads %>%
    mutate(sender_q   = findInterval(sender_rank, quartile_cuts, left.open = TRUE) + 1,
           receiver_q = findInterval(receiver_rank, quartile_cuts, left.open = TRUE) + 1) %>%
    group_by(size_group, sender_q, receiver_q) %>%
    summarise(observed = mean(coop_gap), .groups = "drop") %>%
    left_join(size_titles %>% select(size_group, panel), by = "size_group") %>%
    ggplot(aes(factor(receiver_q, labels = quartile_labels), factor(sender_q, labels = quartile_labels),
               fill = observed)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%+.2f", observed), color = abs(observed) > 1.1),
              fontface = "bold", size = 2.8) +
    scale_fill_coop_gap() +
    scale_color_manual(values = c(`FALSE` = "grey15", `TRUE` = "white"), guide = "none") +
    facet_wrap(~ panel) +
    coord_fixed() +
    labs(title = "Observed Cooperation GAP by rank quartile, across classroom sizes",
         x = "Receiver's rank quartile", y = "Sender's rank quartile") +
    theme_minimal(base_size = 9) +
    theme(panel.grid = element_blank(), strip.text = element_text(face = "bold", hjust = 0),
          plot.title = element_text(face = "bold", hjust = 0.5),
          legend.key.height = unit(1.4, "cm")) +
    guides(fill = guide_colorbar(title.position = "right",
                                 title.theme = element_text(angle = 90, size = 8)))
  figure_s27
}


## ---- plot-figure-s28 ----
# Figure S28: Social Rank GAP coefficient for each rank construction and
# sample; the primary Social Rank in orange.
plot_figure_s28 <- function(rank_robustness) {
  figure_s28 <- ggplot(rank_robustness, aes(estimate, measure, color = primary)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se), height = 0.2) +
    geom_point(size = 1.8) +
    facet_wrap(~ sample, nrow = 1, scales = "free_x") +
    scale_color_manual(values = c(`FALSE` = "grey35", `TRUE` = color_highlight)) +
    labs(title = "Social Rank GAP robustness across popularity-network constructions",
         x = "Coefficient", y = NULL) +
    theme_si(8) +
    theme(plot.title = element_text(hjust = 0.5), plot.title.position = "panel",
          axis.text.y = element_text(color = rev(c(color_highlight, rep(color_text, 5))),
                                     face = rev(c("bold", rep("plain", 5)))),
          panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3))
  figure_s28
}
