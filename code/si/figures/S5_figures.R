# =============================================================================
# S5_figures.R
#
# Figures of Supporting Information, Section 5 (the game and cooperation over time), with the SI layout, colors, panel titles and annotations.
#
# The analysis script code/si/S5_*.R computes the data and calls these
# functions. The notebooks show a quick version of each figure and keep this
# code in a collapsed box.
# =============================================================================

## ---- plot-figure-s9 ----
# Figure S9: (A) tokens sent, (B) the two allocations of each pair, (C) the
# Cooperation GAP with zero gaps in orange, (D) what the zero gaps are made of.
plot_figure_s9 <- function(dyads, pairs) {
  percent_axis <- scale_y_continuous(labels = function(x) paste0(x, "%"),
                                     expand = expansion(mult = c(0, 0.05)))

  panel_a <- dyads %>%
    count(tokens_sent) %>%
    mutate(percent = 100 * n / sum(n)) %>%
    ggplot(aes(tokens_sent, percent)) +
    geom_col(fill = color_slate, width = 0.8) +
    scale_x_continuous(breaks = 0:10) + percent_axis +
    labs(title = "A. Tokens sent", x = "Tokens sent", y = "Directed decisions (%)") +
    theme_si(9)

  pair_cells <- pairs %>%
    count(high = pmax(tokens_sent, tokens_received), low = pmin(tokens_sent, tokens_received)) %>%
    mutate(percent = 100 * n / sum(n))

  panel_b <- ggplot(pair_cells, aes(high, low, fill = percent)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f", percent)), size = 1.9, fontface = "bold",
              color = if_else(pair_cells$percent > 8, "white", "grey15")) +
    scale_fill_gradientn(colors = c("#FFF4D0", "#F2C66D", "#E39A55", "#C95A3A"),
                         name = "% of interactions") +
    scale_x_continuous(breaks = 0:10, expand = c(0, 0)) +
    scale_y_continuous(breaks = 0:10, expand = c(0, 0)) +
    coord_fixed() +
    labs(title = "B. Bivariate token distribution",
         x = "Tokens sent from j to i", y = "Tokens sent from i to j") +
    theme_si(9) +
    theme(panel.grid = element_blank(), panel.border = element_blank(),
          legend.position = c(0.25, 0.85), legend.direction = "horizontal",
          legend.title.align = 0, legend.key.height = unit(2, "mm"),
          legend.title = element_text(face = "bold", size = 7), legend.text = element_text(size = 6)) +
    guides(fill = guide_colorbar(title.position = "top"))

  gap_bars <- dyads %>%
    mutate(kind = case_when(coop_gap == 0 & tokens_sent == 0 ~ "0-0",
                            coop_gap == 0                    ~ "Equal positive",
                            TRUE                             ~ "Unequal")) %>%
    count(coop_gap, kind) %>%
    mutate(percent = 100 * n / sum(n),
           kind = factor(kind, levels = c("Equal positive", "0-0", "Unequal")))

  panel_c <- ggplot(gap_bars, aes(coop_gap, percent, fill = kind)) +
    geom_col(width = 0.8) +
    scale_fill_manual(values = c("Equal positive" = color_highlight, "0-0" = "#E8A06E",
                                 Unequal = color_slate)) +
    scale_x_continuous(breaks = seq(-10, 10, 2)) + percent_axis +
    labs(title = "C. Cooperation GAP", x = "Tokens received - tokens sent",
         y = "Directed decisions (%)") +
    theme_si(9)

  zero_gap <- pairs %>%
    filter(coop_gap == 0) %>%
    mutate(friend_group = if_else(friendship %in% c("Sender declares", "Receiver declares"),
                                  "Asymmetric ties", as.character(friend_group)),
           friend_group = factor(friend_group, levels = c("Asymmetric ties", "Mutual friends", "Non-friends")))

  zero_bars <- zero_gap %>%
    count(tokens_sent, friend_group) %>%
    mutate(percent = 100 * n / nrow(zero_gap)) %>%
    group_by(tokens_sent) %>%
    mutate(share = 100 * n / sum(n)) %>%
    ungroup()

  share_zero <- 100 * mean(zero_gap$tokens_sent == 0)

  panel_d <- ggplot(zero_bars, aes(tokens_sent, percent, fill = friend_group)) +
    geom_col(width = 0.8, color = "white", linewidth = 0.2) +
    # share of each friendship type inside the tall columns
    geom_text(data = filter(zero_bars, percent > 6),
              aes(label = paste0(round(share), "%")), position = position_stack(vjust = 0.5),
              angle = 90, color = "white", fontface = "bold", size = 2.4) +
    annotate("segment", x = -0.4, xend = 0.4, y = 55, yend = 55, color = "#E8A06E", linewidth = 1) +
    annotate("segment", x = 0.6, xend = 10.4, y = 55, yend = 55, color = color_highlight, linewidth = 1) +
    annotate("text", x = 0, y = 59, size = 2.4, fontface = "bold", color = "#E8A06E",
             label = sprintf("0-0\n%.1f%%", share_zero)) +
    annotate("text", x = 5.5, y = 59, size = 2.4, fontface = "bold", color = color_highlight,
             label = sprintf("Equal positive\n%.1f%%", 100 - share_zero)) +
    scale_fill_manual(values = c("Non-friends" = "#9B86C9", "Mutual friends" = "#7FCDBB",
                                 "Asymmetric ties" = "#8FD3EC"), name = "Friendship type") +
    scale_x_continuous(breaks = 0:10) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 25, 50),
                       expand = expansion(mult = c(0, 0.02))) +
    coord_cartesian(ylim = c(0, 63)) +
    labs(title = "D. Zero GAP decomposition", x = "Tokens sent by both students",
         y = "Zero-GAP dyads (%)") +
    theme_si(9) +
    theme(legend.position = c(0.3, 0.6), legend.title = element_text(face = "bold", size = 8),
          legend.key.size = unit(3, "mm"), legend.text = element_text(size = 7))

  figure_s9 <- (panel_a | panel_b) / (panel_c | panel_d)
  figure_s9
}


## ---- s5-figure-constants ----
progress_labels <- c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")


## ---- plot-figure-s10 ----
# Figure S10: mean tokens sent by classroom round, rounds 1-7, personal
# decision order and personal session progress.
plot_figure_s10 <- function(by_round, by_order, by_fifth) {
  time_panel <- function(data, x, title, xlab) {
    ggplot(data, aes({{ x }}, mean)) +
      geom_ribbon(aes(ymin = low, ymax = high), fill = color_slate, alpha = 0.18) +
      geom_line(color = color_slate, linewidth = 0.6) +
      geom_point(aes(size = classrooms), color = color_slate) +
      scale_size_continuous(range = c(0.3, 1.8)) +
      labs(title = title, x = xlab, y = "Mean tokens sent") +
      theme_si(8)
  }

  figure_s10 <- (time_panel(by_round, round, "A. Mean tokens sent by classroom game round",
                            "Classroom game round") |
                 time_panel(filter(by_round, round <= 7), round, "B. Common-support rounds 1-7",
                            "Classroom game round") + scale_x_continuous(breaks = 1:7)) /
                (time_panel(by_order, decision_order, "C. Personal decision order",
                            "Personal decision order") +
                   geom_vline(xintercept = 6, linetype = "dotted", color = "grey60") +
                   scale_x_continuous(breaks = c(1, 6, 10, 20, 30, 38)) |
                 ggplot(by_fifth, aes(fifth, mean)) +
                   geom_line(color = color_slate, linewidth = 0.6) +
                   geom_errorbar(aes(ymin = low, ymax = high), width = 0.08, color = color_slate) +
                   geom_point(color = color_slate, size = 1.8) +
                   scale_x_continuous(breaks = 1:5, labels = progress_labels) +
                   labs(title = "D. Personal session progress", x = "Personal session progress",
                        y = "Mean tokens sent") +
                   theme_si(8)) +
    plot_annotation(title = "Giving over classroom game rounds and personal progress",
                    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5)))
  figure_s10
}


## ---- s5-friend-colors ----
friend_colors <- c("Non-friends" = color_non_friends, "Asymmetric ties" = color_asymmetric,
                   "Mutual friends" = color_mutual, "Overall" = "#9AA0B4")


## ---- plot-figure-s11 ----
# Figure S11: the four views of Figure S10 by friendship group, with the
# overall mean as a dashed gray line.
plot_figure_s11 <- function(by_round_f, by_order_f, by_fifth_f, by_round, by_order, by_fifth) {
  # each group plus the overall mean (dashed gray)
  with_overall <- function(by_group, overall) {
    bind_rows(by_group, overall %>% mutate(friend_group = "Overall")) %>%
      mutate(friend_group = factor(friend_group, levels = names(friend_colors)))
  }

  group_panel <- function(data, x, title, xlab, ribbon = TRUE) {
    ggplot(data, aes({{ x }}, mean, color = friend_group, fill = friend_group)) +
      geom_ribbon(aes(ymin = low, ymax = high), alpha = if (ribbon) 0.12 else 0, color = NA) +
      geom_line(aes(linetype = friend_group == "Overall"), linewidth = 0.6) +
      geom_point(aes(size = classrooms)) +
      scale_color_manual(values = friend_colors, name = "Friendship nomination type") +
      scale_fill_manual(values = friend_colors, guide = "none") +
      scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "dashed"), guide = "none") +
      scale_size_continuous(range = c(0.3, 1.6), guide = "none") +
      labs(title = title, x = xlab, y = "Mean tokens sent") +
      theme_si(8)
  }

  figure_s11 <- (group_panel(with_overall(by_round_f, by_round), round,
                             "A. Mean tokens sent by classroom game round", "Classroom game round") +
                   coord_cartesian(ylim = c(0, 10.5)) +
                   theme(legend.position = c(0.3, 0.2), legend.title = element_text(size = 7),
                         legend.text = element_text(size = 6), legend.key.size = unit(3, "mm")) +
                   guides(color = guide_legend(ncol = 2)) |
                 group_panel(with_overall(filter(by_round_f, round <= 7), filter(by_round, round <= 7)),
                             round, "B. Common-support rounds 1-7", "Classroom game round") +
                   scale_x_continuous(breaks = 1:7)) /
                (group_panel(with_overall(by_order_f, by_order), decision_order,
                             "C. Personal decision order", "Personal decision order") +
                   geom_vline(xintercept = 6, linetype = "dotted", color = "grey60") +
                   scale_x_continuous(breaks = c(1, 6, 10, 20, 30, 38)) +
                   coord_cartesian(ylim = c(0, 10.5)) |
                 group_panel(with_overall(by_fifth_f, by_fifth), fifth,
                             "D. Personal session progress", "Personal session progress",
                             ribbon = FALSE) +
                   geom_errorbar(aes(ymin = low, ymax = high), width = 0.08) +
                   scale_x_continuous(breaks = 1:5, labels = progress_labels)) +
    plot_annotation(title = "Giving over the session by friendship nomination type",
                    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5)))
  figure_s11
}


## ---- plot-s12-panel-a ----
# Figure S12, panel A: aggregate change in mean giving and its two components.
plot_s12_panel_a <- function(decomposition, n_children) {
  group_colors <- friend_colors[c("Non-friends", "Asymmetric ties", "Mutual friends")]
  overall_change <- sum(decomposition$within + decomposition$composition)

  components <- decomposition %>%
    select(friend_group, `Category means` = within, `Category shares` = composition) %>%
    pivot_longer(-friend_group, names_to = "component") %>%
    mutate(friend_group = factor(friend_group, levels = c("Asymmetric ties", "Non-friends", "Mutual friends")))

  component_totals <- components %>% group_by(component) %>% summarise(total = sum(value)) %>%
    add_row(component = "Overall\nchange", total = overall_change) %>%
    mutate(component = factor(component, levels = c("Category shares", "Category means", "Overall\nchange")))

  figure_s12_a <- ggplot() +
    geom_col(data = filter(component_totals, component == "Overall\nchange"),
             aes(total, component), fill = color_slate, width = 0.5) +
    geom_col(data = components, aes(value, factor(component, levels = levels(component_totals$component)),
                                    fill = friend_group), width = 0.5) +
    geom_text(data = filter(components, component == "Category means"),
              aes(value, component, label = sprintf("%.3f", value), group = friend_group),
              position = position_stack(vjust = 0.5), color = "white", size = 2.2, fontface = "bold") +
    geom_point(data = component_totals, aes(total, component), size = 1) +
    geom_text(data = component_totals, aes(total, component, label = sprintf("%+.3f", total)),
              hjust = -0.3, size = 2.4) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    scale_fill_manual(values = group_colors) +
    scale_x_continuous(limits = c(-0.1, 1.05)) +
    scale_y_discrete(limits = c("Category shares", "Category means", "Overall\nchange")) +
    labs(title = "A. Aggregate change components",
         subtitle = sprintf("%s children | 45 classrooms | Equal weight per decision", n_children),
         x = "Change in mean tokens sent", y = NULL) +
    theme_si(8) + theme(panel.grid.major.y = element_blank())
  figure_s12_a
}


## ---- plot-s12-panel-b ----
# Figure S12, panel B: giving of the same children toward the classmates
# they named and toward the others, first and final fifth of the session.
plot_s12_panel_b <- function(child_means, child_level) {
  figure_s12_b <- ggplot(child_means, aes(period, mean, color = receiver_type, group = receiver_type)) +
    geom_line(aes(linetype = receiver_type)) +
    geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.06) +
    geom_point(aes(shape = receiver_type), size = 2, fill = "white") +
    scale_color_manual(values = c("Sender-nominated" = color_highlight, "Not sender-nominated" = color_slate), name = NULL) +
    scale_linetype_manual(values = c("Sender-nominated" = "solid", "Not sender-nominated" = "dashed"), name = NULL) +
    scale_shape_manual(values = c("Sender-nominated" = 16, "Not sender-nominated" = 21), name = NULL) +
    labs(title = "B. Giving within the same children",
         subtitle = sprintf("%d children | %d classrooms | Equal weight per child",
                            nrow(child_level), n_distinct(child_level$class_id)),
         x = "Personal session progress", y = "Mean tokens sent") +
    theme_si(8) + theme(legend.position = c(0.65, 0.12))
  figure_s12_b
}


## ---- plot-s12-panel-c ----
# Figure S12, panel C: within-child changes with classroom-bootstrap 95% CIs.
plot_s12_panel_c <- function(change_components, child_level) {
  figure_s12_c <- ggplot(change_components, aes(estimate, term, color = highlight)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = low, xmax = high), height = 0.15) +
    geom_point(size = 1.8) +
    geom_text(aes(x = high, label = sprintf("%+.2f", estimate)), hjust = -0.2, size = 2.4) +
    scale_color_manual(values = c(`FALSE` = "grey20", `TRUE` = color_highlight)) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(title = "C. Within-child change components",
         subtitle = sprintf("%d children | First vs. final 20%% of personal session progress", nrow(child_level)),
         x = "Mean within-child change (tokens)", y = NULL) +
    theme_si(8) + theme(panel.grid.major.y = element_blank())
  figure_s12_c
}


## ---- plot-figure-s12 ----
# Figure S12: the three panels side by side
plot_figure_s12 <- function(decomposition, child_means, change_components, child_level, n_children) {
  plot_s12_panel_a(decomposition, n_children) |
    plot_s12_panel_b(child_means, child_level) |
    plot_s12_panel_c(change_components, child_level)
}


## ---- plot-figure-s13 ----
# Figure S13: (A) giving over the session by classroom size; (B) classroom-
# specific trends against classroom size.
plot_figure_s13 <- function(size_groups, class_slopes) {
  size_colors <- c("Small (7-15)" = "#5BA4B5", "Medium (16-21)" = "#2E7F8C", "Large (22-39)" = "#173F45")

  figure_s13 <- (ggplot(size_groups, aes(fifth, mean, color = size_group)) +
                   geom_ribbon(aes(ymin = low, ymax = high, fill = size_group), alpha = 0.15, color = NA) +
                   geom_line() + geom_point(size = 1.2) +
                   scale_color_manual(values = size_colors, name = NULL) +
                   scale_fill_manual(values = size_colors, guide = "none") +
                   scale_x_continuous(breaks = 1:5, labels = progress_labels) +
                   labs(title = "A. Personal session progress by classroom size",
                        x = "Personal session progress", y = "Mean tokens sent") +
                   theme_si(8) + theme(legend.position = c(0.75, 0.15))) |
                (ggplot(class_slopes, aes(class_size, slope)) +
                   geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
                   geom_smooth(method = "lm", color = color_highlight, fill = color_highlight,
                               alpha = 0.15, linewidth = 0.6) +
                   geom_point(color = color_slate) +
                   labs(title = "B. Classroom-specific trends and class size",
                        x = "Participants in classroom", y = "First-to-last change in tokens sent") +
                   theme_si(8))
  figure_s13 <- figure_s13 +
    plot_annotation(title = "Temporal sending patterns and classroom size",
                    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5)))
  figure_s13
}


## ---- plot-figure-s14 ----
plot_figure_s14 <- function(intervals) {
  # Figure S14: median interval (and interquartile range) by decile of the
  # child's decision sequence, for all decisions and by relationship
  pacing_by_group <- bind_rows(
    intervals %>% mutate(panel = "A. All relations"),
    intervals %>% mutate(panel = c("Non-friends" = "B. Non-friends", "Asymmetric ties" = "C. Asymmetric ties",
                                   "Mutual friends" = "D. Mutual friends")[as.character(friend_group)])
  ) %>%
    group_by(panel, decile) %>%
    summarise(median = median(seconds), q25 = quantile(seconds, 0.25), q75 = quantile(seconds, 0.75),
              .groups = "drop")

  panel_colors <- c("A. All relations" = color_slate, "B. Non-friends" = color_non_friends,
                    "C. Asymmetric ties" = color_asymmetric, "D. Mutual friends" = color_mutual)
  decile_labels <- paste0((0:9) * 10, "-", (1:10) * 10)

  figure_s14 <- ggplot(pacing_by_group, aes(decile, median, color = panel, fill = panel)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.18, color = NA) +
    geom_line(linewidth = 0.7) + geom_point(size = 1) +
    facet_wrap(~ panel, ncol = 2) +
    scale_color_manual(values = panel_colors) + scale_fill_manual(values = panel_colors) +
    scale_x_continuous(breaks = 1:10, labels = decile_labels) +
    scale_y_continuous(limits = c(0, 125), breaks = seq(0, 120, 20)) +
    labs(title = "Decision pacing over the session", x = "Within-session progress decile (%)",
         y = "Interval (seconds)") +
    theme_si(8) +
    theme(plot.title = element_text(hjust = 0.5), plot.title.position = "panel",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
  figure_s14
}
