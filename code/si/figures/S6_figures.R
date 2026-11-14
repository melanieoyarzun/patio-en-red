# =============================================================================
# S6_figures.R
#
# Figures of Supporting Information, Section 6 (identification), with the SI layout, colors, panel titles and annotations.
#
# The analysis script code/si/S6_*.R computes the data and calls these
# functions. The notebooks show a quick version of each figure and keep this
# code in a collapsed box.
# =============================================================================

## ---- plot-figure-s15 ----
# Figure S15: balance coefficients in within-class-round SD units; the
# demeaned partner rank (the design-based check) in orange.
plot_figure_s15 <- function(figure_s15_data, outcomes) {
  short_outcomes <- c("Social Rank GAP\n(partner - sender)", "Partner Rank", "Partner Rank\n(demeaned)")
  names(short_outcomes) <- outcomes

  figure_s15 <- figure_s15_data %>%
    mutate(outcome = factor(short_outcomes[as.character(outcome)], levels = short_outcomes),
           term = factor(term, levels = c("GPA", "Attendance", "Female")),
           highlight = outcome == "Partner Rank\n(demeaned)") %>%
    ggplot(aes(estimate, term, color = highlight)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    geom_errorbarh(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se), height = 0.15) +
    geom_point(size = 1.8) +
    facet_wrap(~ outcome, nrow = 1) +
    scale_color_manual(values = c(`FALSE` = color_slate, `TRUE` = color_highlight)) +
    scale_x_continuous(limits = c(-0.7, 0.35), breaks = c(-0.6, -0.3, 0, 0.3)) +
    labs(x = "Coefficient on balance outcome\n(within-class-round SD units)", y = NULL) +
    theme_si(8) +
    theme(strip.text = element_text(hjust = 0.5), panel.grid.major.y = element_line(color = "grey92"))
  figure_s15
}


## ---- plot-figure-s16 ----
# Figure S16: round-by-round balance coefficients; demeaned partner rank in orange.
plot_figure_s16 <- function(round_balance) {
  row_labels <- c(raw_gap = "Social Rank GAP\n(partner - sender)",
                  partner_rank = "Partner Rank\n(assigned)",
                  partner_rank_demeaned = "Partner Rank\n(demeaned)")

  figure_s16 <- round_balance %>%
    mutate(highlight = outcome == "partner_rank_demeaned",
           outcome = factor(row_labels[outcome], levels = row_labels),
           covariate = factor(recode(covariate, female = "Female", attendance_z = "Attendance", gpa = "GPA"),
                              levels = c("Attendance", "GPA", "Female"))) %>%
    ggplot(aes(round_id, estimate, color = highlight)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
    geom_linerange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), linewidth = 0.3) +
    geom_point(size = 0.9) +
    facet_grid(outcome ~ covariate, scales = "free_y") +
    scale_color_manual(values = c(`FALSE` = color_slate, `TRUE` = color_highlight)) +
    scale_x_continuous(breaks = c(1, 10, 20, 30, 40)) +
    labs(x = "Round", y = "Round-specific balance coefficient") +
    theme_si(8) +
    theme(strip.text.x = element_text(hjust = 0.5), strip.text.y = element_text(angle = 0, hjust = 0))
  figure_s16
}


## ---- plot-figure-s17 ----
# Figure S17: demeaned rank gap by covariate group (low in slate, high in orange).
plot_figure_s17 <- function(density_data) {
  # legend text inside each panel
  group_names <- tribble(
    ~covariate,      ~Low,              ~High,
    "A. Attendance", "Low attendance",  "High attendance",
    "B. GPA",        "Low GPA",         "High GPA",
    "C. Sex",        "Boys",            "Girls"
  )

  figure_s17 <- ggplot(density_data, aes(gap_demeaned)) +
    geom_density(data = filter(density_data, group == "Low"),
                 fill = color_slate, color = "#6F7A93", alpha = 0.5, linewidth = 0.8) +
    geom_density(data = filter(density_data, group == "High"),
                 fill = color_highlight, color = color_highlight, alpha = 0.12, linewidth = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_text(data = group_names, aes(x = 0.95, y = 1.35, label = Low), hjust = 1,
              color = "#6F7A93", fontface = "bold", size = 3) +
    geom_text(data = group_names, aes(x = 0.95, y = 1.2, label = High), hjust = 1,
              color = color_highlight, fontface = "bold", size = 3) +
    facet_wrap(~ covariate, nrow = 1) +
    coord_cartesian(xlim = c(-1, 1)) +
    labs(x = "Rank gap (demeaned within class x round)", y = "Density") +
    theme_si(9)
  figure_s17
}


## ---- plot-figure-s18 ----
# Figure S18: Social Rank GAP by round (box plots).
plot_figure_s18 <- function(dyads) {
  figure_s18 <- ggplot(dyads, aes(factor(round), rank_gap)) +
    geom_boxplot(fill = color_slate_light, color = "grey25", linewidth = 0.3,
                 outlier.size = 0.1, outlier.alpha = 0.3) +
    labs(x = "Round", y = "Social Rank GAP") +
    theme_si(8) + theme(axis.text.x = element_text(size = 5))
  figure_s18
}


## ---- plot-figure-s19 ----
# Figure S19: rank gap distribution round by round in one classroom.
plot_figure_s19 <- function(dyads, example_rounds) {
  figure_s19 <- dyads %>% filter(class_id == example_rounds) %>%
    mutate(round_label = factor(paste("Round", round), levels = paste("Round", sort(unique(round))))) %>%
    ggplot(aes(rank_gap)) +
    geom_density(fill = color_slate_light, color = "#6F7A93", alpha = 0.8, bw = "nrd0", adjust = 0.6) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) +
    facet_wrap(~ round_label, ncol = 5) +
    scale_x_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) +
    scale_y_continuous(breaks = c(0, 1.5, 3, 4.5)) +
    coord_cartesian(ylim = c(0, 4.5)) +
    labs(title = sprintf("Rank gap distribution, Class %d", example_rounds),
         x = "Social Rank GAP", y = "Density") +
    theme_si(7) +
    theme(strip.text = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5),
          plot.title.position = "panel", panel.border = element_blank(),
          axis.line = element_line(color = "grey40", linewidth = 0.3), panel.grid.major.y = element_blank())
  figure_s19
}


## ---- plot-figure-s20 ----
# Figure S20: Social Rank GAP and Cooperation GAP by classroom.
plot_figure_s20 <- function(dyads) {
  figure_s20 <- dyads %>%
    select(class_id, `A. Social Rank GAP by classroom` = rank_gap,
           `B. Cooperation GAP by classroom` = coop_gap) %>%
    pivot_longer(-class_id) %>%
    ggplot(aes(factor(class_id), value)) +
    geom_hline(yintercept = 0, color = "grey30", linewidth = 0.4) +
    geom_boxplot(fill = color_slate_light, color = "grey25", linewidth = 0.3,
                 outlier.size = 0.1, outlier.alpha = 0.3) +
    facet_wrap(~ name, ncol = 1, scales = "free_y") +
    labs(x = "Class ID (ordered as in Table S1)", y = NULL) +
    theme_si(8)
  figure_s20
}


## ---- plot-figure-s21 ----
# Figure S21 as in the SI: a "raincloud" (decisions as dots, a box plot and
# half a density) with the support statistics printed under each group.
plot_figure_s21 <- function(dyads, support) {
  rain <- dyads %>%
    filter(friendship %in% c("Non-friends", "Mutual")) %>%
    mutate(group = if_else(friendship == "Mutual", "Mutual friends", "Non-friends"),
           x = if_else(group == "Non-friends", 1, 2))

  half_density <- rain %>%
    group_by(group, x) %>%
    reframe(density(rank_gap, from = -1, to = 1, n = 256)[c("x", "y")] %>% as_tibble() %>%
              rename(gap = x, d = y)) %>%
    group_by(group) %>%
    mutate(width = 0.3 * d / max(d)) %>%
    ungroup()

  rain_colors <- c("Non-friends" = color_non_friends, "Mutual friends" = color_mutual)

  support_text <- support %>%
    mutate(group = if_else(friendship == "Mutual", "Mutual friends", "Non-friends"),
           x = if_else(group == "Non-friends", 1, 2)) %>%
    transmute(x,
              obs  = format(n, big.mark = ","),
              sd   = sprintf("%.3f", sd),
              iqr  = map_dbl(group, ~ IQR(rain$rank_gap[rain$group == .x])) %>% sprintf("%.3f", .),
              s025 = sprintf("%.1f%% (%d/45)", share_025, classes_025),
              s050 = sprintf("%.1f%% (%d/45)", share_050, classes_050)) %>%
    pivot_longer(-x, names_to = "row", values_to = "text") %>%
    mutate(y = -1.2 - 0.12 * match(row, c("obs", "sd", "iqr", "s025", "s050")))

  row_names <- tibble(text = c("Observations", "SD", "IQR", "|GAP|>=0.25", "|GAP|>=0.50"),
                      y = -1.2 - 0.12 * (1:5))

  figure_s21 <- ggplot() +
    geom_hline(yintercept = c(-0.25, 0.25), linetype = "dashed", color = "#1F3A5F", linewidth = 0.4) +
    geom_hline(yintercept = c(-0.5, 0.5), linetype = "dotted", color = "#B03A2E", linewidth = 0.6) +
    geom_hline(yintercept = 0, color = "grey40", linewidth = 0.3) +
    geom_jitter(data = rain, aes(x - 0.3, rank_gap, color = group),
                width = 0.07, height = 0, size = 0.05, alpha = 0.3) +
    geom_boxplot(data = rain, aes(x, rank_gap, group = group, color = group, fill = group),
                 width = 0.12, alpha = 0.15, outlier.shape = NA) +
    geom_polygon(data = half_density %>% group_by(group) %>%
                   reframe(gx = c(x[1] + 0.12, x[1] + 0.12 + width, x[1] + 0.12),
                           gy = c(-1, gap, 1)),
                 aes(gx, gy, fill = group, color = group), alpha = 0.55, linewidth = 0.3) +
    geom_text(data = support_text, aes(x, y, label = text), size = 2.5, color = color_text) +
    geom_text(data = row_names, aes(0.45, y, label = text), size = 2.5, hjust = 1, color = color_text) +
    scale_color_manual(values = rain_colors) + scale_fill_manual(values = rain_colors) +
    scale_x_continuous(breaks = c(1, 2), labels = c("Non-friends", "Mutual friends")) +
    scale_y_continuous(breaks = seq(-1, 1, 0.25)) +
    coord_cartesian(ylim = c(-1, 1), clip = "off") +
    labs(x = NULL, y = "Social Rank GAP (sender - receiver)") +
    theme_si(9) +
    theme(axis.text.x = element_text(face = "bold", size = 11, color = color_text),
          plot.margin = margin(5, 5, 110, 60))
  figure_s21
}
