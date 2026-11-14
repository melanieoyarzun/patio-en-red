# =============================================================================
# S4_figures.R
#
# Figures of Supporting Information, Section 4 (Social Rank), with the SI layout, colors, panel titles and annotations.
#
# The analysis script code/si/S4_*.R computes the data and calls these
# functions. The notebooks show a quick version of each figure and keep this
# code in a collapsed box.
# =============================================================================

## ---- plot-figure-s6 ----
# Figure S6: pooled Spearman correlations between every pair of constructions,
# with the primary Social Rank in the framed top row.
plot_figure_s6 <- function(alternative_ranks) {
  # Figure S6: pooled Spearman correlations between every pair of constructions
  # (lower triangle), with the primary Social Rank in the framed top row
  matrix_labels <- c(social_rank          = "Pop. Social Rank\n(Affine transform)",
                     rank_pn_centrality   = "Signed PN\ncentrality",
                     rank_net_nominations = "Net\nnominations",
                     rank_pr_minus_neg_in = "Pos. PageRank\nminus neg.\nindegree",
                     rank_two_layer       = "Two-layer\nPageRank",
                     rank_positive_only   = "Positive-only\nPageRank")

  correlations <- cor(alternative_ranks[, names(matrix_labels)], method = "spearman")
  correlation_cells <- as_tibble(correlations, rownames = "row") %>%
    pivot_longer(-row, names_to = "column", values_to = "rho") %>%
    mutate(row_i = match(row, names(matrix_labels)),
           col_i = match(column, names(matrix_labels))) %>%
    filter(col_i >= row_i) %>%                       # upper triangle as in the SI
    mutate(row = factor(matrix_labels[row], levels = rev(matrix_labels)),
           column = factor(matrix_labels[column], levels = matrix_labels))

  figure_s6 <- ggplot(correlation_cells, aes(column, row, fill = rho)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%.2f", rho), fontface = if_else(rho == 1, "bold", "plain"),
                  color = if_else(rho > 0.95, "white", "grey15")), size = 3.6) +
    annotate("rect", xmin = 0.5, xmax = 6.5, ymin = 5.5, ymax = 6.5,
             fill = NA, color = "grey20", linewidth = 1.2) +
    scale_fill_gradientn(colors = c("#F7F4EC", "#F2D98C", "#E39A55", "#C95A3A", "#B03A2E"),
                         limits = c(0, 1), name = "Spearman correlation") +
    scale_color_identity() +
    labs(title = "Popularity-rank alternatives", x = NULL, y = NULL) +
    theme_minimal(base_size = 9) +
    theme(panel.grid = element_blank(), plot.title = element_text(face = "bold"),
          legend.key.height = unit(1.6, "cm")) +
    guides(fill = guide_colorbar(title.position = "right",
                                 title.theme = element_text(angle = 90, size = 8)))
  figure_s6
}


## ---- plot-figure-s7 ----
# Figure S7: (A) ranks of one classroom as the weight w of non-nominations
# changes; (B) agreement with the primary order in that classroom; (C) all
# classrooms, median in orange.
plot_figure_s7 <- function(fine_ranks, agreement_by_weight, example_class) {
  primary_line <- geom_vline(xintercept = 1, linetype = "dashed", color = "grey40")

  panel_a <- fine_ranks %>%
    filter(class_id == example_class) %>%
    ggplot(aes(w, rank, group = student_id, color = social_rank)) +
    geom_line(linewidth = 0.45) +
    primary_line +
    annotate("text", x = 1, y = 1.06, label = "primary (w = 1)", size = 2.6, color = color_text) +
    scale_color_gradient(low = "#C9D0DE", high = "#2E4A6B") +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    labs(title = "A. One classroom: Social Rank vs. w (n = 18)",
         x = "Reference weight for non-nominations (w)",
         y = "Social Rank (normalized PageRank)") +
    theme_si(base_size = 8)

  panel_b <- agreement_by_weight %>%
    filter(class_id == example_class) %>%
    pivot_longer(c(spearman, pairs), names_to = "measure") %>%
    mutate(measure = factor(measure, levels = c("spearman", "pairs"),
                            labels = c("Spearman rank correlation", "Pairwise orderings preserved"))) %>%
    ggplot(aes(w, value, color = measure, linetype = measure)) +
    geom_line(linewidth = 0.8) +
    primary_line +
    scale_color_manual(values = c(color_highlight, color_slate), name = NULL) +
    scale_linetype_manual(values = c("solid", "dotted"), name = NULL) +
    scale_y_continuous(limits = c(0.5, 1.01), breaks = c(0.5, 0.75, 1)) +
    labs(title = "B. One classroom: order is preserved",
         x = "Reference weight for non-nominations (w)", y = "Agreement with primary order") +
    theme_si(base_size = 8) +
    theme(legend.position = c(0.5, 0.15), legend.key.width = unit(0.8, "cm"))

  band <- agreement_by_weight %>%
    group_by(w) %>%
    summarise(median = median(spearman), low = quantile(spearman, 0.1), high = quantile(spearman, 0.9))

  panel_c <- ggplot() +
    geom_line(data = agreement_by_weight, aes(w, spearman, group = class_id),
              color = "#C9D0DE", linewidth = 0.25) +
    geom_ribbon(data = band, aes(w, ymin = low, ymax = high), fill = color_highlight, alpha = 0.2) +
    geom_line(data = band, aes(w, median), color = color_highlight, linewidth = 1) +
    primary_line +
    scale_y_continuous(limits = c(0.5, 1.01), breaks = c(0.5, 0.75, 1)) +
    labs(title = "C. All classrooms (k = 45)",
         subtitle = "Median (orange) and 10-90th percentile; gray: each classroom",
         x = "Reference weight for non-nominations (w)", y = "Spearman vs. primary order") +
    theme_si(base_size = 8)

  figure_s7 <- panel_a + panel_b + panel_c
  figure_s7
}


## ---- plot-figure-s8 ----
# Figure S8: Social Rank against each student characteristic (gray: children;
# black: binned means with 95% CI; orange: linear fit and Pearson correlation),
# and box plots for the categorical characteristics.
plot_figure_s8 <- function(correlates, students) {
  s8_titles <- c(
    nom_q3 = "A. Best friends", nom_q1 = "B. Most like to hang out",
    nom_q2 = "C. Least like to hang out", net_popularity = "D. Net popularity",
    nom_q4 = "E. Most popular", nom_q5 = "F. Not popular",
    nom_q6 = "G. Initiate fights", nom_q7 = "H. Make fun of others",
    nom_q8 = "I. Kind to others", nom_q9 = "J. Cooperative",
    aggression = "K. Aggression (composite)", prosociality = "L. Prosociality (composite)",
    attendance = "M. Attendance previous term", gpa = "N. Overall GPA previous term",
    age = "O. Age (within-class)"
  )

  scatter_data <- correlates %>%
    select(social_rank, all_of(names(s8_titles))) %>%
    pivot_longer(-social_rank, names_to = "variable", values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(panel = factor(s8_titles[variable], levels = s8_titles))

  scatter_labels <- scatter_data %>%
    group_by(panel) %>%
    summarise(test = list(cor.test(social_rank, value))) %>%
    mutate(label = map_chr(test, ~ sprintf("R = %.2f, %s", .x$estimate, format_p(.x$p.value))))

  scatter_panels <- ggplot(scatter_data, aes(social_rank, value)) +
    geom_jitter(color = color_slate, alpha = 0.25, size = 0.4, width = 0.005, height = 0.05) +
    geom_smooth(method = "lm", color = color_highlight, fill = color_highlight,
                alpha = 0.2, linewidth = 0.6) +
    stat_summary_bin(fun.data = mean_cl_normal, bins = 10, size = 0.15, linewidth = 0.3) +
    geom_label(data = scatter_labels, aes(x = 0.5, y = -2.75, label = label),
               color = color_highlight, size = 2.1, fontface = "bold",
               label.size = 0.3, label.padding = unit(0.12, "lines")) +
    facet_wrap(~ panel, ncol = 4) +
    coord_cartesian(ylim = c(-3, 3)) +
    scale_x_continuous(breaks = seq(0, 1, 0.2)) +
    labs(x = NULL, y = "Peer nominations / characteristic (z-score)") +
    theme_si(base_size = 7)

  # Categorical characteristics: box plots with a classroom-clustered joint
  # test and partial R2 conditional on classroom fixed effects.
  box_panel <- function(data, group, title) {
    data <- data %>%
      filter(!is.na(.data[[group]])) %>%
      mutate(group = .data[[group]], class_id = factor(class_id))
    fit <- felm(social_rank ~ group | class_id | 0 | class_id, data = data)
    classroom_only <- felm(social_rank ~ 1 | class_id, data = data)
    group_p <- lfe::waldtest(fit, seq_along(coef(fit)), type = "cluster")[["p.F"]]
    partial_r2 <- 1 - sum(residuals(fit)^2) / sum(residuals(classroom_only)^2)
    counts <- data %>% count(group) %>%
      mutate(label = paste0(group, " (n=", n, ")"), label = factor(label, levels = label))
    data <- data %>% left_join(counts, by = "group")
    ggplot(data, aes(social_rank, label)) +
      geom_jitter(color = color_slate, alpha = 0.25, size = 0.4, height = 0.15) +
      geom_boxplot(fill = color_slate_light, color = "grey25", alpha = 0.8, width = 0.45,
                   outlier.shape = NA, linewidth = 0.3) +
      annotate("label", x = 0.5, y = 0.45,
               label = sprintf("group p = %.2f,  partial R2 = %.3f", group_p, partial_r2),
               color = color_highlight, size = 2.1, fontface = "bold", label.size = 0.3) +
      scale_y_discrete(limits = rev, expand = expansion(add = c(0.9, 0.5))) +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      labs(title = title, x = NULL, y = NULL) +
      theme_si(base_size = 7) +
      theme(panel.grid.major.y = element_blank())
  }

  categorical <- students %>%
    mutate(sex = if_else(female == 1, "Female", "Male"),
           guardian = factor(guardian_education,
                             levels = sort(unique(guardian_education)),
                             labels = str_remove(sort(unique(guardian_education)), "^[0-9]+ ")),
           repetition = if_else(repeated_grade == 1, "Repeated", "Not repeated"))

  box_panels <- box_panel(categorical, "sex", "P. Sex") +
    box_panel(categorical, "guardian", "Q. Guardian education") +
    box_panel(categorical, "repetition", "S. Grade repetition") +
    plot_layout(widths = c(1, 1.4, 1))

  figure_s8 <- (scatter_panels / box_panels) +
    plot_layout(heights = c(4, 1)) +
    plot_annotation(title = "Popularity-based Social Rank and related student characteristics",
                    caption = "Popularity-based Social Rank (normalized PageRank)",
                    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5),
                                  plot.caption = element_text(hjust = 0.5, size = 9)))
  figure_s8
}
