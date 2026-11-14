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
    geom_text(aes(label = sprintf("%.2f", rho), fontface = if_else(rho == 1, "bold", "plain")),
              color = "white", size = 3.6) +
    annotate("rect", xmin = 0.5, xmax = 6.5, ymin = 5.5, ymax = 6.5,
             fill = NA, color = color_highlight, linewidth = 1.2) +
    scale_fill_gradientn(colors = c("#F5F6F7", "#A7C0CF", "#2E6A8E"), limits = c(0, 1),
                         name = "Spearman correlation") +
    coord_fixed(expand = FALSE) +
    labs(title = "Popularity-rank alternatives", x = NULL, y = NULL) +
    theme_minimal(base_size = 9) +
    theme(panel.grid = element_blank(), plot.title = element_text(face = "bold"),
          axis.line = element_line(color = "grey75", linewidth = 0.3),
          legend.key.height = unit(1.9, "cm"), legend.key.width = unit(0.35, "cm")) +
    guides(fill = guide_colorbar(title.position = "right", frame.colour = "grey75",
                                 title.theme = element_text(angle = 90, size = 8)))
  figure_s6
}


## ---- plot-figure-s7 ----
# Figure S7: (A) ranks of one classroom as the weight w of non-nominations
# changes; (B) agreement with the primary order in that classroom; (C) all
# classrooms, median in orange.
plot_figure_s7 <- function(fine_ranks, agreement_by_weight, example_class) {
  primary_line <- geom_vline(xintercept = 1, linetype = "dashed", color = "grey20", linewidth = 0.4)
  w_label <- expression("Reference weight for non-nominations (" * italic(w) * ")")
  theme_s7 <- theme_si_frame(base_size = 8, l_axes = TRUE) +
    theme(plot.title = element_text(hjust = 0), plot.title.position = "panel")
  x_axis <- scale_x_continuous(limits = c(0, 2), breaks = c(0.5, 1, 1.5, 2), expand = c(0, 0))
  y_agreement <- scale_y_continuous(limits = c(0.5, 1.01), breaks = c(0.5, 0.75, 1),
                                    labels = c("0.5", "0.75", "1"), expand = c(0, 0))
  example <- fine_ranks %>% filter(class_id == example_class)
  n_example <- n_distinct(example$student_id)

  panel_a <- ggplot(example, aes(w, rank, group = student_id, color = social_rank)) +
    geom_line(linewidth = 0.6) +
    primary_line +
    annotate("text", x = 1, y = 1.04, label = "primary~(italic(w) == 1)", parse = TRUE,
             size = 2.6, color = color_text) +
    scale_color_gradient(low = "#C9D0DE", high = "#2E4A6B") +
    scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
    x_axis +
    labs(title = bquote(bold("A. One classroom: Social Rank vs.") ~ italic(w) ~ (italic(n) == .(n_example))),
         x = w_label, y = "Social Rank (normalized PageRank)") +
    theme_s7

  panel_b <- agreement_by_weight %>%
    filter(class_id == example_class) %>%
    pivot_longer(c(spearman, pairs), names_to = "measure") %>%
    mutate(measure = factor(measure, levels = c("spearman", "pairs"),
                            labels = c("Spearman rank correlation", "Pairwise orderings preserved"))) %>%
    ggplot(aes(w, value, color = measure, linetype = measure)) +
    geom_line(linewidth = 0.9) +
    primary_line +
    scale_color_manual(values = c(color_highlight, "#7A86A8"), name = NULL) +
    scale_linetype_manual(values = c("solid", "dotted"), name = NULL) +
    x_axis + y_agreement +
    labs(title = "B. One classroom: order is preserved", x = w_label,
         y = "Agreement with primary order") +
    theme_s7 +
    theme(legend.position = c(0.55, 0.12), legend.key.width = unit(0.7, "cm"),
          legend.key.height = unit(0.3, "cm"), legend.spacing.y = unit(0, "pt"),
          legend.text = element_text(size = 6.5))

  band <- agreement_by_weight %>%
    group_by(w) %>%
    summarise(median = median(spearman), low = quantile(spearman, 0.1), high = quantile(spearman, 0.9))
  legend_c <- c("Median across classrooms", "10\u201390th percentile", "Individual classrooms")

  panel_c <- ggplot() +
    geom_line(data = agreement_by_weight, aes(w, spearman, group = class_id, linetype = legend_c[3]),
              color = "#C9D0DE", linewidth = 0.25) +
    geom_ribbon(data = band, aes(w, ymin = low, ymax = high, fill = legend_c[2]), alpha = 0.25) +
    geom_line(data = band, aes(w, median, color = legend_c[1]), linewidth = 1) +
    primary_line +
    scale_color_manual(values = setNames(color_highlight, legend_c[1]), name = NULL) +
    scale_fill_manual(values = setNames(color_highlight, legend_c[2]), name = NULL) +
    scale_linetype_manual(values = setNames("solid", legend_c[3]), name = NULL) +
    x_axis + y_agreement +
    labs(title = bquote(bold("C. All classrooms") ~ (italic(k) == .(n_distinct(agreement_by_weight$class_id)))),
         x = w_label, y = "Spearman vs. primary order") +
    guides(color = guide_legend(order = 1), fill = guide_legend(order = 2),
           linetype = guide_legend(order = 3, override.aes = list(color = "#C9D0DE", linewidth = 0.5))) +
    theme_s7 +
    theme(legend.position = c(0.55, 0.14), legend.spacing.y = unit(0, "pt"),
          legend.margin = margin(0, 2, 0, 2), legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.7, "cm"), legend.text = element_text(size = 6.5),
          legend.background = element_rect(fill = alpha("white", 0.85), color = NA))

  figure_s7 <- (panel_a + panel_b + panel_c) +
    figure_title(paste("Reference-weight robustness of popularity-based Social Rank:",
                       "the within-class ordering is preserved"), size = 10)
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
  theme_s8 <- theme_si_frame(base_size = 7) +
    theme(axis.title.y = element_text(size = 6))
  x_rank <- scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

  # One scatter panel: children (gray), binned means with 95% CI (black),
  # linear fit and Pearson correlation (orange)
  scatter_panel <- function(variable, y_title = NULL) {
    data <- correlates %>%
      transmute(social_rank, value = .data[[variable]]) %>%
      filter(!is.na(value))
    test <- cor.test(data$social_rank, data$value)
    label <- sprintf("R = %.2f, %s", test$estimate, format_p(test$p.value))
    ggplot(data, aes(social_rank, value)) +
      geom_point(color = color_slate, alpha = 0.25, size = 0.4,
                 position = position_jitter(width = 0.005, height = 0.05, seed = 1)) +
      geom_smooth(method = "lm", formula = y ~ x, color = color_highlight, fill = color_highlight,
                  alpha = 0.2, linewidth = 0.6) +
      stat_summary_bin(fun.data = mean_cl_normal, bins = 10, size = 0.15, linewidth = 0.3) +
      annotate("label", x = 0.5, y = -2.7, label = label, color = color_highlight, size = 2.1,
               fontface = "bold", label.size = 0.3, label.padding = unit(0.12, "lines")) +
      coord_cartesian(ylim = c(-3, 3.2)) +
      scale_y_continuous(breaks = -3:3) +
      x_rank +
      labs(title = s8_titles[[variable]], x = NULL, y = y_title) +
      theme_s8
  }

  # One box-plot panel for a categorical characteristic, group labels above
  # the boxes, a classroom-clustered joint test, and partial R2.
  box_panel <- function(data, group, title, fills, y_title = NULL) {
    data <- data %>%
      filter(!is.na(.data[[group]])) %>%
      mutate(group = .data[[group]], class_id = factor(class_id))
    fit <- felm(social_rank ~ group | class_id | 0 | class_id, data = data)
    classroom_only <- felm(social_rank ~ 1 | class_id, data = data)
    group_p <- lfe::waldtest(fit, seq_along(coef(fit)), type = "cluster")[["p.F"]]
    partial_r2 <- 1 - sum(residuals(fit)^2) / sum(residuals(classroom_only)^2)
    counts <- data %>% count(group) %>%
      mutate(y = rev(seq_along(group)), label = paste0(group, " (n=", n, ")"))
    data <- data %>% left_join(counts, by = "group")
    stats <- sprintf("'group p = %.2f,  partial '*italic(R)^2*' = %.3f'", group_p, partial_r2)
    ggplot(data, aes(social_rank, y)) +
      geom_point(color = color_slate, alpha = 0.25, size = 0.4,
                 position = position_jitter(height = 0.12, seed = 1)) +
      geom_boxplot(aes(group = y, fill = group), color = "grey25", alpha = 0.75, width = 0.45,
                   outlier.shape = NA, linewidth = 0.3, coef = Inf) +
      geom_text(data = counts, aes(x = 0.5, y = y + 0.42, label = label),
                fontface = "bold", size = 2.2, color = "grey20") +
      annotate("label", x = 0.5, y = 0.35, label = stats, parse = TRUE,
               color = color_highlight, size = 2.1, fontface = "bold", label.size = 0.3) +
      scale_fill_manual(values = setNames(fills, counts$group)) +
      scale_y_continuous(limits = c(0.1, nrow(counts) + 0.6), breaks = NULL) +
      x_rank +
      labs(title = title, x = NULL, y = y_title) +
      theme_s8
  }
  blues <- function(n) colorRampPalette(c("#DCE6F0", "#4E6E8E"))(n)

  categorical <- students %>%
    mutate(sex = factor(if_else(female == 1, "Female", "Male"), levels = c("Male", "Female")),
           # the two primary categories are merged into "Primary or less", as in the SI
           guardian = str_remove(guardian_education, "^[0-9]+ "),
           guardian = if_else(str_detect(guardian, "^Primary"), "Primary or less", guardian),
           guardian = factor(guardian, levels = unique(guardian[order(guardian_education)])),
           repetition = factor(if_else(repeated_grade == 1, "Repeated", "Not repeated"),
                               levels = c("Not repeated", "Repeated")))
  n_guardian <- n_distinct(na.omit(categorical$guardian))

  y_titles <- c(nom_q3 = "Peer nominations (Z-score)", nom_q4 = "Peer nominations (Z-score)",
                nom_q8 = "Peer nominations (Z-score)", attendance = "Z-score")
  grid_panels <- map(names(s8_titles), function(v) {
    scatter_panel(v, if (v %in% names(y_titles)) y_titles[[v]] else NULL)
  })
  sex_panel <- box_panel(categorical, "sex", "P. Sex", c(color_male, color_female), "Sex")

  # Religion (Panel R in the SI) is not part of the public data
  bottom_row <- box_panel(categorical, "guardian", "Q. Guardian education", blues(n_guardian)) +
    box_panel(categorical, "repetition", "S. Grade repetition", blues(3)[2:3]) +
    plot_layout(widths = c(2.6, 1))

  figure_s8 <- (wrap_plots(c(grid_panels, list(sex_panel)), ncol = 4) / bottom_row) +
    plot_layout(heights = c(4, 1.25)) +
    plot_annotation(title = "Popularity-based Social Rank and related student characteristics",
                    caption = "Popularity-based Social Rank (normalized PageRank)",
                    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
                                  plot.caption = element_text(hjust = 0.5, size = 9)))
  figure_s8
}
