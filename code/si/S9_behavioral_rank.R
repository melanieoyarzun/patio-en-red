# =============================================================================
# S9_behavioral_rank.R   SI Section 9: Game-based Behavioral Social Rank
#
#   Behavioral Social Rank (BSR) = PageRank on the network of tokens sent in
#   the game, rescaled 0-1 within classroom.
#
#   Figure S29  Figure 3 panels B-C with the BSR gap
#   Figure S30  BSR and student characteristics
#   Tables S53-S55  Cooperation GAP on the BSR gap (observed, filtered,
#                   asymmetric friendships)
#
#   Input : data/generated/analysis_dyads.csv, data/generated/analysis_students.csv
#   Output: output/tables/si/S9_*.csv, output/figures/si/S9_*.(pdf|png)
# =============================================================================

## ---- s9-setup ----
source(here::here("code", "00_helpers.R"))
source(here("code", "00_style.R"))
library(igraph)
library(patchwork)
dir.create(here("output", "figures", "si"), recursive = TRUE, showWarnings = FALSE)

dyads    <- read_csv(here("data", "generated", "analysis_dyads.csv"),    show_col_types = FALSE)
students <- read_csv(here("data", "generated", "analysis_students.csv"), show_col_types = FALSE)

save_si_figure <- function(plot, name, width, height) {
  ggsave(here("output", "figures", "si", paste0(name, ".pdf")), plot, width = width, height = height)
  ggsave(here("output", "figures", "si", paste0(name, ".png")), plot, width = width, height = height, dpi = 300)
}


## ---- s9-bsr ----
# PageRank of each classroom's token network: edge i -> j weighted by the
# tokens i sent to j. A child ranks high when she receives many tokens,
# especially from classmates who themselves receive many.
behavioral_rank <- dyads %>%
  transmute(class_id, from = as.character(sender_id), to = as.character(receiver_id), weight = tokens_sent) %>%
  group_by(class_id) %>%
  group_modify(function(edges, key) {
    network <- graph_from_data_frame(edges %>% filter(weight > 0),
                                     vertices = data.frame(name = unique(c(edges$from, edges$to))))
    pr <- page_rank(network, damping = 0.85)$vector
    tibble(student_id = as.numeric(names(pr)), bsr_pagerank = pr)
  }) %>%
  mutate(bsr = (bsr_pagerank - min(bsr_pagerank)) / (max(bsr_pagerank) - min(bsr_pagerank))) %>%
  ungroup()

students <- students %>% left_join(behavioral_rank %>% select(student_id, bsr_pagerank, bsr), by = "student_id")

bsr_of <- setNames(students$bsr, students$student_id)
dyads <- dyads %>%
  mutate(bsr_gap = bsr_of[as.character(sender_id)] - bsr_of[as.character(receiver_id)])


## ---- s9-correlation ----
# How close are the behavioral and the nomination-based ranks?
# Raw PageRank scores pooled over classrooms, the 0-1 ranks rescaled within
# classrooms, and the median within-class correlation. Raw PageRank is about
# 1 / class size, so the pooled raw correlation also reflects class size; the
# rescaled ranks compare children only within their own classroom.
tibble(
  comparison = c("Raw PageRank, pooled", "Rescaled 0-1 ranks, pooled", "Rescaled ranks, median within class"),
  pearson  = c(cor(students$bsr_pagerank, students$pagerank),
               cor(students$bsr, students$social_rank),
               students %>% group_by(class_id) %>% summarise(r = cor(bsr, social_rank)) %>% pull(r) %>% median()),
  spearman = c(cor(students$bsr_pagerank, students$pagerank, method = "spearman"),
               cor(students$bsr, students$social_rank, method = "spearman"),
               students %>% group_by(class_id) %>% summarise(r = cor(bsr, social_rank, method = "spearman")) %>%
                 pull(r) %>% median())
)


## ---- s9-regressions ----
# Tables S53-S54: three specifications (no FE; classroom FE; classroom +
# sender FE) for all dyads, mutual friends and non-friends
bsr_models <- function(data, outcome) {
  data <- data %>% filter(!is.na(.data[[outcome]])) %>%
    mutate(across(c(sender_id, class_id), as.factor))
  f <- function(fe, cl) as.formula(paste(outcome, "~ bsr_gap |", fe, "| 0 |", cl))
  list("(1)" = felm(f("0", "0"), data = data),
       "(2)" = felm(f("class_id", "class_id"), data = data),
       "(3)" = felm(f("class_id + sender_id", "class_id"), data = data))
}

samples <- list("All dyads" = dyads,
                "Mutual friends" = filter(dyads, friendship == "Mutual"),
                "Non-friends" = filter(dyads, friendship == "Non-friends"))

table_s53 <- map(samples, ~ regression_table(bsr_models(.x, "coop_gap"), terms = "bsr_gap"))
table_s54 <- map(samples, ~ regression_table(bsr_models(.x, "coop_gap_filtered"), terms = "bsr_gap"))



## ---- s9-asymmetric ----
# Table S55: one-sided friendships (filtered tokens), split by who has the
# higher behavioral rank
asymmetric <- dyads %>%
  filter(friendship == "Sender declares", !is.na(coop_gap_filtered)) %>%
  mutate(direction = if_else(bsr_gap < 0, "Lower-to-higher", "Higher-to-lower"))

table_s55 <- asymmetric %>%
  split(.$direction) %>%
  map(~ regression_table(bsr_models(.x, "coop_gap_filtered"), terms = "bsr_gap"))


## ---- s9-figure-s29-data ----
# Figure 3 panels B and C with the behavioral rank gap: per-child averages,
# 7 equal-width bins (as in 05_figure3_cooperation.R), 95% CIs across children
mean_ci <- function(x) c(mean = mean(x, na.rm = TRUE),
                         ci = 1.96 * sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))

bsr_bins <- dyads %>%
  filter(friendship %in% c("Non-friends", "Mutual", "Sender declares")) %>%
  group_by(friendship, sender_id) %>%
  summarise(gap = median(bsr_gap),
            sent = mean(tokens_sent_filtered, na.rm = TRUE),
            received = mean(tokens_received_filtered, na.rm = TRUE),
            coop_observed = mean(coop_gap),
            coop_filtered = mean(coop_gap_filtered, na.rm = TRUE), .groups = "drop") %>%
  group_by(friendship) %>% mutate(bin = cut_interval(gap, 7)) %>%
  group_by(friendship, bin) %>%
  # (the CIs are computed first: summarise() replaces each column as it goes)
  summarise(gap = mean(gap),
            sent_ci     = mean_ci(sent)[["ci"]],          sent     = mean_ci(sent)[["mean"]],
            received_ci = mean_ci(received)[["ci"]],      received = mean_ci(received)[["mean"]],
            Observed_ci = mean_ci(coop_observed)[["ci"]], Observed = mean_ci(coop_observed)[["mean"]],
            Filtered_ci = mean_ci(coop_filtered)[["ci"]], Filtered = mean_ci(coop_filtered)[["mean"]],
            .groups = "drop")

## ---- s9-figure-s29-panels ----
# Figure S29: Figure 3 panels B and C with the Behavioral Social Rank GAP,
# one column per friendship type. The three columns share one drawing,
# written as a function.
s29_groups <- tribble(
  ~friendship,       ~title,        ~color,
  "Non-friends",     "Non Friends", color_non_friends,
  "Mutual",          "Mutual",      color_mutual,
  "Sender declares", "Ego declares", color_asymmetric
)

s29_theme <- theme_si_frame(7) + theme(axis.text = element_text(size = 6))
rank_side_labels <- annotate("text", x = c(-0.55, 0.55), y = -Inf, vjust = -0.6, size = 2,
                             fontface = "bold", label = c("Receiver has higher rank", "Sender has higher rank"))

s29_column <- function(friendship, title, color) {
  bins <- bsr_bins %>% filter(friendship == !!friendship)
  header <- header_bar(title, color)

  first <- bins %>% slice_min(gap, n = 1)
  last  <- bins %>% filter(!is.na(Filtered)) %>% slice_max(gap, n = 1)
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
    # "Sent" above and "Received" below the first bin
    annotate("text", x = first$gap + 0.05, size = 2, fontface = "bold", color = color,
             y = c(max(first$sent, first$received) + 1.4, min(first$sent, first$received) - 1.3),
             label = c("Sent", "Received")) +
    rank_side_labels +
    scale_x_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, 0.5)) +
    scale_y_continuous(breaks = c(0, 5, 10)) +
    coord_cartesian(ylim = c(-0.5, 11.5)) +
    labs(x = NULL, y = if (friendship == "Non-friends") "Tokens" else NULL) +
    s29_theme

  gaps <- bins %>%
    pivot_longer(c(Observed, Filtered), names_to = "version", values_to = "coop_gap") %>%
    mutate(ci = if_else(version == "Observed", Observed_ci, Filtered_ci)) %>%
    ggplot(aes(gap, coop_gap, alpha = version)) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
    geom_line(color = color, linewidth = 0.9) +
    geom_errorbar(aes(ymin = coop_gap - ci, ymax = coop_gap + ci), color = color, width = 0.03) +
    geom_point(color = color, size = 2) +
    annotate("text", x = 1.05, y = -4.4, hjust = 1, size = 2, fontface = "bold", color = color,
             label = "Non random") +
    annotate("text", x = 1.05, y = -5.1, hjust = 1, size = 2, fontface = "bold", color = color,
             alpha = 0.4, label = "Observed") +
    # "Cooperation GAP" above the last filtered point
    annotate("text", x = min(last$gap, 0.85), y = last$Filtered + 1.6, size = 2, fontface = "bold",
             color = color, label = "Cooperation\nGAP", lineheight = 0.9) +
    rank_side_labels +
    scale_alpha_manual(values = c(Filtered = 1, Observed = 0.4)) +
    scale_x_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, 0.5)) +
    scale_y_continuous(breaks = seq(-5, 5, 2.5)) +
    coord_cartesian(ylim = c(-6.5, 6)) +
    labs(x = NULL, y = if (friendship == "Non-friends") "GAP [Received - Sent]" else NULL) +
    s29_theme

  header / tokens / header / gaps + plot_layout(heights = c(0.12, 1, 0.12, 1))
}

## ---- s9-figure-s29 ----
figure_s29 <- pmap(s29_groups, s29_column) %>%
  wrap_plots(nrow = 1) +
  plot_annotation(caption = "Behavioral Social Ranking GAP [game PageRank sender - receiver]",
                  theme = theme(plot.caption = element_text(hjust = 0.5, size = 9)))
figure_s29

## ---- s9-figure-s30-data ----
# Figure S30: (A) the two PageRank scores, log scales; (B-H) Behavioral Social
# Rank against other characteristics (within-class z-scores), with binned
# means, a linear fit and the pooled Pearson correlation
s30_titles <- c(friendship = "B. Friendship nominations", attendance = "C. Attendance previous term",
                gpa = "D. Overall GPA previous term", liked = "E. Liked", disliked = "F. Disliked",
                aggression = "G. Aggression", popular = "H. Popularity")

# within-classroom z-scores from 01_build_measures.R (step "student-covariates")
bsr_correlates <- students %>%
  transmute(bsr, friendship = received_q3_z, attendance = attendance_z, gpa = gpa_z,
            liked = received_q1_z, disliked = received_q2_z,
            aggression = aggression_total_z, popular = received_q4_z) %>%
  select(bsr, all_of(names(s30_titles))) %>%
  pivot_longer(-bsr) %>%
  filter(!is.na(value)) %>%
  mutate(panel = factor(s30_titles[name], levels = s30_titles))

s30_labels <- bsr_correlates %>%
  group_by(panel) %>%
  summarise(test = list(cor.test(bsr, value))) %>%
  mutate(label = map_chr(test, ~ sprintf("R = %.2f, %s", .x$estimate, format_p(.x$p.value))))

bsr_correlates %>% group_by(panel) %>% summarise(r = cor(bsr, value))

## ---- s9-figure-s30-panels ----
# Figure S30: (A) the two PageRank scores on log scales; (B-H) one panel per
# characteristic, with binned means, a linear fit and the pooled Pearson
# correlation. The seven panels share one drawing, written as a function.
# SI look: centered titles, the panel letter at the top left, dark frame
s30_theme <- theme_si_frame(7) +
  theme(plot.title = element_text(size = 11, lineheight = 0.95),
        plot.tag = element_text(size = 8), plot.tag.position = c(0.02, 0.98),
        axis.text = element_text(size = 6))
split_title <- function(title) {
  list(tag = sub("\\..*", "", title),
       title = str_wrap(sub("^[A-Z]\\.\\s*", "", title), 22))
}

correlate_panel <- function(this_panel, y_title = NULL) {
  data <- bsr_correlates %>% filter(panel == this_panel)
  parts <- split_title(this_panel)
  ggplot(data, aes(bsr, value)) +
    geom_point(color = color_slate, alpha = 0.3, size = 0.5,
               position = position_jitter(width = 0.005, height = 0.05, seed = 1)) +
    geom_smooth(method = "lm", formula = y ~ x, color = color_red, fill = color_red,
                alpha = 0.15, linewidth = 0.6) +
    stat_summary_bin(fun.data = mean_cl_normal, bins = 10, size = 0.15, linewidth = 0.3) +
    geom_label(data = filter(s30_labels, panel == this_panel), aes(x = 0.5, y = -2.6, label = label),
               color = color_red, size = 2.6, label.size = 0.3) +
    coord_cartesian(ylim = c(-3.1, 3.1)) +
    scale_y_continuous(breaks = -3:3) +
    labs(title = parts$title, tag = parts$tag, x = NULL, y = y_title) +
    s30_theme
}

pagerank_test <- cor.test(log(students$bsr_pagerank), log(students$pagerank))
panel_a <- ggplot(students, aes(bsr_pagerank, pagerank)) +
  geom_point(color = color_slate, alpha = 0.35, size = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, color = color_red, se = FALSE, linewidth = 0.6,
              fullrange = TRUE) +
  stat_summary_bin(fun.data = mean_cl_normal, bins = 10, size = 0.15, linewidth = 0.3) +
  annotate("label", x = 0.06, y = 0.012, size = 2.6, color = color_red, label.size = 0.3,
           label = sprintf("R = %.2f, %s", pagerank_test$estimate, format_p(pagerank_test$p.value))) +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Social Rank Measures\nComparison (Log Scale)", tag = "A",
       x = "Behavioral Social Rank\n(Social Dilemma Game PageRank)",
       y = "Popularity nomination\nPageRank") +
  s30_theme

## ---- s9-figure-s30 ----
top_row <- c(list(panel_a), map2(s30_titles[1:3], list("Z-score", NULL, NULL), correlate_panel))
bottom_row <- map2(s30_titles[4:7], list("Peer nominations (z-score)", NULL, NULL, NULL), correlate_panel)
figure_s30 <- wrap_plots(c(top_row, bottom_row), nrow = 2) +
  plot_annotation(caption = "Behavioral Social Rank (normalized Social Dilemma Game PageRank)",
                  theme = theme(plot.caption = element_text(hjust = 0.5, size = 9)))
figure_s30

## ---- s9-save ----
iwalk(table_s53, ~ save_table(.x, paste0("S9_table_s53_observed_", str_replace_all(tolower(.y), "[^a-z]+", "_"))))
iwalk(table_s54, ~ save_table(.x, paste0("S9_table_s54_filtered_", str_replace_all(tolower(.y), "[^a-z]+", "_"))))
iwalk(table_s55, ~ save_table(.x, paste0("S9_table_s55_asymmetric_", str_replace_all(tolower(.y), "[^a-z]+", "_"))))
write_csv(students %>% select(student_id, class_id, bsr_pagerank, bsr), here("data", "generated", "behavioral_rank.csv"))
save_si_figure(figure_s29, "S9_figure_s29_bsr_cooperation", 9, 5.5)
save_si_figure(figure_s30, "S9_figure_s30_bsr_correlates", 10, 6)
