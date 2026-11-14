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
source(here("code", "si", "figures", "S9_figures.R"))   # plotting functions of this section
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


## ---- s9-figure-s29 ----
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

# Quick look: filtered Cooperation GAP along the behavioral rank gap
ggplot(bsr_bins, aes(gap, Filtered)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_point() +
  facet_wrap(~ friendship)

## ---- s9-figure-s29-si ----
# Supporting Information layout (plotting code in figures/S9_figures.R)
figure_s29 <- plot_figure_s29(bsr_bins, dyads)
figure_s29

## ---- s9-figure-s30 ----
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

# Quick look: Behavioral Social Rank against each characteristic
ggplot(bsr_correlates, aes(bsr, value)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap(~ panel)

## ---- s9-figure-s30-si ----
# Supporting Information layout (plotting code in figures/S9_figures.R)
figure_s30 <- plot_figure_s30(students, bsr_correlates, s30_labels, s30_titles)
figure_s30

## ---- s9-save ----
iwalk(table_s53, ~ save_table(.x, paste0("S9_table_s53_observed_", str_replace_all(tolower(.y), "[^a-z]+", "_"))))
iwalk(table_s54, ~ save_table(.x, paste0("S9_table_s54_filtered_", str_replace_all(tolower(.y), "[^a-z]+", "_"))))
iwalk(table_s55, ~ save_table(.x, paste0("S9_table_s55_asymmetric_", str_replace_all(tolower(.y), "[^a-z]+", "_"))))
write_csv(students %>% select(student_id, class_id, bsr_pagerank, bsr), here("data", "generated", "behavioral_rank.csv"))
save_si_figure(figure_s29, "S9_figure_s29_bsr_cooperation", 9, 5.5)
save_si_figure(figure_s30, "S9_figure_s30_bsr_correlates", 10, 6)
