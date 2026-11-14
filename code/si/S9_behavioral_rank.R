# =============================================================================
# S9_behavioral_rank.R   SI Section 9: Game-based Behavioral Social Rank
#
#   Behavioral Social Rank (BSR) = PageRank on the network of tokens sent in
#   the game, rescaled 0-1 within classroom.
#
#   Figure S29  Figure 3 panels B-C with the BSR gap
#   Figure S30  BSR and student characteristics
#   Tables S52-S54  Cooperation GAP on the BSR gap (observed, filtered,
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
# Tables S52-S53: three specifications (no FE; classroom FE; classroom +
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

table_s52 <- map(samples, ~ regression_table(bsr_models(.x, "coop_gap"), terms = "bsr_gap"))
table_s53 <- map(samples, ~ regression_table(bsr_models(.x, "coop_gap_filtered"), terms = "bsr_gap"))

table_s52
table_s53


## ---- s9-asymmetric ----
# Table S54: one-sided friendships (filtered tokens), split by who has the
# higher behavioral rank
asymmetric <- dyads %>%
  filter(friendship == "Sender declares", !is.na(coop_gap_filtered)) %>%
  mutate(direction = if_else(bsr_gap < 0, "Lower-to-higher", "Higher-to-lower"))

table_s54 <- asymmetric %>%
  split(.$direction) %>%
  map(~ regression_table(bsr_models(.x, "coop_gap_filtered"), terms = "bsr_gap"))
table_s54


## ---- s9-figure-s29 ----
# Figure 3 panels B and C using the behavioral rank gap: per-child
# averages, 7 equal-width bins (as in 05_figure3_cooperation.R)
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
  summarise(gap = mean(gap), sent = mean(sent, na.rm = TRUE), received = mean(received, na.rm = TRUE),
            Observed = mean(coop_observed), Filtered = mean(coop_filtered, na.rm = TRUE), .groups = "drop")

friend_colors <- c("Non-friends" = color_non_friends, Mutual = color_mutual, "Sender declares" = color_asymmetric)

row_tokens <- ggplot(bsr_bins, aes(gap, color = friendship)) +
  geom_ribbon(aes(ymin = received, ymax = sent), fill = "#F0E5A8", alpha = 0.6, color = NA) +
  geom_line(aes(y = sent)) + geom_point(aes(y = sent), shape = 18, size = 2.5) +
  geom_line(aes(y = received)) + geom_point(aes(y = received), shape = 15, size = 2) +
  facet_wrap(~ friendship) + scale_color_manual(values = friend_colors) +
  labs(x = "Behavioral Social Rank GAP", y = "Filtered tokens") +
  theme_paper(9) + theme(strip.background = element_blank())

row_gap <- bsr_bins %>%
  pivot_longer(c(Observed, Filtered), names_to = "version", values_to = "coop_gap") %>%
  ggplot(aes(gap, coop_gap, color = friendship, alpha = version)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() + geom_point() +
  facet_wrap(~ friendship) + scale_color_manual(values = friend_colors) +
  scale_alpha_manual(values = c(Observed = 0.4, Filtered = 1)) +
  labs(x = "Behavioral Social Rank GAP", y = "Cooperation GAP") +
  theme_paper(9) + theme(strip.background = element_blank())

figure_s29 <- row_tokens / row_gap
figure_s29


## ---- s9-figure-s30 ----
# BSR against other student characteristics (within-class z-scores)
nominations_received <- dyads %>%
  group_by(student_id = receiver_id) %>%
  summarise(friendship = sum(nom_q3), popular = sum(nom_q4),
            liked = sum(nom_q1), disliked = sum(nom_q2))

bsr_correlates <- students %>%
  left_join(nominations_received, by = "student_id") %>%
  group_by(class_id) %>%
  mutate(across(c(social_rank, friendship, popular, liked, disliked, attendance, gpa),
                ~ (.x - mean(.x)) / sd(.x))) %>%
  ungroup() %>%
  select(bsr, `Social Rank` = social_rank, `Friendship nominations` = friendship,
         `Popular nominations` = popular, `Liked (hang out)` = liked,
         `Disliked (not hang out)` = disliked, Attendance = attendance, GPA = gpa) %>%
  pivot_longer(-bsr)

bsr_correlates %>% group_by(name) %>% summarise(r = cor(bsr, value, use = "complete.obs"))

figure_s30 <- ggplot(bsr_correlates, aes(bsr, value)) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_smooth(method = "lm", color = "#C95A3A") +
  facet_wrap(~ name, ncol = 4, scales = "free_y") +
  labs(x = "Behavioral Social Rank", y = "Within-class z-score") +
  theme_paper(8) + theme(strip.background = element_blank())


## ---- s9-save ----
iwalk(table_s52, ~ save_table(.x, paste0("S9_table_s52_observed_", str_replace_all(tolower(.y), "[^a-z]+", "_"))))
iwalk(table_s53, ~ save_table(.x, paste0("S9_table_s53_filtered_", str_replace_all(tolower(.y), "[^a-z]+", "_"))))
iwalk(table_s54, ~ save_table(.x, paste0("S9_table_s54_asymmetric_", str_replace_all(tolower(.y), "[^a-z]+", "_"))))
write_csv(students %>% select(student_id, class_id, bsr_pagerank, bsr), here("data", "generated", "behavioral_rank.csv"))
save_si_figure(figure_s29, "S9_figure_s29_bsr_cooperation", 9, 6)
save_si_figure(figure_s30, "S9_figure_s30_bsr_correlates", 9, 4.5)
