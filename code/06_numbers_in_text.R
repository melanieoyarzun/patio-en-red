# =============================================================================
# 06_numbers_in_text.R
#
# Every number quoted in the main text (Results and Methods) that is not
# already in Table 1 or in a figure. Each block says where the number appears.
#
#   Input : data/generated/analysis_dyads.csv, data/generated/analysis_students.csv,
#           output/tables/table1.csv
#   Output: output/tables/numbers_in_text.csv
# =============================================================================

## ---- numbers-packages ----
library(tidyverse)
library(lfe)
library(igraph)
library(here)

dyads    <- read_csv(here("data", "generated", "analysis_dyads.csv"),    show_col_types = FALSE) %>%
  mutate(class_round = paste(class_id, round, sep = "_"))
students <- read_csv(here("data", "generated", "analysis_students.csv"), show_col_types = FALSE)
table1   <- read_csv(here("output", "tables", "table1.csv"),  show_col_types = FALSE)

numbers <- list()   # we collect everything here and save it at the end


## ---- numbers-sample ----
# Abstract, Results and Methods: who took part
numbers$children   <- nrow(students)
numbers$classrooms <- n_distinct(students$class_id)
numbers$schools    <- n_distinct(students$school_id)
numbers$decisions  <- nrow(dyads)
numbers$percent_female <- 100 * mean(students$female)
# two computed ages above 15 are birth-date errors and are left out (as in the SI)
valid_age <- students$age[!is.na(students$age) & students$age <= 15]
numbers$age_mean   <- mean(valid_age)
numbers$age_sd     <- sd(valid_age)

numbers[c("children", "classrooms", "schools", "decisions",
          "percent_female", "age_mean", "age_sd")]


## ---- numbers-design ----
# Methods, "Random assignment of dyadic partners"
decisions_per_child <- dyads %>% count(sender_id)
rounds_per_class    <- dyads %>% group_by(class_id) %>% summarise(rounds = n_distinct(round))

numbers$decisions_per_child_min  <- min(decisions_per_child$n)
numbers$decisions_per_child_max  <- max(decisions_per_child$n)
numbers$decisions_per_child_mean <- mean(decisions_per_child$n)
numbers$rounds_per_class_min     <- min(rounds_per_class$rounds)
numbers$rounds_per_class_max     <- max(rounds_per_class$rounds)
numbers$rounds_per_class_mean    <- mean(rounds_per_class$rounds)

# Each pair of classmates met only once: count unordered pairs and check that
# no pair appears in more than one round
pairs <- dyads %>%
  mutate(child_a = pmin(sender_id, receiver_id),
         child_b = pmax(sender_id, receiver_id)) %>%
  group_by(child_a, child_b) %>%
  summarise(decisions = n(), rounds = n_distinct(round), .groups = "drop")

numbers$unordered_pairs        <- nrow(pairs)
numbers$pairs_in_several_rounds <- sum(pairs$rounds > 1)
numbers$pairs_with_two_decisions <- sum(pairs$decisions == 2)

numbers[str_detect(names(numbers), "per_|pairs")]


## ---- numbers-friendship ----
# Results and Fig. 3 legend: share of each relationship type
friendship_shares <- dyads %>%
  mutate(type = if_else(friendship %in% c("Sender declares", "Receiver declares"),
                        "Asymmetric", friendship)) %>%
  count(type) %>%
  mutate(percent = 100 * n / sum(n))

friendship_shares

numbers$percent_non_friends <- friendship_shares$percent[friendship_shares$type == "Non-friends"]
numbers$percent_mutual      <- friendship_shares$percent[friendship_shares$type == "Mutual"]
numbers$percent_asymmetric  <- friendship_shares$percent[friendship_shares$type == "Asymmetric"]

# Results, "Friendship modulates...": spread of the rank gap in each group
numbers$sd_rank_gap_mutual      <- sd(dyads$rank_gap[dyads$friendship == "Mutual"])
numbers$sd_rank_gap_non_friends <- sd(dyads$rank_gap[dyads$friendship == "Non-friends"])


## ---- numbers-quartiles ----
# Results: "a top- versus bottom-quartile pairing, about 13% of interactions,
# implies roughly 1.8 tokens".
# Quartiles of Social Rank are computed over all senders and receivers.
quartile_cuts <- quantile(c(dyads$sender_rank, dyads$receiver_rank), c(0.25, 0.50, 0.75))

to_quartile <- function(rank) {
  case_when(rank <= quartile_cuts[1] ~ 1,
            rank <= quartile_cuts[2] ~ 2,
            rank <= quartile_cuts[3] ~ 3,
            TRUE ~ 4)
}

dyads <- dyads %>%
  mutate(sender_quartile   = to_quartile(sender_rank),
         receiver_quartile = to_quartile(receiver_rank))

beta_all <- table1$beta[table1$sample == "All dyads"]

top_to_bottom <- dyads %>% filter(sender_quartile == 4, receiver_quartile == 1)

# share of pairs between the top and the bottom quartile (in either direction),
# computed in each classroom and then averaged over the 45 classrooms
numbers$percent_top_vs_bottom_pairs <- dyads %>%
  group_by(class_id) %>%
  summarise(share = mean(abs(sender_quartile - receiver_quartile) == 3)) %>%
  summarise(100 * mean(share)) %>%
  pull()
numbers$predicted_coop_gap_top_to_bottom <- beta_all * mean(top_to_bottom$rank_gap)
numbers$observed_coop_gap_top_to_bottom  <- mean(top_to_bottom$coop_gap)


## ---- numbers-filtered ----
# Results and Methods: null-model filtered regressions (Fig. 3C, SI tables)
numbers$percent_transfers_kept <- 100 * mean(!is.na(dyads$tokens_sent_filtered))
numbers$percent_dyads_kept     <- 100 * mean(!is.na(dyads$coop_gap_filtered))

filtered_slope <- function(data) {
  model <- felm(coop_gap_filtered ~ rank_gap | sender_id + class_round | 0 | class_id,
                data = data %>% filter(!is.na(coop_gap_filtered)))
  summary(model)$coefficients["rank_gap", ]
}

filtered_all <- filtered_slope(dyads)
filtered_non <- filtered_slope(dyads %>% filter(friendship == "Non-friends"))
filtered_mut <- filtered_slope(dyads %>% filter(friendship == "Mutual"))

rbind(all = filtered_all, non_friends = filtered_non, mutual = filtered_mut)

numbers$filtered_slope_all         <- filtered_all[["Estimate"]]
numbers$filtered_slope_non_friends <- filtered_non[["Estimate"]]
numbers$filtered_p_non_friends     <- filtered_non[["Pr(>|t|)"]]
numbers$filtered_slope_mutual      <- filtered_mut[["Estimate"]]


## ---- numbers-asymmetric ----
# Results, "Directional effects in asymmetric friendships": one-sided pairs
asymmetric <- dyads %>% filter(friendship == "Sender declares")   # each pair once

numbers$percent_full_cooperation_asym <-
  100 * mean(asymmetric$tokens_sent == 10 & asymmetric$tokens_received == 10)
# "one student sent at least five tokens more than they received"
numbers$percent_strongly_asymmetric_asym <-
  100 * mean(abs(asymmetric$tokens_sent - asymmetric$tokens_received) >= 5)
# the blue region of the heatmap (one sent 10, the other 5 or less)
numbers$percent_blue_region_asym <-
  100 * mean(pmax(asymmetric$tokens_sent, asymmetric$tokens_received) == 10 &
             pmin(asymmetric$tokens_sent, asymmetric$tokens_received) <= 5)


## ---- numbers-behavioral-rank ----
# Discussion: Behavioral Social Rank = PageRank on the network of tokens sent
# in the game, rescaled 0-1 within class. Compared with the nomination-based
# Social Rank using a Spearman correlation.
behavioral_rank <- dyads %>%
  transmute(class_id, from = as.character(sender_id), to = as.character(receiver_id),
            weight = tokens_sent) %>%
  group_by(class_id) %>%
  group_modify(function(edges, key) {
    pr <- page_rank(graph_from_data_frame(edges, directed = TRUE))$vector
    tibble(student_id = as.numeric(names(pr)), behavioral_pagerank = pr)
  }) %>%
  mutate(behavioral_rank = (behavioral_pagerank - min(behavioral_pagerank)) /
                           (max(behavioral_pagerank) - min(behavioral_pagerank))) %>%
  ungroup() %>%
  left_join(students %>% select(student_id, social_rank), by = "student_id")

# Three versions of the correlation: (1) raw PageRank scores pooled over all
# classrooms; (2) the 0-1 ranks rescaled within each classroom, pooled; and
# (3) the median of the within-classroom correlations. Raw PageRank is
# roughly 1 / class size, so (1) also reflects differences in class size;
# (2) and (3) compare children only within their own classroom.
numbers$spearman_raw_pagerank_pooled <-
  cor(behavioral_rank$behavioral_pagerank,
      students$pagerank[match(behavioral_rank$student_id, students$student_id)],
      method = "spearman")
numbers$spearman_rescaled_ranks_pooled <-
  cor(behavioral_rank$behavioral_rank, behavioral_rank$social_rank, method = "spearman")
numbers$spearman_median_within_class <- behavioral_rank %>%
  group_by(class_id) %>%
  summarise(rho = cor(behavioral_rank, social_rank, method = "spearman")) %>%
  summarise(median(rho)) %>%
  pull()


## ---- numbers-quality ----
# Methods, response quality.
# (1) Children who named the same classmate as both "most popular" and
#     "not popular" (questions 4 and 5).
conflicting <- dyads %>% filter(nom_q4 == 1 & nom_q5 == 1)

numbers$conflicting_popularity_pairs    <- nrow(conflicting)                 # SI: 121
numbers$children_with_conflict          <- n_distinct(conflicting$sender_id) # SI: 70 of 859
numbers$classes_with_conflict           <- n_distinct(conflicting$class_id)  # SI: 30 of 45
# each conflict involves two nominations (one in Q4, one in Q5)
numbers$percent_of_popularity_nominations <- 100 * 2 * nrow(conflicting) /
  (sum(dyads$nom_q4) + sum(dyads$nom_q5))

# (2) Time between consecutive decisions of the same child (only classes
#     where the tablets recorded the time of each decision).
intervals <- dyads %>%
  filter(!is.na(seconds_since_start)) %>%
  arrange(sender_id, seconds_since_start) %>%
  group_by(sender_id) %>%
  mutate(seconds_since_previous = seconds_since_start - lag(seconds_since_start)) %>%
  ungroup()

numbers$median_seconds_between_decisions <- median(intervals$seconds_since_previous, na.rm = TRUE)
numbers$classes_with_timing <- n_distinct(intervals$class_id)


## ---- numbers-save ----
numbers_table <- tibble(quantity = names(numbers), value = unlist(numbers))
numbers_table %>% mutate(value = round(value, 3)) %>% print(n = Inf)

write_csv(numbers_table, here("output", "tables", "numbers_in_text.csv"))
