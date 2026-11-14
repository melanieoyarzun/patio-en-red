# =============================================================================
# 01_build_measures.R
#
# From the raw game decisions and peer nominations to the variables used in
# every table and figure of the paper.
#
#   Input : data/raw/dyads.csv, data/raw/students.csv
#   Output: data/generated/analysis_dyads.csv     (one row per directed decision)
#           data/generated/analysis_students.csv  (one row per child, with Social Rank)
#
# Steps
#   1. Tokens received: match each decision i -> j with the decision j -> i
#   2. Cooperation GAP = tokens received - tokens sent
#   3. Friendship type from the best-friend question (Q3)
#   4. Social Rank: PageRank on the popularity network (Q4 and Q5)
#   5. Social Rank GAP = sender rank - receiver rank
#   6. Null-model filtering of token exchanges (SI robustness)
#
# The notebook notebooks/main/01_data_and_measures.qmd walks through these steps.
# =============================================================================

## ---- packages ----
library(tidyverse)
library(igraph)
library(here)


## ---- read-data ----
dyads    <- read_csv(here("data", "raw", "dyads.csv"),    show_col_types = FALSE)
students <- read_csv(here("data", "raw", "students.csv"), show_col_types = FALSE)

nrow(dyads)      # 17,804 directed decisions
nrow(students)   # 859 children
n_distinct(dyads$class_id)   # 45 classrooms


## ---- tokens-received ----
# Each row is one decision: sender i gives `tokens_sent` (0 to 10) to receiver j.
# In the same round, j also decided how much to give to i. That reverse
# decision is what i receives. We find it by swapping sender and receiver.

reverse_decisions <- dyads %>%
  select(class_id,
         sender_id   = receiver_id,
         receiver_id = sender_id,
         tokens_received = tokens_sent,
         round_reverse = round)

dyads <- dyads %>%
  left_join(reverse_decisions, by = c("class_id", "sender_id", "receiver_id"))

# every decision has its reverse, and both happened in the same round
stopifnot(!anyNA(dyads$tokens_received))
stopifnot(all(dyads$round == dyads$round_reverse))

dyads <- dyads %>% select(-round_reverse)


## ---- cooperation-gap ----
# Cooperation GAP (from the sender's point of view): what i receives minus what
# i sends. Positive values mean i receives more than i gives.

dyads <- dyads %>%
  mutate(coop_gap = tokens_received - tokens_sent)


## ---- friendship-type ----
# Question 3: "Select 2-5 classmates you consider your best friends".
# nom_q3 = 1 if the sender named the receiver. To know if the receiver also
# named the sender, we look at the reverse row, as we did with the tokens.

reverse_friend <- dyads %>%
  select(class_id,
         sender_id   = receiver_id,
         receiver_id = sender_id,
         friend_reverse = nom_q3)

dyads <- dyads %>%
  left_join(reverse_friend, by = c("class_id", "sender_id", "receiver_id")) %>%
  mutate(friendship = case_when(
    nom_q3 == 1 & friend_reverse == 1 ~ "Mutual",
    nom_q3 == 1 & friend_reverse == 0 ~ "Sender declares",    # one-sided, i named j
    nom_q3 == 0 & friend_reverse == 1 ~ "Receiver declares",  # one-sided, j named i
    nom_q3 == 0 & friend_reverse == 0 ~ "Non-friends"
  ))

dyads %>% count(friendship) %>% mutate(percent = round(100 * n / sum(n), 2))


## ---- popularity-network ----
# Questions 4 and 5: "most popular and well-known" and "not popular or
# well-known". For every ordered pair (i, j) in a classroom:
#
#     s_ij = nom_q4 - nom_q5      +1 popular, -1 not popular, 0 not named
#     a_ij = s_ij + 1             weights 2, 0 and 1 (all non-negative)
#
# a_ij is the weight of the edge i -> j in the popularity network.

popularity_edges <- dyads %>%
  transmute(class_id,
            from = as.character(sender_id),
            to   = as.character(receiver_id),
            weight = nom_q4 - nom_q5 + 1)

count(popularity_edges, weight)


## ---- pagerank ----
# PageRank (damping 0.85) is computed separately in each classroom.
# We then rescale it within each classroom to go from 0 (lowest rank in the
# class) to 1 (highest rank in the class). This is our Social Rank.

pagerank_one_class <- function(edges_one_class) {
  network <- graph_from_data_frame(edges_one_class %>% select(from, to, weight),
                                   directed = TRUE)
  pr <- page_rank(network, damping = 0.85, directed = TRUE)$vector   # uses edge weights
  tibble(student_id = as.numeric(names(pr)), pagerank = pr)
}

social_rank <- popularity_edges %>%
  group_by(class_id) %>%
  group_modify(~ pagerank_one_class(.x)) %>%
  mutate(social_rank = (pagerank - min(pagerank)) / (max(pagerank) - min(pagerank))) %>%
  ungroup()

students <- students %>%
  left_join(social_rank %>% select(student_id, pagerank, social_rank),
            by = "student_id")

summary(social_rank$social_rank)


## ---- rank-gap ----
# Social Rank GAP of a decision = sender's rank - receiver's rank.
# Positive: the sender has higher rank. It goes from -1 to 1.

dyads <- dyads %>%
  left_join(social_rank %>% select(sender_id = student_id, sender_rank = social_rank),
            by = "sender_id") %>%
  left_join(social_rank %>% select(receiver_id = student_id, receiver_rank = social_rank),
            by = "receiver_id") %>%
  mutate(rank_gap = sender_rank - receiver_rank,
         # round away floating-point noise, so that two children with the
         # same rank have a gap of exactly 0 (and not 1e-16)
         rank_gap = round(rank_gap, 10))

summary(dyads$rank_gap)


## ---- student-covariates ----
# Attendance is standardized within each classroom (mean 0, SD 1), so it
# compares a child with her own classmates. Used in the balance check.

students <- students %>%
  group_by(class_id) %>%
  mutate(attendance_z = (attendance - mean(attendance)) / sd(attendance)) %>%
  ungroup()


## ---- null-model-score ----
# Null-model filtering (Methods and SI). For each sender we shuffle *who*
# receives each of her allocations, keeping all her allocations as they are.
# Under this shuffle every classmate is equally likely to get any of them, so
# the expected value and spread of any cell i -> j are simply the mean and the
# (population) standard deviation of sender i's own allocations.
# (The notebook checks this with 1,000 actual shuffles.)
#
# Each observed transfer gets a score: how far it is from the sender's usual
# giving, in units of the sender's own standard deviation.
#
#     d_ij = | tokens_sent_ij - mean_i | / sd_i

dyads <- dyads %>%
  group_by(sender_id) %>%
  mutate(sender_mean = mean(tokens_sent),
         sender_sd   = sqrt(mean((tokens_sent - sender_mean)^2)),
         departure   = if_else(sender_sd > 0,
                               abs(tokens_sent - sender_mean) / sender_sd,
                               0)) %>%
  ungroup()


## ---- null-model-filter ----
# Keep the 50% of all directed transfers with the largest departure (one
# common cutoff for the whole sample). Dropped transfers become NA, not 0,
# because 0 is a real decision in the game.
# A dyad enters the filtered Cooperation GAP only if BOTH directions are kept.

retention <- 0.50
cutoff <- sort(dyads$departure, decreasing = TRUE)[ceiling(retention * nrow(dyads))]

reverse_departure <- dyads %>%
  select(class_id,
         sender_id   = receiver_id,
         receiver_id = sender_id,
         departure_reverse = departure)

dyads <- dyads %>%
  left_join(reverse_departure, by = c("class_id", "sender_id", "receiver_id")) %>%
  mutate(
    tokens_sent_filtered     = if_else(departure >= cutoff,         tokens_sent,     NA_real_),
    tokens_received_filtered = if_else(departure_reverse >= cutoff, tokens_received, NA_real_),
    coop_gap_filtered        = tokens_received_filtered - tokens_sent_filtered
  )

mean(!is.na(dyads$tokens_sent_filtered))   # share of transfers kept (about 0.50)
mean(!is.na(dyads$coop_gap_filtered))      # share of dyads kept     (about 0.26)


## ---- save ----
dir.create(here("data", "generated"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "tables", "si"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures", "si"), recursive = TRUE, showWarnings = FALSE)

dyads <- dyads %>%
  select(class_id, round, sender_id, receiver_id,
         tokens_sent, tokens_received, coop_gap,
         friendship, sender_rank, receiver_rank, rank_gap,
         departure, tokens_sent_filtered, tokens_received_filtered, coop_gap_filtered,
         seconds_since_start, starts_with("nom_q"))

write_csv(dyads,    here("data", "generated", "analysis_dyads.csv"))
write_csv(students, here("data", "generated", "analysis_students.csv"))
