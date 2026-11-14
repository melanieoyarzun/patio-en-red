# =============================================================================
# 02_table1.R
#
# Table 1: Cooperation GAP regressed on Social Rank GAP, by relationship type.
# Also the pooled model that tests whether the slope is flatter among mutual
# friends (reported in the text and in SI Table S20).
#
#   Input : data/generated/analysis_dyads.csv
#   Output: output/tables/table1.csv               (all numbers, full precision)
#           output/tables/table1_display.csv       (laid out as Table 1)
#           output/tables/table1_pooled_interaction.csv
# =============================================================================

## ---- table1-packages ----
library(tidyverse)
library(lfe)      # felm(): linear models with many fixed effects
library(here)

dyads <- read_csv(here("data", "generated", "analysis_dyads.csv"), show_col_types = FALSE) %>%
  mutate(class_round = paste(class_id, round, sep = "_"),   # one code per classroom x round
         across(c(sender_id, class_id, class_round), as.factor))


## ---- table1-samples ----
# Five samples, one per column of Table 1.
#
# One-sided friendships are seen from the side of the child who named the
# other ("Sender declares"), and split by who has the higher rank:
#   L -> H : the lower-ranked child names the higher-ranked one (rank_gap < 0)
#   H -> L : the higher-ranked child names the lower-ranked one (rank_gap > 0)
# Two of these 1,300 pairs have exactly the same rank (rank_gap = 0). As in
# the paper, they are counted in the H -> L column.

samples <- list(
  "All dyads"      = dyads,
  "Non-friends"    = dyads %>% filter(friendship == "Non-friends"),
  "Mutual friends" = dyads %>% filter(friendship == "Mutual"),
  "Asym. (L->H)"   = dyads %>% filter(friendship == "Sender declares", rank_gap < 0),
  "Asym. (H->L)"   = dyads %>% filter(friendship == "Sender declares", rank_gap >= 0)
)

sapply(samples, nrow)


## ---- table1-models ----
# Linear regression with fixed effects, standard errors clustered by classroom.
#
#   Coop GAP_ij = beta * RankGAP_ij + sender FE + classroom-by-round FE
#
# felm formula:  outcome ~ regressor | fixed effects | (no IV) 0 | cluster
#
# The two asymmetric samples are small, so they use classroom FE instead of
# classroom-by-round FE.

with_class_round <- coop_gap ~ rank_gap | sender_id + class_round | 0 | class_id
with_class       <- coop_gap ~ rank_gap | sender_id + class_id    | 0 | class_id

models <- list(
  "All dyads"      = felm(with_class_round, data = samples[["All dyads"]]),
  "Non-friends"    = felm(with_class_round, data = samples[["Non-friends"]]),
  "Mutual friends" = felm(with_class_round, data = samples[["Mutual friends"]]),
  "Asym. (L->H)"   = felm(with_class,       data = samples[["Asym. (L->H)"]]),
  "Asym. (H->L)"   = felm(with_class,       data = samples[["Asym. (H->L)"]])
)

summary(models[["Non-friends"]])


## ---- table1-panel-b ----
# Panel B puts beta in more intuitive units:
#
#   effect in tokens     = beta * SD(Social Rank GAP)
#   effect in SD tokens  = beta * SD(Social Rank GAP) / SD(Cooperation GAP)
#   model-adjusted       = beta * SD(Social Rank GAP after removing the fixed
#                          effects) / SD(Cooperation GAP)
#
# The last one uses only the part of the rank gap that the fixed-effects
# model actually uses (its variation within sender and within class-round).

fixed_effects_used <- list(
  "All dyads"      = c("sender_id", "class_round"),
  "Non-friends"    = c("sender_id", "class_round"),
  "Mutual friends" = c("sender_id", "class_round"),
  "Asym. (L->H)"   = c("sender_id", "class_id"),
  "Asym. (H->L)"   = c("sender_id", "class_id")
)

rank_gap_within <- function(data, fixed_effects) {
  # demeanlist() removes the fixed effects from a variable
  demeaned <- demeanlist(list(data$rank_gap), as.list(data[, fixed_effects]))
  demeaned[[1]]
}


## ---- table1-assemble ----
stars <- function(p) {
  case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "")
}

table1 <- map_dfr(names(models), function(name) {
  model   <- models[[name]]
  data    <- samples[[name]]
  results <- summary(model)
  beta    <- coef(model)[["rank_gap"]]

  tibble(
    sample   = name,
    beta     = beta,
    se       = results$coefficients["rank_gap", "Cluster s.e."],
    p_value  = results$coefficients["rank_gap", "Pr(>|t|)"],
    n        = model$N,
    r2       = results$r.squared,
    r2_adj   = results$adj.r.squared,
    effect_tokens          = beta * sd(data$rank_gap),
    effect_sd_tokens       = beta * sd(data$rank_gap) / sd(data$coop_gap),
    effect_sd_model_adjust = beta * sd(rank_gap_within(data, fixed_effects_used[[name]])) /
                             sd(data$coop_gap)
  )
})

table1 %>% mutate(across(where(is.numeric), ~ round(.x, 3)))


## ---- table1-display ----
# The same numbers, laid out as Table 1 (one column per sample)
table1_display <- table1 %>%
  transmute(
    sample,
    `Social Rank GAP`           = paste0(sprintf("%.2f", beta), stars(p_value)),
    `(SE)`                      = sprintf("(%.2f)", se),
    Observations                = format(n, big.mark = ",", trim = TRUE),
    `R2`                        = sprintf("%.3f", r2),
    `R2 adj.`                   = sprintf("%.3f", r2_adj),
    `Effect in tokens`          = paste0(sprintf("%.2f", effect_tokens), stars(p_value)),
    `Effect in SD tokens`       = paste0(sprintf("%.3f", effect_sd_tokens), stars(p_value)),
    `Effect in SD (model-adj.)` = paste0(sprintf("%.3f", effect_sd_model_adjust), stars(p_value))
  ) %>%
  pivot_longer(-sample, names_to = "row") %>%
  pivot_wider(names_from = sample, values_from = value)

table1_display


## ---- pooled-interaction ----
# Separate regressions describe each group but do not test whether the slopes
# differ. The pooled model does: non-friends and mutual friends together, with
# a mutual-friend dummy and its interaction with the rank gap.
#
#   Coop GAP = b1 RankGAP + b2 Mutual + b3 (RankGAP x Mutual) + sender FE + class-round FE
#
# b3 < 0 means the rank gradient is flatter among mutual friends.

non_and_mutual <- dyads %>%
  filter(friendship %in% c("Non-friends", "Mutual")) %>%
  mutate(mutual = as.integer(friendship == "Mutual"))

pooled <- felm(coop_gap ~ rank_gap * mutual | sender_id + class_round | 0 | class_id,
               data = non_and_mutual)

pooled_table <- summary(pooled)$coefficients %>%
  as_tibble(rownames = "term")

pooled_table


## ---- table1-save ----
write_csv(table1,         here("output", "tables", "table1.csv"))
write_csv(table1_display, here("output", "tables", "table1_display.csv"))
write_csv(pooled_table,   here("output", "tables", "table1_pooled_interaction.csv"))
