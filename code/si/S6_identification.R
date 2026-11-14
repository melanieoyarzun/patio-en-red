# =============================================================================
# S6_identification.R   SI Section 6: Identification and empirical support
#
#   Table S16   Balance: raw GAP, partner rank and demeaned partner rank
#   Figure S15  The same coefficients on a common (SD) scale
#   Figure S16  Balance round by round
#   Figure S17  Demeaned exposure by covariate groups
#   Figures S18-S19  Social Rank GAP by round (all classes; class 12)
#   Figure S20  Social Rank GAP and Cooperation GAP by classroom
#   Figure S21  Rank-gap support among non-friends and mutual friends
#
#   Input : data/generated/analysis_dyads.csv, data/generated/analysis_students.csv
#   Output: output/tables/si/S6_*.csv, output/figures/si/S6_*.(pdf|png)
# =============================================================================

## ---- s6-setup ----
source(here::here("code", "00_helpers.R"))
source(here("code", "00_style.R"))
library(patchwork)
dir.create(here("output", "figures", "si"), recursive = TRUE, showWarnings = FALSE)

dyads    <- read_csv(here("data", "generated", "analysis_dyads.csv"),    show_col_types = FALSE)
students <- read_csv(here("data", "generated", "analysis_students.csv"), show_col_types = FALSE)

save_si_figure <- function(plot, name, width, height) {
  ggsave(here("output", "figures", "si", paste0(name, ".pdf")), plot, width = width, height = height)
  ggsave(here("output", "figures", "si", paste0(name, ".png")), plot, width = width, height = height, dpi = 300)
}


## ---- s6-exposure ----
# Three versions of the exposure of the focal child (the sender):
#   raw_gap               partner rank - own rank  (higher = met a higher-ranked partner)
#   partner_rank          the partner's rank only
#   partner_rank_demeaned partner rank - the rank she could expect: the mean
#                         rank of the other children active in that class-round
balance_data <- dyads %>%
  left_join(students %>% select(sender_id = student_id, female, attendance_z, gpa), by = "sender_id") %>%
  mutate(class_round  = factor(paste(class_id, round, sep = "_")),
         raw_gap      = receiver_rank - sender_rank,
         partner_rank = receiver_rank) %>%
  group_by(class_round) %>%
  mutate(expected_partner_rank = (sum(sender_rank) - sender_rank) / (n() - 1),
         partner_rank_demeaned = partner_rank - expected_partner_rank) %>%
  ungroup()

outcomes <- c(raw_gap = "A: Social Rank GAP (partner - sender)",
              partner_rank = "B: Partner Rank",
              partner_rank_demeaned = "C: Partner Rank (demeaned)")


## ---- s6-balance-table ----
# Table S16: each outcome on each covariate alone (1-3) and on all three (4),
# with classroom-by-round FE and SE clustered by classroom
covariate_sets <- list("(1)" = "female", "(2)" = "attendance_z", "(3)" = "gpa",
                       "(4)" = "female + attendance_z + gpa")

balance_models <- expand_grid(outcome = names(outcomes), model = names(covariate_sets)) %>%
  mutate(fit = map2(outcome, model, ~ felm(
    as.formula(paste(.x, "~", covariate_sets[[.y]], "| class_round | 0 | class_id")),
    data = balance_data)))

table_s16 <- balance_models %>%
  mutate(coefs = map(fit, ~ summary(.x)$coefficients %>% as_tibble(rownames = "term")),
         r2 = map_dbl(fit, ~ summary(.x)$r.squared)) %>%
  select(-fit) %>%
  unnest(coefs) %>%
  transmute(panel = outcomes[outcome], model, term, estimate = Estimate,
            se = `Cluster s.e.`, p_value = `Pr(>|t|)`, r2)

table_s16 %>%
  mutate(cell = paste0(sprintf("%.4f", estimate), stars(p_value), " (", sprintf("%.4f", se), ")")) %>%
  select(panel, term, model, cell) %>%
  pivot_wider(names_from = model, values_from = cell, values_fill = "")


## ---- s6-figure-s15 ----
# Coefficients of model (4) divided by the within-class-round SD of each
# outcome, so the three outcomes share a scale
outcome_sd <- balance_data %>%
  group_by(class_round) %>%
  mutate(across(all_of(names(outcomes)), ~ .x - mean(.x))) %>%
  ungroup() %>%
  summarise(across(all_of(names(outcomes)), sd)) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "sd")

figure_s15_data <- balance_models %>%
  filter(model == "(4)") %>%
  mutate(coefs = map(fit, ~ summary(.x)$coefficients %>% as_tibble(rownames = "term"))) %>%
  select(outcome, coefs) %>% unnest(coefs) %>%
  left_join(outcome_sd, by = "outcome") %>%
  mutate(estimate = Estimate / sd, se = `Cluster s.e.` / sd,
         outcome = factor(outcomes[outcome], levels = outcomes),
         term = recode(term, female = "Female", attendance_z = "Attendance", gpa = "GPA"))

figure_s15 <- ggplot(figure_s15_data, aes(estimate, term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se), height = 0.2) +
  geom_point(size = 2) +
  facet_wrap(~ outcome, nrow = 1) +
  labs(x = "Coefficient (within class-round SD units)", y = NULL) +
  theme_paper(9) + theme(strip.background = element_blank(), strip.text = element_text(face = "bold"))
figure_s15


## ---- s6-figure-s16 ----
# Round by round: each outcome on each covariate, classroom FE, SE by class
round_balance <- expand_grid(round_id = sort(unique(balance_data$round)),
                             outcome = names(outcomes),
                             covariate = c("female", "attendance_z", "gpa")) %>%
  mutate(result = pmap(list(round_id, outcome, covariate), function(r, y, x) {
    d <- balance_data %>% filter(round == r)
    if (n_distinct(d$class_id) < 3) return(NULL)
    fit <- felm(as.formula(paste(y, "~", x, "| class_id | 0 | class_id")), data = d)
    ct <- summary(fit)$coefficients
    tibble(estimate = ct[x, "Estimate"], se = ct[x, "Cluster s.e."])
  })) %>%
  unnest(result)

figure_s16 <- round_balance %>%
  mutate(outcome = factor(outcomes[outcome], levels = outcomes),
         covariate = recode(covariate, female = "Female", attendance_z = "Attendance", gpa = "GPA")) %>%
  ggplot(aes(round_id, estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), size = 0.1) +
  facet_grid(outcome ~ covariate, scales = "free_y", labeller = label_wrap_gen(20)) +
  labs(x = "Round", y = "Coefficient") +
  theme_paper(8) + theme(strip.background = element_blank())
figure_s16


## ---- s6-figure-s17 ----
# Raw exposure after removing class-round means, by covariate group
# (attendance and GPA split at their median)
density_data <- balance_data %>%
  group_by(class_round) %>% mutate(gap_demeaned = raw_gap - mean(raw_gap)) %>% ungroup() %>%
  transmute(gap_demeaned,
            Attendance = if_else(attendance_z > median(attendance_z), "Above median", "Below median"),
            GPA        = if_else(gpa > median(gpa), "Above median", "Below median"),
            Female     = if_else(female == 1, "Girls", "Boys")) %>%
  pivot_longer(-gap_demeaned, names_to = "covariate", values_to = "group")

figure_s17 <- ggplot(density_data, aes(gap_demeaned, color = group)) +
  geom_density() +
  facet_wrap(~ covariate) +
  labs(x = "Social Rank GAP (class-round demeaned)", y = "Density", color = NULL) +
  theme_paper(9) + theme(legend.position = "bottom", strip.background = element_blank())
figure_s17


## ---- s6-figures-s18-s19 ----
figure_s18 <- ggplot(dyads, aes(factor(round), rank_gap)) +
  geom_boxplot(outlier.size = 0.2, fill = "grey90") +
  scale_x_discrete(breaks = seq(1, 43, 3)) +
  labs(x = "Round", y = "Social Rank GAP") + theme_paper(9)

figure_s19 <- dyads %>% filter(class_id == 12) %>%
  ggplot(aes(rank_gap)) +
  geom_histogram(binwidth = 0.1, fill = "grey55") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ round, ncol = 5) +
  labs(x = "Social Rank GAP", y = "Decisions") +
  theme_paper(7) + theme(strip.background = element_blank())
figure_s18


## ---- s6-figure-s20 ----
figure_s20 <- dyads %>%
  select(class_id, `Social Rank GAP` = rank_gap, `Cooperation GAP` = coop_gap) %>%
  pivot_longer(-class_id) %>%
  ggplot(aes(factor(class_id), value)) +
  geom_boxplot(outlier.size = 0.2, fill = "grey90") +
  facet_wrap(~ name, ncol = 1, scales = "free_y") +
  labs(x = "Classroom", y = NULL) +
  theme_paper(8) + theme(strip.background = element_blank())
figure_s20


## ---- s6-support ----
# Figure S21: how much rank-gap variation mutual friendships really have
support <- dyads %>%
  filter(friendship %in% c("Non-friends", "Mutual")) %>%
  group_by(friendship) %>%
  summarise(n = n(), sd = sd(rank_gap),
            share_025 = 100 * mean(abs(rank_gap) >= 0.25),
            share_050 = 100 * mean(abs(rank_gap) >= 0.50),
            classes_with_obs = n_distinct(class_id),
            classes_025 = n_distinct(class_id[abs(rank_gap) >= 0.25]),
            classes_050 = n_distinct(class_id[abs(rank_gap) >= 0.50]))
support

figure_s21 <- dyads %>%
  filter(friendship %in% c("Non-friends", "Mutual")) %>%
  ggplot(aes(rank_gap, friendship, fill = friendship)) +
  geom_violin(alpha = 0.5, color = NA) +
  geom_jitter(height = 0.12, size = 0.1, alpha = 0.15) +
  geom_boxplot(width = 0.12, outlier.shape = NA, fill = "white") +
  geom_vline(xintercept = c(-0.25, 0.25), linetype = "dashed", color = "#4B6587") +
  geom_vline(xintercept = c(-0.5, 0.5), linetype = "dotted", color = "#D9874C") +
  scale_fill_manual(values = c("Non-friends" = color_non_friends, Mutual = color_mutual)) +
  labs(x = "Social Rank GAP", y = NULL) + theme_paper(9)
figure_s21


## ---- s6-save ----
save_table(table_s16, "S6_table_s16_balance")
save_table(round_balance, "S6_round_balance")
save_table(support, "S6_support_by_friendship")
save_si_figure(figure_s15, "S6_figure_s15_balance_sd", 8, 2.6)
save_si_figure(figure_s16, "S6_figure_s16_round_balance", 9, 6)
save_si_figure(figure_s17, "S6_figure_s17_densities", 9, 3)
save_si_figure(figure_s18, "S6_figure_s18_gap_by_round", 8, 3.5)
save_si_figure(figure_s19, "S6_figure_s19_class12_rounds", 8, 9)
save_si_figure(figure_s20, "S6_figure_s20_by_classroom", 10, 5)
save_si_figure(figure_s21, "S6_figure_s21_support", 7, 3.5)
