# =============================================================================
# S5_game_and_time.R   SI Section 5: Social-dilemma game and cooperation data
#
#   Table S11  Matching schedule (A) and temporal trends in giving (B)
#   Table S12  Game behavior; Figure S9 token exchanges
#   Figure S10 Giving over rounds and session progress; Table S13 coverage
#   Figure S11 / Table S14  The same by friendship type, with slope tests
#   Figure S12 / Table S15  Composition decomposition and within-child change
#   Figure S13 Giving over the session by classroom size
#   Figure S14 Time between decisions
#
#   Input : data/generated/analysis_dyads.csv, data/generated/analysis_students.csv
#   Output: output/tables/si/S5_*.csv, output/figures/si/S5_*.(pdf|png)
# =============================================================================

## ---- s5-setup ----
source(here::here("code", "00_helpers.R"))
source(here("code", "00_style.R"))
library(patchwork)
dir.create(here("output", "figures", "si"), recursive = TRUE, showWarnings = FALSE)

dyads <- read_csv(here("data", "generated", "analysis_dyads.csv"), show_col_types = FALSE)

save_si_figure <- function(plot, name, width, height) {
  ggsave(here("output", "figures", "si", paste0(name, ".pdf")), plot, width = width, height = height)
  ggsave(here("output", "figures", "si", paste0(name, ".png")), plot, width = width, height = height, dpi = 300)
}


## ---- s5-session-position ----
# Three ways of saying "when" a decision happened:
#   round           the classroom round of the matching schedule
#   decision_order  1st, 2nd, ... decision of this child (children skip
#                   rounds in which they have no partner)
#   progress        (order - 1) / (number of decisions - 1), from 0 to 1
# plus the classroom size and three friendship groups.

dyads <- dyads %>%
  group_by(class_id) %>%
  mutate(class_size = n_distinct(sender_id)) %>%
  group_by(sender_id) %>%
  arrange(round, .by_group = TRUE) %>%
  mutate(decision_order = row_number(),
         progress       = (decision_order - 1) / (n() - 1),
         fifth          = pmin(pmax(ceiling(progress * 5), 1), 5)) %>%   # 0 counts in the first fifth
  ungroup() %>%
  mutate(friend_group = case_when(friendship == "Mutual"      ~ "Mutual friends",
                                  friendship == "Non-friends" ~ "Non-friends",
                                  TRUE                        ~ "Asymmetric ties"),
         friend_group = factor(friend_group, levels = c("Non-friends", "Asymmetric ties", "Mutual friends")))


## ---- s5-schedule ----
# Table S11, Panel A
describe <- function(x, unit) {
  tibble(unit = unit, n = length(x), mean = mean(x), sd = sd(x), median = median(x),
         range = paste0(min(x), "-", max(x)))
}

table_s11_a <- bind_rows(
  describe(dyads %>% group_by(class_id) %>% summarise(r = n_distinct(round)) %>% pull(r),
           "Recorded matching rounds per classroom"),
  describe(dyads %>% count(sender_id) %>% pull(n), "Directed decisions per participant"),
  describe(dyads %>% distinct(class_id, class_size) %>% pull(class_size), "Participants per classroom")
)

table_s11_a


## ---- s5-temporal-models ----
# Table S11, Panel B: linear trends in tokens sent. All standard errors are
# clustered by classroom; class size is centered at 18 in the interactions.
dyads <- dyads %>%
  mutate(size_18 = class_size - 18,
         across(c(sender_id, receiver_id, class_id), as.factor, .names = "{.col}_f"))

trend <- function(formula, data = dyads) felm(as.formula(formula), data = data)

temporal_models <- list(
  "Absolute round, unadjusted"         = trend("tokens_sent ~ round | 0 | 0 | class_id"),
  "Absolute round + class size"        = trend("tokens_sent ~ round + class_size | 0 | 0 | class_id"),
  "Absolute round + classroom FE"      = trend("tokens_sent ~ round | class_id_f | 0 | class_id"),
  "Absolute round + sender and partner FE" = trend("tokens_sent ~ round | sender_id_f + receiver_id_f | 0 | class_id"),
  "Rounds 1-7 + sender and partner FE" = trend("tokens_sent ~ round | sender_id_f + receiver_id_f | 0 | class_id",
                                               data = filter(dyads, round <= 7)),
  "Relative progress + sender and partner FE" = trend("tokens_sent ~ progress | sender_id_f + receiver_id_f | 0 | class_id"),
  "Relative progress x class size"     = trend("tokens_sent ~ progress * size_18 | sender_id_f + receiver_id_f | 0 | class_id"),
  "Absolute round x class size"        = trend("tokens_sent ~ round * size_18 | sender_id_f + receiver_id_f | 0 | class_id")
)

table_s11_b <- imap_dfr(temporal_models, function(model, name) {
  summary(model)$coefficients %>%
    as_tibble(rownames = "term") %>%
    filter(term != "(Intercept)", !is.nan(Estimate)) %>%   # main effect of size is absorbed by the FE
    transmute(specification = name, term, estimate = Estimate,
              se = `Cluster s.e.`, p_value = `Pr(>|t|)`, n = model$N)
})

table_s11_b %>% mutate(across(where(is.double), ~ round(.x, 4)))


## ---- s5-game-descriptives ----
# Table S12. Directed decisions for tokens and GAP; one row per unordered
# pair for the difference between the two allocations.
pairs <- dyads %>% filter(sender_id_f != receiver_id_f, as.numeric(as.character(sender_id)) < as.numeric(as.character(receiver_id)))

table_s12 <- tribble(
  ~quantity,                        ~n,             ~mean,                                    ~sd,                                    ~median,
  "Tokens sent",                    nrow(dyads),    mean(dyads$tokens_sent),                  sd(dyads$tokens_sent),                  median(dyads$tokens_sent),
  "Absolute reciprocal difference", nrow(pairs),    mean(abs(pairs$coop_gap)),                sd(abs(pairs$coop_gap)),                median(abs(pairs$coop_gap)),
  "Observed Cooperation GAP",       nrow(dyads),    mean(dyads$coop_gap),                     sd(dyads$coop_gap),                     median(dyads$coop_gap)
)

table_s12

c(zero_tokens = mean(dyads$tokens_sent == 0), ten_tokens = mean(dyads$tokens_sent == 10),
  equal_pairs = mean(pairs$coop_gap == 0),
  gap_negative = mean(dyads$coop_gap < 0), gap_zero = mean(dyads$coop_gap == 0),
  gap_positive = mean(dyads$coop_gap > 0),
  zero_zero_among_equal = mean(pairs$tokens_sent[pairs$coop_gap == 0] == 0)) %>% round(3)


## ---- s5-figure-s9 ----
panel_a <- ggplot(dyads, aes(tokens_sent)) +
  geom_bar(fill = "grey55") +
  scale_x_continuous(breaks = 0:10) +
  labs(title = "A. Tokens sent", x = "Tokens sent", y = "Decisions") + theme_paper(9)

pair_cells <- pairs %>%
  count(high = pmax(tokens_sent, tokens_received), low = pmin(tokens_sent, tokens_received)) %>%
  mutate(percent = 100 * n / sum(n))

panel_b <- ggplot(pair_cells, aes(high, low, fill = percent)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(percent, 1)), size = 1.8) +
  scale_fill_gradient(low = "#FFF4D6", high = "#C95A3A") +
  scale_x_continuous(breaks = 0:10) + scale_y_continuous(breaks = 0:10) +
  coord_fixed() +
  labs(title = "B. The two allocations of each pair", x = "Higher allocation", y = "Lower allocation") +
  theme_paper(9)

panel_c <- dyads %>%
  mutate(kind = case_when(coop_gap == 0 & tokens_sent == 0 ~ "0-0 exchange",
                          coop_gap == 0                    ~ "equal positive",
                          TRUE                             ~ "unequal")) %>%
  ggplot(aes(coop_gap, fill = kind)) +
  geom_bar() +
  scale_fill_manual(values = c("0-0 exchange" = "#E7B56A", "equal positive" = "#C95A3A", unequal = "grey60")) +
  labs(title = "C. Cooperation GAP", x = "Received - sent", y = "Decisions", fill = NULL) +
  theme_paper(9) + theme(legend.position = c(0.2, 0.8))

panel_d <- pairs %>%
  filter(coop_gap == 0) %>%
  mutate(friend_group = if_else(friendship %in% c("Sender declares", "Receiver declares"),
                                "Asymmetric ties", as.character(friend_group))) %>%
  ggplot(aes(tokens_sent, fill = friend_group)) +
  geom_bar() +
  scale_fill_manual(values = c("Non-friends" = color_non_friends, "Asymmetric ties" = color_asymmetric,
                               "Mutual friends" = color_mutual)) +
  scale_x_continuous(breaks = 0:10) +
  labs(title = "D. Balanced pairs by allocation and friendship", x = "Tokens each sent", y = "Pairs", fill = NULL) +
  theme_paper(9) + theme(legend.position = c(0.3, 0.8))

figure_s9 <- (panel_a | panel_b) / (panel_c | panel_d)
figure_s9


## ---- s5-round-means ----
# Means with 95% confidence intervals, and the number of classrooms behind
# each point (used for point size)
mean_ci <- function(data, ...) {
  data %>%
    group_by(...) %>%
    summarise(mean = mean(tokens_sent), se = sd(tokens_sent) / sqrt(n()),
              classrooms = n_distinct(class_id), decisions = n(), .groups = "drop") %>%
    mutate(low = mean - 1.96 * se, high = mean + 1.96 * se)
}

by_round    <- mean_ci(dyads, round)
by_order    <- mean_ci(dyads, decision_order)
by_fifth    <- mean_ci(dyads, fifth)

by_round %>% filter(round <= 7)

## ---- s5-means-by-fifth ----
by_fifth


## ---- s5-figure-s10 ----
time_panel <- function(data, x, title, xlab, ribbon = TRUE) {
  p <- ggplot(data, aes({{ x }}, mean))
  if (ribbon) p <- p + geom_ribbon(aes(ymin = low, ymax = high), fill = "grey85")
  p + geom_line(color = "grey30") +
    geom_point(aes(size = classrooms), color = "grey20") +
    scale_size_continuous(range = c(0.4, 2.2)) +
    labs(title = title, x = xlab, y = "Mean tokens sent") +
    theme_paper(9)
}

figure_s10 <- (time_panel(by_round, round, "A. Classroom round", "Round") |
               time_panel(filter(by_round, round <= 7), round, "B. Rounds 1-7", "Round")) /
              (time_panel(by_order, decision_order, "C. Personal decision order", "Decision", ribbon = FALSE) |
               time_panel(by_fifth, fifth, "D. Fifths of personal progress", "Fifth") +
                 geom_errorbar(aes(ymin = low, ymax = high), width = 0.1))
figure_s10


## ---- s5-coverage ----
# Table S13: how many classrooms have at least one decision in each round
coverage <- dyads %>%
  group_by(round) %>%
  summarise(classrooms = n_distinct(class_id)) %>%
  mutate(share = 100 * classrooms / 45)

coverage %>% group_by(classrooms, share) %>%
  summarise(rounds = paste0(min(round), "-", max(round)), .groups = "drop") %>%
  arrange(desc(classrooms))


## ---- s5-by-friendship ----
# Figure S11: the same four views, by friendship group
by_round_f <- mean_ci(dyads, round, friend_group)
by_order_f <- mean_ci(dyads, decision_order, friend_group)
by_fifth_f <- mean_ci(dyads, fifth, friend_group)

by_round_f %>% filter(round <= 2)

## ---- s5-friendship-by-fifth ----
by_fifth_f %>% filter(fifth %in% c(1, 5))


## ---- s5-figure-s11 ----
friend_colors <- c("Non-friends" = color_non_friends, "Asymmetric ties" = color_asymmetric,
                   "Mutual friends" = color_mutual)

group_panel <- function(data, x, title, xlab) {
  ggplot(data, aes({{ x }}, mean, color = friend_group)) +
    geom_hline(yintercept = mean(dyads$tokens_sent), linetype = "dashed", color = "#7C8FA6") +
    geom_line() +
    geom_point(aes(size = classrooms)) +
    scale_color_manual(values = friend_colors) +
    scale_size_continuous(range = c(0.3, 2), guide = "none") +
    labs(title = title, x = xlab, y = "Mean tokens sent", color = NULL) +
    theme_paper(9)
}

figure_s11 <- (group_panel(by_round_f, round, "A. Classroom round", "Round") |
               group_panel(filter(by_round_f, round <= 7), round, "B. Rounds 1-7", "Round")) /
              (group_panel(by_order_f, decision_order, "C. Personal decision order", "Decision") |
               group_panel(by_fifth_f, fifth, "D. Fifths of personal progress", "Fifth") +
                 geom_errorbar(aes(ymin = low, ymax = high), width = 0.1) +
                 theme(legend.position = "bottom"))
figure_s11


## ---- s5-friendship-slopes ----
# Table S14: does giving change at the same rate in the three groups?
# tokens ~ time x group, with sender and receiver FE. Time = round - 1
# (columns 1-2) or personal progress (column 3).
dyads <- dyads %>% mutate(round_c = round - 1)

slope_models <- list(
  "All classroom rounds" = felm(tokens_sent ~ round_c * friend_group | sender_id_f + receiver_id_f | 0 | class_id, data = dyads),
  "Classroom rounds 1-7" = felm(tokens_sent ~ round_c * friend_group | sender_id_f + receiver_id_f | 0 | class_id, data = filter(dyads, round <= 7)),
  "Personal progress"    = felm(tokens_sent ~ progress * friend_group | sender_id_f + receiver_id_f | 0 | class_id, data = dyads)
)

slope_table <- imap_dfr(slope_models, function(model, name) {
  b <- coef(model)
  V <- vcov(model)
  time <- names(b)[1]
  interactions <- grep(":", names(b), value = TRUE)

  # implied slope of each group = reference slope + its interaction
  implied <- map_dfr(interactions, function(term) {
    w <- setNames(rep(0, length(b)), names(b)); w[c(time, term)] <- 1
    tibble(term = paste("Implied slope:", sub(".*friend_group", "", term)),
           estimate = sum(w * b), se = sqrt(drop(t(w) %*% V %*% w)))
  })

  # Wald test that both interactions are zero (common slope)
  wald <- lfe::waldtest(model, interactions, type = "cluster")

  summary(model)$coefficients %>%
    as_tibble(rownames = "term") %>%
    transmute(term, estimate = Estimate, se = `Cluster s.e.`) %>%
    bind_rows(implied) %>%
    bind_rows(tibble(term = c("Wald chi2(2)", "Wald p-value", "Directed allocations"),
                     estimate = c(wald[["chi2"]], wald[["p"]], model$N))) %>%
    mutate(model = name)
})

slope_table %>%
  mutate(value = if_else(is.na(se), sprintf("%.5g", estimate), sprintf("%.3f (%.3f)", estimate, se))) %>%
  select(model, term, value) %>%
  pivot_wider(names_from = model, values_from = value)


## ---- s5-decomposition ----
# Figure S12A: is the rise from the first to the final fifth due to more
# giving within each group, or to a change in the mix of groups?
first_last <- dyads %>% filter(fifth %in% c(1, 5)) %>%
  mutate(period = if_else(fifth == 1, "E", "L"))

cells <- first_last %>%
  group_by(friend_group, period) %>%
  summarise(mean = mean(tokens_sent), n = n(), .groups = "drop") %>%
  group_by(period) %>% mutate(share = n / sum(n)) %>% ungroup() %>%
  pivot_wider(id_cols = friend_group, names_from = period, values_from = c(mean, share))

decomposition <- cells %>%
  mutate(within      = (share_L + share_E) / 2 * (mean_L - mean_E),
         composition = (mean_L + mean_E) / 2 * (share_L - share_E))

decomposition
c(total = sum(decomposition$within + decomposition$composition),
  within = sum(decomposition$within), composition = sum(decomposition$composition))


## ---- s5-within-child ----
# Within the same child: did giving rise more toward classmates she named
# (A = mutual or sender-only) than toward the others (N)?
child_cells <- first_last %>%
  mutate(receiver_type = if_else(friendship %in% c("Mutual", "Sender declares"), "A", "N")) %>%
  group_by(class_id, sender_id, receiver_type, period) %>%
  summarise(mean_tokens = mean(tokens_sent), mean_gap = mean(rank_gap),
            share_mutual = mean(friendship == "Mutual"),
            share_receiver_only = mean(friendship == "Receiver declares"),
            decisions = n(), .groups = "drop")

# keep children observed in the four cells
complete_children <- child_cells %>% count(sender_id) %>% filter(n == 4) %>% pull(sender_id)

child_level <- child_cells %>%
  filter(sender_id %in% complete_children) %>%
  pivot_wider(id_cols = c(class_id, sender_id), names_from = c(receiver_type, period),
              values_from = c(mean_tokens, mean_gap, share_mutual, share_receiver_only)) %>%
  transmute(class_id, sender_id,
            change_A = mean_tokens_A_L - mean_tokens_A_E,
            change_N = mean_tokens_N_L - mean_tokens_N_E,
            D        = change_A - change_N,
            d_gap_A  = mean_gap_A_L - mean_gap_A_E,
            d_gap_N  = mean_gap_N_L - mean_gap_N_E,
            d_mutual_A        = share_mutual_A_L - share_mutual_A_E,
            d_receiver_only_N = share_receiver_only_N_L - share_receiver_only_N_E)

c(children = nrow(child_level), classrooms = n_distinct(child_level$class_id),
  change_A = mean(child_level$change_A), change_N = mean(child_level$change_N),
  D = mean(child_level$D))

within_child_models <- function(data) {
  list(lm(D ~ 1, data = data),
       lm(D ~ d_gap_A + d_gap_N, data = data),
       lm(D ~ d_gap_A + d_gap_N + d_mutual_A + d_receiver_only_N, data = data))
}

# Classroom bootstrap: resample classrooms with replacement (4,999 times),
# keeping each child's four cells together
set.seed(4999)
classrooms <- unique(child_level$class_id)
bootstrap <- map_dfr(1:4999, function(b) {
  sampled <- tibble(class_id = sample(classrooms, replace = TRUE)) %>%
    left_join(child_level, by = "class_id", relationship = "many-to-many")
  fits <- within_child_models(sampled)
  tibble(draw = b, mean_D = mean(sampled$D),
         model = 1:3, intercept = map_dbl(fits, ~ coef(.x)[1]),
         coefficients = map(fits, coef))
})

bootstrap %>% filter(model == 1) %>%
  summarise(ci_low = quantile(mean_D, 0.025), ci_high = quantile(mean_D, 0.975))


## ---- s5-within-child-models ----
table_s15 <- map2_dfr(within_child_models(child_level), 1:3, function(fit, m) {
  draws <- bootstrap %>% filter(model == m) %>% pull(coefficients) %>% do.call(rbind, .)
  tibble(model = m, term = names(coef(fit)), estimate = coef(fit),
         bootstrap_se = apply(draws, 2, sd),
         ci_low = apply(draws, 2, quantile, 0.025), ci_high = apply(draws, 2, quantile, 0.975),
         r2 = summary(fit)$r.squared)
})

table_s15 %>% mutate(across(where(is.double), ~ round(.x, 3)))


## ---- s5-figure-s12 ----
# Figure S12: (A) the decomposition; (B) within-child means toward
# nominated (A) and other (N) receivers in the first and final fifths
figure_s12_a <- decomposition %>%
  select(friend_group, `Within categories` = within, `Category shares` = composition) %>%
  pivot_longer(-friend_group, names_to = "component") %>%
  ggplot(aes(component, value, fill = friend_group)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = friend_colors) +
  labs(title = "A. Change in mean giving, first to final fifth", x = NULL, y = "Tokens", fill = NULL) +
  theme_paper(9) + theme(legend.position = "bottom")

figure_s12_b <- child_level %>%
  summarise(`Nominated (A)` = mean(change_A), `Other (N)` = mean(change_N), Difference = mean(D)) %>%
  pivot_longer(everything()) %>%
  mutate(name = fct_inorder(name)) %>%
  ggplot(aes(name, value)) +
  geom_col(fill = c("#00A388", "grey60", "#C95A3A"), width = 0.6) +
  labs(title = "B. Within-child change (216 children)", x = NULL, y = "Tokens") +
  theme_paper(9)

figure_s12 <- figure_s12_a | figure_s12_b
figure_s12


## ---- s5-class-size ----
# Figure S13: by classroom size group, and classroom-specific slopes
size_groups <- dyads %>%
  mutate(size_group = cut(class_size, c(0, 15, 21, 39), labels = c("Small (7-15)", "Medium (16-21)", "Large (22-39)"))) %>%
  mean_ci(fifth, size_group)

class_slopes <- dyads %>%
  group_by(class_id, class_size) %>%
  group_modify(~ tibble(slope = coef(felm(tokens_sent ~ progress | sender_id_f + receiver_id_f, data = .x))[["progress"]])) %>%
  ungroup()

cor.test(class_slopes$slope, class_slopes$class_size)

figure_s13 <- (ggplot(size_groups, aes(fifth, mean, color = size_group)) +
                 geom_ribbon(aes(ymin = low, ymax = high, fill = size_group), alpha = 0.15, color = NA) +
                 geom_line() + geom_point() +
                 labs(title = "A. By classroom size", x = "Fifth of personal progress",
                      y = "Mean tokens sent", color = NULL, fill = NULL) +
                 theme_paper(9) + theme(legend.position = "bottom")) |
              (ggplot(class_slopes, aes(class_size, slope)) +
                 geom_hline(yintercept = 0, linetype = "dashed") +
                 geom_point(color = "grey30") +
                 geom_smooth(method = "lm", color = "#C95A3A") +
                 labs(title = "B. Classroom-specific change", x = "Classroom size",
                      y = "First-to-last change (tokens)") +
                 theme_paper(9))
figure_s13


## ---- s5-pacing ----
# Figure S14: seconds between two consecutive decisions of the same child,
# in the 29 classrooms where the time of each decision was recorded
intervals <- dyads %>%
  filter(!is.na(seconds_since_start)) %>%
  group_by(sender_id) %>%
  arrange(decision_order, .by_group = TRUE) %>%
  mutate(seconds = seconds_since_start - lag(seconds_since_start),
         decile  = pmin(pmax(ceiling(progress * 10), 1), 10)) %>%
  ungroup() %>%
  filter(!is.na(seconds))

c(intervals = nrow(intervals), children = n_distinct(intervals$sender_id),
  classrooms = n_distinct(intervals$class_id), median = median(intervals$seconds))

pacing <- intervals %>%
  group_by(decile) %>%
  summarise(median = median(seconds), q25 = quantile(seconds, 0.25), q75 = quantile(seconds, 0.75))

pacing

figure_s14 <- ggplot(pacing, aes(decile, median)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "grey85") +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Decile of the child's decision sequence", y = "Seconds since previous decision") +
  theme_paper(10)
figure_s14


## ---- s5-save ----
save_table(table_s11_a, "S5_table_s11a_schedule")
save_table(table_s11_b, "S5_table_s11b_temporal_models")
save_table(table_s12, "S5_table_s12_game")
save_table(coverage, "S5_table_s13_coverage")
save_table(slope_table, "S5_table_s14_friendship_slopes")
save_table(decomposition, "S5_decomposition")
save_table(table_s15, "S5_table_s15_within_child")
save_table(pacing, "S5_pacing")
save_si_figure(figure_s9,  "S5_figure_s9_game",           9, 7)
save_si_figure(figure_s10, "S5_figure_s10_time",          9, 6)
save_si_figure(figure_s11, "S5_figure_s11_time_friendship", 9, 6.5)
save_si_figure(figure_s12, "S5_figure_s12_decomposition", 9, 3.8)
save_si_figure(figure_s13, "S5_figure_s13_class_size",    9, 3.6)
save_si_figure(figure_s14, "S5_figure_s14_pacing",        6, 3.5)
