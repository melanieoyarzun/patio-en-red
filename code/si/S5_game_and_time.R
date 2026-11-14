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


## ---- s5-figure-s9-panels ----
# Figure S9: (A) tokens sent, (B) the two allocations of each pair,
# (C) the Cooperation GAP, with zero gaps in orange, (D) what zero gaps are made of.
# Each panel is built on its own and the four are assembled at the end.
# large centered titles, bold axis titles, dark left and bottom axes
theme_s9 <- theme_si(9) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        plot.title.position = "panel",
        axis.title = element_text(face = "bold"),
        axis.line = element_line(color = "grey15", linewidth = 0.5))
percent_axis <- scale_y_continuous(labels = function(x) paste0(x, "%"),
                                   expand = expansion(mult = c(0, 0.05)))

panel_a <- dyads %>%
  count(tokens_sent) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ggplot(aes(tokens_sent, percent)) +
  geom_col(fill = color_slate, width = 0.8) +
  scale_x_continuous(breaks = 0:10) + percent_axis +
  labs(title = "A. Tokens sent", x = "Tokens sent", y = "Directed decisions (%)") +
  theme_s9

pair_cells <- pairs %>%
  count(high = pmax(tokens_sent, tokens_received), low = pmin(tokens_sent, tokens_received)) %>%
  mutate(percent = 100 * n / sum(n))

panel_b <- ggplot(pair_cells, aes(high, low, fill = percent)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f", percent)), size = 1.9, fontface = "bold",
            color = if_else(pair_cells$percent > 8, "white", "grey15")) +
  scale_fill_gradientn(colors = c("#FFF4D0", "#F2C66D", "#E39A55", "#C95A3A"),
                       name = "% of interactions") +
  scale_x_continuous(breaks = 0:10, expand = c(0, 0)) +
  scale_y_continuous(breaks = 0:10, expand = c(0, 0)) +
  coord_fixed() +
  labs(title = "B. Bivariate token distribution",
       x = "Tokens sent from j to i", y = "Tokens sent from i to j") +
  theme_s9 +
  theme(panel.grid = element_blank(), panel.border = element_blank(),
        legend.position = c(0.25, 0.85), legend.direction = "horizontal",
        legend.title.align = 0, legend.key.height = unit(2, "mm"),
        legend.title = element_text(face = "bold", size = 7), legend.text = element_text(size = 6)) +
  guides(fill = guide_colorbar(title.position = "top"))

gap_bars <- dyads %>%
  mutate(kind = case_when(coop_gap == 0 & tokens_sent == 0 ~ "0-0",
                          coop_gap == 0                    ~ "Equal positive",
                          TRUE                             ~ "Unequal")) %>%
  count(coop_gap, kind) %>%
  mutate(percent = 100 * n / sum(n),
         kind = factor(kind, levels = c("Equal positive", "0-0", "Unequal")))

panel_c <- ggplot(gap_bars, aes(coop_gap, percent, fill = kind)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("Equal positive" = color_highlight, "0-0" = "#E8A06E",
                               Unequal = color_slate)) +
  scale_x_continuous(breaks = seq(-10, 10, 2)) + percent_axis +
  labs(title = "C. Cooperation GAP", x = "Tokens received - tokens sent",
       y = "Directed decisions (%)") +
  theme_s9

zero_gap <- pairs %>%
  filter(coop_gap == 0) %>%
  mutate(friend_group = if_else(friendship %in% c("Sender declares", "Receiver declares"),
                                "Asymmetric ties", as.character(friend_group)),
         friend_group = factor(friend_group, levels = c("Asymmetric ties", "Mutual friends", "Non-friends")))

zero_bars <- zero_gap %>%
  count(tokens_sent, friend_group) %>%
  mutate(percent = 100 * n / nrow(zero_gap)) %>%
  group_by(tokens_sent) %>%
  mutate(share = 100 * n / sum(n)) %>%
  ungroup()

share_zero <- 100 * mean(zero_gap$tokens_sent == 0)

panel_d <- ggplot(zero_bars, aes(tokens_sent, percent, fill = friend_group)) +
  geom_col(width = 0.8, color = "white", linewidth = 0.2) +
  # share of each friendship type inside the tall columns
  geom_text(data = filter(zero_bars, percent > 6),
            aes(label = paste0(round(share), "%")), position = position_stack(vjust = 0.5),
            angle = 90, color = "white", fontface = "bold", size = 2.4) +
  annotate("segment", x = -0.4, xend = 0.4, y = 55, yend = 55, color = "#E8A06E", linewidth = 1) +
  annotate("segment", x = 0.6, xend = 10.4, y = 55, yend = 55, color = color_highlight, linewidth = 1) +
  annotate("text", x = 0, y = 59, size = 2.4, fontface = "bold", color = "#E8A06E",
           label = sprintf("0-0\n%.1f%%", share_zero)) +
  annotate("text", x = 5.5, y = 59, size = 2.4, fontface = "bold", color = color_highlight,
           label = sprintf("Equal positive\n%.1f%%", 100 - share_zero)) +
  scale_fill_manual(values = c("Non-friends" = "#9B86C9", "Mutual friends" = "#7FCDBB",
                               "Asymmetric ties" = "#8FD3EC"), name = "Friendship type") +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 25, 50),
                     expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(ylim = c(0, 63)) +
  labs(title = "D. Zero GAP decomposition", x = "Tokens sent by both students",
       y = "Zero-GAP dyads (%)") +
  theme_s9 +
  # legend as colored text, as in the SI
  annotate("text", x = -0.4, y = c(48.5, 44.5, 41, 37.5), hjust = 0, fontface = "bold",
           size = c(3.1, 2.9, 2.9, 2.9),
           label = c("Friendship type", "Non-friends", "Mutual friends", "Asymmetric ties"),
           color = c("grey15", "#9B86C9", "#7FCDBB", "#8FD3EC"))

## ---- s5-figure-s9 ----
figure_s9 <- (panel_a | panel_b) / (panel_c | panel_d) & plot_layout(widths = c(1, 1))
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


## ---- s5-figure-s10-panels ----
# Figure S10: mean tokens sent by classroom round, rounds 1-7, personal
# decision order and personal session progress. Point size = classrooms
# behind each mean. Panels A-C share one drawing, written as a function;
# each panel is built on its own and the four are assembled at the end.
progress_labels <- c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")

time_panel <- function(data, x, title, xlab) {
  ggplot(data, aes({{ x }}, mean)) +
    geom_ribbon(aes(ymin = low, ymax = high), fill = color_slate, alpha = 0.18) +
    geom_line(color = color_slate, linewidth = 0.6) +
    geom_point(aes(size = classrooms), color = color_slate) +
    scale_size_continuous(range = c(0.3, 1.8)) +
    labs(title = title, x = xlab, y = "Mean tokens sent") +
    theme_si(8)
}

s10_a <- time_panel(by_round, round, "A. Mean tokens sent by classroom game round",
                    "Classroom game round")

s10_b <- time_panel(filter(by_round, round <= 7), round, "B. Common-support rounds 1-7",
                    "Classroom game round") +
  scale_x_continuous(breaks = 1:7)

# dotted line at the sixth decision
s10_c <- time_panel(by_order, decision_order, "C. Personal decision order",
                    "Personal decision order") +
  geom_vline(xintercept = 6, linetype = "dotted", color = "grey60") +
  scale_x_continuous(breaks = c(1, 6, 10, 20, 30, 38))

# fifths of each child's own sequence: means with 95% CI
s10_d <- ggplot(by_fifth, aes(fifth, mean)) +
  geom_line(color = color_slate, linewidth = 0.6) +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.08, color = color_slate) +
  geom_point(color = color_slate, size = 1.8) +
  scale_x_continuous(breaks = 1:5, labels = progress_labels) +
  labs(title = "D. Personal session progress", x = "Personal session progress",
       y = "Mean tokens sent") +
  theme_si(8)

## ---- s5-figure-s10 ----
figure_s10 <- (s10_a | s10_b) / (s10_c | s10_d) +
  plot_annotation(title = "Giving over classroom game rounds and personal progress",
                  theme = theme(plot.title = element_text(face = "bold", hjust = 0.5)))
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


## ---- s5-figure-s11-panels ----
# Figure S11: the four views of Figure S10 by friendship group, with the
# overall mean as a dashed gray line. The four panels share one drawing,
# written as a function; each panel is built on its own and the four are
# assembled at the end.
friend_colors <- c("Non-friends" = color_non_friends, "Asymmetric ties" = color_asymmetric,
                   "Mutual friends" = color_mutual, "Overall" = "#9AA0B4")

# each group plus the overall mean (dashed gray)
with_overall <- function(by_group, overall) {
  bind_rows(by_group, overall %>% mutate(friend_group = "Overall")) %>%
    mutate(friend_group = factor(friend_group, levels = names(friend_colors)))
}

group_panel <- function(data, x, title, xlab, ribbon = TRUE) {
  ggplot(data, aes({{ x }}, mean, color = friend_group, fill = friend_group)) +
    geom_ribbon(aes(ymin = low, ymax = high), alpha = if (ribbon) 0.12 else 0, color = NA) +
    geom_line(aes(linetype = friend_group == "Overall"), linewidth = 0.6) +
    geom_point(aes(size = classrooms)) +
    scale_color_manual(values = friend_colors, name = "Friendship nomination type") +
    scale_fill_manual(values = friend_colors, guide = "none") +
    scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "dashed"), guide = "none") +
    scale_size_continuous(range = c(0.3, 1.6), guide = "none") +
    labs(title = title, x = xlab, y = "Mean tokens sent") +
    theme_si(8)
}

# legend inside panel A, in two columns
s11_a <- group_panel(with_overall(by_round_f, by_round), round,
                     "A. Mean tokens sent by classroom game round", "Classroom game round") +
  coord_cartesian(ylim = c(0, 10.5)) +
  theme(legend.position = c(0.3, 0.2), legend.title = element_text(size = 7),
        legend.text = element_text(size = 6), legend.key.size = unit(3, "mm")) +
  guides(color = guide_legend(ncol = 2))

s11_b <- group_panel(with_overall(filter(by_round_f, round <= 7), filter(by_round, round <= 7)),
                     round, "B. Common-support rounds 1-7", "Classroom game round") +
  scale_x_continuous(breaks = 1:7)

s11_c <- group_panel(with_overall(by_order_f, by_order), decision_order,
                     "C. Personal decision order", "Personal decision order") +
  geom_vline(xintercept = 6, linetype = "dotted", color = "grey60") +
  scale_x_continuous(breaks = c(1, 6, 10, 20, 30, 38)) +
  coord_cartesian(ylim = c(0, 10.5))

# fifths: error bars instead of a band
s11_d <- group_panel(with_overall(by_fifth_f, by_fifth), fifth,
                     "D. Personal session progress", "Personal session progress",
                     ribbon = FALSE) +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.08) +
  scale_x_continuous(breaks = 1:5, labels = progress_labels)

## ---- s5-figure-s11 ----
figure_s11 <- (s11_a | s11_b) / (s11_c | s11_d) +
  plot_annotation(title = "Giving over the session by friendship nomination type",
                  theme = theme(plot.title = element_text(face = "bold", hjust = 0.5)))
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


## ---- s5-figure-s12-data ----
# Figure S12: (A) aggregate change and its two components; (B) mean giving
# of the same children toward the classmates they named and the others, in the
# first and final fifths; (C) the within-child changes with classroom-bootstrap CIs
# within-child means (first and final fifth) toward nominated (A) and other (N) classmates
child_means <- child_cells %>%
  filter(sender_id %in% complete_children) %>%
  group_by(receiver_type, period) %>%
  summarise(mean = mean(mean_tokens), se = sd(mean_tokens) / sqrt(n()), .groups = "drop") %>%
  mutate(receiver_type = if_else(receiver_type == "A", "Sender-nominated", "Not sender-nominated"),
         period = if_else(period == "E", "First 20%", "Final 20%"),
         period = factor(period, levels = c("First 20%", "Final 20%")))

# classroom bootstrap of the three child-level means
set.seed(999)
change_draws <- map_dfr(1:999, function(b) {
  sampled <- tibble(class_id = sample(classrooms, replace = TRUE)) %>%
    left_join(child_level, by = "class_id", relationship = "many-to-many")
  tibble(reference = mean(sampled$change_N), additional = mean(sampled$D), total = mean(sampled$change_A))
})

change_components <- tibble(
  term = factor(c("Reference\n(not sender-\nnominated)", "Additional\nchange toward\nnominated",
                  "Total change\ntoward\nnominated"),
                levels = rev(c("Reference\n(not sender-\nnominated)", "Additional\nchange toward\nnominated",
                               "Total change\ntoward\nnominated"))),
  estimate = c(mean(child_level$change_N), mean(child_level$D), mean(child_level$change_A)),
  low  = map_dbl(change_draws, quantile, 0.025),
  high = map_dbl(change_draws, quantile, 0.975),
  highlight = c(FALSE, TRUE, FALSE))

## ---- s5-figure-s12-panels ----
# Shared look of the three panels: left-aligned titles over the panel
theme_s12 <- function(l_axes = FALSE) {
  theme_si_frame(base_size = 8, l_axes = l_axes) +
    theme(plot.title = element_text(hjust = 0), plot.subtitle = element_text(hjust = 0),
          plot.title.position = "plot")
}

n_children <- n_distinct(dyads$sender_id)

# Panel A: aggregate change in mean giving and its two components
group_colors <- friend_colors[c("Non-friends", "Asymmetric ties", "Mutual friends")]
overall_change <- sum(decomposition$within + decomposition$composition)
rows <- c("Category shares", "Category means", "Overall\nchange")

components <- decomposition %>%
  select(friend_group, `Category means` = within, `Category shares` = composition) %>%
  pivot_longer(-friend_group, names_to = "component") %>%
  # stacked from left to right: asymmetric ties, non-friends, mutual friends
  mutate(friend_group = factor(friend_group, levels = c("Mutual friends", "Non-friends", "Asymmetric ties")),
         component = factor(component, levels = rows))

component_totals <- components %>% group_by(component) %>% summarise(total = sum(value)) %>%
  add_row(component = "Overall\nchange", total = overall_change) %>%
  mutate(component = factor(component, levels = rows))

# small box with the contribution of each group to the composition term
composition <- components %>%
  filter(component == "Category shares") %>%
  mutate(friend_group = factor(friend_group, levels = c("Non-friends", "Mutual friends", "Asymmetric ties"))) %>%
  arrange(friend_group) %>%
  mutate(y = 1.33 - 0.13 * row_number())

figure_s12_a <- ggplot() +
  geom_col(data = filter(component_totals, component == "Overall\nchange"),
           aes(total, component), fill = color_slate, width = 0.55) +
  geom_col(data = components, aes(value, component, fill = friend_group), width = 0.55) +
  geom_text(data = filter(components, component == "Category means"),
            aes(value, component, label = sprintf("%.3f", value), group = friend_group),
            position = position_stack(vjust = 0.5), color = "white", size = 2.1, fontface = "bold") +
  geom_point(data = component_totals, aes(total, component), size = 1) +
  geom_text(data = component_totals, aes(pmax(total, 0.06), component, label = sprintf("%+.3f", total)),
            hjust = -0.25, size = 2.3) +
  annotate("text", x = 0.88, y = 1.45, label = "Composition contributions", hjust = 1, size = 2.2) +
  geom_text(data = composition, aes(0.66, y, label = friend_group, color = friend_group),
            hjust = 1, size = 2.2, fontface = "bold") +
  geom_text(data = composition, aes(0.88, y, label = sprintf("%+.3f", value), color = friend_group),
            hjust = 1, size = 2.2, fontface = "bold") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey20") +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(limits = c(-0.12, 1), breaks = seq(0, 0.8, 0.2), expand = c(0, 0)) +
  scale_y_discrete(limits = rows, labels = function(x) sub("Category ", "Category\n", x)) +
  labs(title = "A. Aggregate change components",
       subtitle = sprintf("%s children | 45 classrooms | Equal weight per decision", n_children),
       x = "Change in mean tokens sent", y = NULL) +
  theme_s12(l_axes = TRUE)

# Panel B: giving of the same children toward the classmates they named and
# toward the others. The dashed line is the counterfactual: giving to named
# classmates if it had changed as much as giving to the others.
types <- c("Sender-nominated", "Not sender-nominated")
child_means <- child_means %>% mutate(receiver_type = factor(receiver_type, levels = types))
value_at <- function(type, when) child_means$mean[child_means$receiver_type == type &
                                                    as.integer(child_means$period) == when]
counterfactual <- value_at(types[1], 1) + value_at(types[2], 2) - value_at(types[2], 1)
observed_final <- value_at(types[1], 2)
extra <- tibble(x = c(1, 2), y = c(value_at(types[1], 1), counterfactual))

figure_s12_b <- ggplot(child_means, aes(as.integer(period), mean, color = receiver_type,
                                        group = receiver_type)) +
  geom_line(data = extra, aes(x, y), inherit.aes = FALSE, linetype = "dashed", color = "grey20") +
  geom_point(data = extra[2, ], aes(x, y), inherit.aes = FALSE, shape = 21, fill = "white",
             color = "grey20", size = 1.8) +
  geom_line(linewidth = 0.7) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.06) +
  geom_point(aes(shape = receiver_type), size = 2, fill = "white") +
  annotate("segment", x = 2.09, xend = 2.09, y = counterfactual, yend = observed_final,
           color = color_highlight, linewidth = 0.6) +
  annotate("text", x = 2.12, y = (counterfactual + observed_final) / 2,
           label = sprintf("%+.2f", observed_final - counterfactual),
           color = color_highlight, hjust = 0, size = 2.3) +
  scale_color_manual(values = setNames(c(color_highlight, color_slate), types), name = NULL) +
  scale_shape_manual(values = setNames(c(16, 21), types), name = NULL) +
  scale_x_continuous(breaks = 1:2, labels = levels(child_means$period), limits = c(0.95, 2.25)) +
  scale_y_continuous(limits = c(3, 8)) +
  labs(title = "B. Giving within the same children",
       subtitle = sprintf("%d children | %d classrooms | Equal weight per child",
                          nrow(child_level), n_distinct(child_level$class_id)),
       x = "Personal session progress", y = "Mean tokens sent") +
  theme_s12() +
  theme(legend.position = c(0.68, 0.1), legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size = 6.5), legend.background = element_blank())

# Panel C: within-child changes with classroom-bootstrap 95% CIs
figure_s12_c <- ggplot(change_components, aes(estimate, term, color = highlight)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey20") +
  geom_errorbarh(aes(xmin = low, xmax = high), height = 0.15) +
  geom_point(size = 1.8) +
  geom_text(aes(x = high, label = sprintf("%+.2f", estimate)), hjust = -0.2, size = 2.3) +
  scale_color_manual(values = c(`FALSE` = "grey20", `TRUE` = color_highlight)) +
  scale_x_continuous(limits = c(-0.1, 2.6), breaks = seq(0, 2.5, 0.5)) +
  labs(title = "C. Within-child change components",
       subtitle = sprintf("%d children | First vs. final 20%% of personal session progress", nrow(child_level)),
       x = "Mean within-child change (tokens)", y = NULL) +
  theme_s12()

## ---- s5-figure-s12 ----
figure_s12 <- figure_s12_a | figure_s12_b | figure_s12_c
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


## ---- s5-figure-s13-panels ----
# Figure S13: (A) giving over the session by classroom size; (B) classroom-
# specific trends against classroom size. Each panel is built on its own and
# the two are assembled at the end.
size_colors <- c("Small (7-15)" = "#5BA4B5", "Medium (16-21)" = "#2E7F8C", "Large (22-39)" = "#173F45")

s13_a <- ggplot(size_groups, aes(fifth, mean, color = size_group)) +
  geom_ribbon(aes(ymin = low, ymax = high, fill = size_group), alpha = 0.15, color = NA) +
  geom_line() + geom_point(size = 1.2) +
  scale_color_manual(values = size_colors, name = NULL) +
  scale_fill_manual(values = size_colors, guide = "none") +
  scale_x_continuous(breaks = 1:5, labels = progress_labels) +
  labs(title = "A. Personal session progress by classroom size",
       x = "Personal session progress", y = "Mean tokens sent") +
  theme_si(8) + theme(legend.position = c(0.75, 0.15))

# linear fit in orange
s13_b <- ggplot(class_slopes, aes(class_size, slope)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_smooth(method = "lm", color = color_highlight, fill = color_highlight,
              alpha = 0.15, linewidth = 0.6) +
  geom_point(color = color_slate) +
  labs(title = "B. Classroom-specific trends and class size",
       x = "Participants in classroom", y = "First-to-last change in tokens sent") +
  theme_si(8)

## ---- s5-figure-s13 ----
figure_s13 <- (s13_a | s13_b) +
  plot_annotation(title = "Temporal sending patterns and classroom size",
                  theme = theme(plot.title = element_text(face = "bold", hjust = 0.5)))
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


## ---- s5-figure-s14-data ----
# Figure S14: median interval (and interquartile range) by decile of the
# child's decision sequence, for all decisions and by relationship
pacing_by_group <- bind_rows(
  intervals %>% mutate(panel = "A. All relations"),
  intervals %>% mutate(panel = c("Non-friends" = "B. Non-friends", "Asymmetric ties" = "C. Asymmetric ties",
                                 "Mutual friends" = "D. Mutual friends")[as.character(friend_group)])
) %>%
  group_by(panel, decile) %>%
  summarise(median = median(seconds), q25 = quantile(seconds, 0.25), q75 = quantile(seconds, 0.75),
            .groups = "drop")

## ---- s5-figure-s14 ----
# One panel per relationship, each in its color: median interval and IQR band
panel_colors <- c("A. All relations" = color_slate, "B. Non-friends" = color_non_friends,
                  "C. Asymmetric ties" = color_asymmetric, "D. Mutual friends" = color_mutual)
decile_labels <- paste0((0:9) * 10, "-", (1:10) * 10)

figure_s14 <- ggplot(pacing_by_group, aes(decile, median, color = panel, fill = panel)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.18, color = NA) +
  geom_line(linewidth = 0.7) + geom_point(size = 1) +
  facet_wrap(~ panel, ncol = 2) +
  scale_color_manual(values = panel_colors) + scale_fill_manual(values = panel_colors) +
  scale_x_continuous(breaks = 1:10, labels = decile_labels) +
  scale_y_continuous(limits = c(0, 125), breaks = seq(0, 120, 20)) +
  labs(title = "Decision pacing over the session", x = "Within-session progress decile (%)",
       y = "Interval (seconds)") +
  theme_si(8) +
  theme(plot.title = element_text(hjust = 0.5), plot.title.position = "panel",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
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
save_si_figure(figure_s12, "S5_figure_s12_decomposition", 11, 3.8)
save_si_figure(figure_s13, "S5_figure_s13_class_size",    9, 4)
save_si_figure(figure_s14, "S5_figure_s14_pacing",        8, 5.5)
