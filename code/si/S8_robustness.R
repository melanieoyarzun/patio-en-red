# =============================================================================
# S8_robustness.R   SI Section 8: Global robustness and sensitivity analyses
#
#   8.1  Null-model filtered tokens: Tables S25-S30, Figure S25
#   8.2  Filtering threshold and cutoff definition: Tables S31-S35
#   8.3  Nomination limit: Table S36, Figure S26
#   8.4  Classroom size: Figure S27
#   8.5  Temporal robustness: Table S37
#   8.6  Lagged Social Rank GAP placebo: Tables S38-S39
#   8.7  Sex and grade level: Tables S40-S51
#   8.8  Alternative Social Rank constructions: Figure S28
#   8.9  Rank measures from affiliation networks: Table S52
#
#   Input : data/generated/analysis_dyads.csv, data/generated/analysis_students.csv,
#           data/generated/alternative_ranks.csv (from S4)
#   Output: output/tables/si/S8_*.csv, output/figures/si/S8_*.(pdf|png)
# =============================================================================

## ---- s8-setup ----
source(here::here("code", "00_helpers.R"))
source(here("code", "00_style.R"))
library(igraph)
library(patchwork)
dir.create(here("output", "figures", "si"), recursive = TRUE, showWarnings = FALSE)

dyads    <- read_csv(here("data", "generated", "analysis_dyads.csv"),    show_col_types = FALSE)
students <- read_csv(here("data", "generated", "analysis_students.csv"), show_col_types = FALSE)

dyads <- dyads %>%
  mutate(class_round = paste(class_id, round, sep = "_"),
         mutual = as.integer(friendship == "Mutual"))

save_si_figure <- function(plot, name, width, height) {
  ggsave(here("output", "figures", "si", paste0(name, ".pdf")), plot, width = width, height = height)
  ggsave(here("output", "figures", "si", paste0(name, ".png")), plot, width = width, height = height, dpi = 300)
}

# The preferred model (sender + classroom-by-round FE, SE by classroom) for
# any outcome and right-hand side. Returns NULL when it cannot be estimated.
preferred_model <- function(data, outcome = "coop_gap", rhs = "rank_gap",
                            fixed_effects = "sender_id + class_round") {
  data <- data %>% mutate(across(any_of(c("sender_id", "class_id", "class_round", "round")), as.factor))
  quiet_felm(as.formula(paste(outcome, "~", rhs, "|", fixed_effects, "| 0 | class_id")), data)
}

# Coefficient, SE and p of one term. NA = "not estimable" (n.e.): the model
# failed, the SE is not finite, or fewer than 10 residual degrees of freedom
# remain after the fixed effects (felm's df, which is also capped at the
# number of classrooms - 1).
term_estimate <- function(model, term = "rank_gap") {
  if (is.null(model) || model$df < 10) return(tibble(estimate = NA_real_, se = NA_real_, p_value = NA_real_))
  ct <- quiet_summary(model)$coefficients
  se_col <- if ("Cluster s.e." %in% colnames(ct)) "Cluster s.e." else "Std. Error"
  if (!term %in% rownames(ct) || !is.finite(ct[term, se_col]))
    return(tibble(estimate = NA_real_, se = NA_real_, p_value = NA_real_))
  tibble(estimate = ct[term, "Estimate"], se = ct[term, se_col], p_value = ct[term, "Pr(>|t|)"])
}


## ---- s8-filtered-tables ----
# 8.1 The main tables again, with the null-model filtered Cooperation GAP
# (only pairs where both transfers survive the filter; see 01_build_measures.R)
filtered <- dyads %>% filter(!is.na(coop_gap_filtered))
cat("Decisions with both directions kept by the filter:", nrow(filtered), "\n")

fe_filtered_all    <- fe_progression(filtered, outcome = "coop_gap_filtered")
fe_filtered_non    <- fe_progression(filter(filtered, friendship == "Non-friends"), outcome = "coop_gap_filtered")
fe_filtered_mutual <- fe_progression(filter(filtered, friendship == "Mutual"), outcome = "coop_gap_filtered")
fe_filtered_pooled <- fe_progression(filter(filtered, friendship %in% c("Non-friends", "Mutual")),
                                     outcome = "coop_gap_filtered", formula_rhs = "rank_gap * mutual")

regression_table(fe_filtered_all,    terms = "rank_gap")

## ---- s8-filtered-non ----
regression_table(fe_filtered_non,    terms = "rank_gap")

## ---- s8-filtered-mutual ----
regression_table(fe_filtered_mutual, terms = "rank_gap")

## ---- s8-filtered-pooled ----
regression_table(fe_filtered_pooled, terms = c("rank_gap", "mutual", "rank_gap:mutual"))


## ---- s8-filtered-low-to-high ----
filtered_asymmetric <- filtered %>%
  filter(friendship == "Sender declares") %>%
  mutate(high_to_low = as.integer(rank_gap >= 0))

fe_filtered_lh <- fe_progression(filter(filtered_asymmetric, high_to_low == 0), outcome = "coop_gap_filtered")

regression_table(fe_filtered_lh, terms = "rank_gap")

## ---- s8-filtered-high-to-low ----
filtered_high_to_low <- filter(filtered_asymmetric, high_to_low == 1)
fe_filtered_hl <- fe_progression(filtered_high_to_low, outcome = "coop_gap_filtered")

# A saturated fixed-effects design leaves no variation for the rank gap.
fixed_effects_4 <- model.matrix(~ factor(class_id) + factor(sender_id) + factor(round),
                               data = filtered_high_to_low)
fixed_effects_5 <- model.matrix(~ factor(sender_id) + factor(class_round),
                               data = filtered_high_to_low)
if (qr(fixed_effects_4)$rank == nrow(filtered_high_to_low)) fe_filtered_hl["(4)"] <- list(NULL)
if (qr(fixed_effects_5)$rank == nrow(filtered_high_to_low)) fe_filtered_hl["(5)"] <- list(NULL)

regression_table(fe_filtered_hl, terms = "rank_gap")

## ---- s8-filtered-contrast ----
# Table S30: H-to-L minus L-to-H slope on the current filtered sample.
contrast_data <- filtered_asymmetric %>%
  mutate(across(c(sender_id, class_id, class_round, round),
                ~ interaction(.x, high_to_low, drop = TRUE), .names = "{.col}_direction"),
         across(c(sender_id, class_id, class_round, round), factor))

contrast_fe <- list(
  "None" = character(),
  "Classroom" = "class_id",
  "Sender + classroom" = c("sender_id", "class_id"),
  "Sender + classroom + round" = c("sender_id", "class_id", "round"),
  "Sender + classroom-by-round" = c("sender_id", "class_round")
)

contrast_models <- function(effects, separate) {
  has_fe <- length(effects) > 0L
  if (has_fe && separate) effects <- paste0(effects, "_direction")
  fe <- if (has_fe) paste(effects, collapse = " + ") else "0"
  base <- if (has_fe && separate) "rank_gap" else "rank_gap + high_to_low"
  as_formula <- function(rhs) as.formula(paste("coop_gap_filtered ~", rhs, "|", fe, "| 0 | class_id"))
  list(full = as_formula(paste(base, "+ rank_gap:high_to_low")), restricted = as_formula(base))
}

filtered_contrast_row <- function(effects, separate, draws = 999L) {
  formulas <- contrast_models(effects, separate)
  full <- quiet_felm(formulas$full, contrast_data)
  terms <- c("rank_gap", "rank_gap:high_to_low")
  if (is.null(full) || full$N <= full$p) return(tibble(estimable = FALSE))
  b <- coef(full)[terms]
  V <- vcov(full)[terms, terms, drop = FALSE]
  if (any(!is.finite(b)) || any(!is.finite(V)) || V[2, 2] <= 0) return(tibble(estimable = FALSE))

  difference <- unname(b[2])
  se <- sqrt(V[2, 2])
  t_obs <- difference / se
  # Use the 45 classrooms in the experiment as the reference for cluster
  # inference, as in the SI. Bootstrap weights use the 44 observed clusters.
  df <- n_distinct(dyads$class_id) - 1L
  classes <- unique(contrast_data$class_id)
  restricted <- quiet_felm(formulas$restricted, contrast_data)
  residual <- drop(residuals(restricted))
  fitted <- contrast_data$coop_gap_filtered - residual

  set.seed(123)
  t_star <- map_dbl(seq_len(draws), function(draw) {
    weights <- setNames(sample(c(-1, 1), length(classes), replace = TRUE), classes)
    sample_data <- contrast_data %>%
      mutate(coop_gap_filtered = fitted + residual * weights[as.character(class_id)])
    fit <- quiet_felm(formulas$full, sample_data)
    if (is.null(fit)) return(NA_real_)
    unname(coef(fit)[terms[2]]) / sqrt(vcov(fit)[terms[2], terms[2]])
  })
  tibble(estimable = TRUE,
         lower_to_higher = unname(b[1]), lower_to_higher_se = sqrt(V[1, 1]),
         higher_to_lower = sum(b), higher_to_lower_se = sqrt(sum(V)),
         contrast = difference, contrast_se = se,
         ci_low = difference - qt(.975, df) * se, ci_high = difference + qt(.975, df) * se,
         cluster_p = 2 * pt(-abs(t_obs), df),
         wild_p = if (all(is.finite(t_star))) mean(abs(t_star) >= abs(t_obs)) else NA_real_)
}

table_s30 <- imap_dfr(contrast_fe, function(effects, label) {
  # With no fixed effects, direction-specific intercepts and the standard
  # interaction are the same model. Estimate it only once.
  if (length(effects) == 0L) {
    return(filtered_contrast_row(effects, FALSE) %>%
             mutate(fixed_effects = label, pooled_model = "Direction-specific intercepts / standard interaction",
                    .before = 1))
  }
  bind_rows(
    filtered_contrast_row(effects, TRUE) %>% mutate(pooled_model = "Direction-specific FE", .before = 1),
    filtered_contrast_row(effects, FALSE) %>% mutate(pooled_model = "Common FE interaction", .before = 1)
  ) %>% mutate(fixed_effects = label, .before = 1)
})


## ---- s8-figure-s25-data ----
# These bin summaries come from the historical central-band filter used for
# the SI figure. The regression tables above use the global retention cutoff.
s25_bins <- read_csv(here("data", "raw", "figure_s25_bins.csv"), show_col_types = FALSE)
s25_filtered_mean <- 4.946900760089924
s25_rows <- tribble(
  ~roman, ~label,                       ~group,            ~color,            ~y_min,
  "I",    "Non friends",                "Non-friends",     color_non_friends, -6,
  "II",   "Mutual friends",             "Mutual",          color_mutual,      -6,
  "III",  "Sender declared friendship", "Sender declares", color_asymmetric, -8.5
)

## ---- s8-figure-s25-panel-style ----
s25_panel_theme <- theme_paper(base_size = 10) +
  theme(panel.border = element_rect(color = "grey70", linewidth = 0.45),
        axis.line = element_line(color = color_text, linewidth = 0.4),
        plot.title = element_text(face = "bold", size = 9),
        plot.margin = margin(2, 4, 2, 3))
s25_rank_labels <- tibble(
  gap = c(-0.62, 0.62),
  label = c("Receiver higher rank", "Sender higher rank")
)

## ---- s8-figure-s25-tokens ----
s25_token_panel <- function(filtered, color, mutual) {
  first_bin <- slice_min(filtered, gap, n = 1, with_ties = FALSE)
  ggplot(filtered, aes(gap)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = color_text) +
    geom_hline(yintercept = s25_filtered_mean, linetype = "dashed", color = color_text) +
    geom_ribbon(aes(ymin = mean_received, ymax = mean_sent), fill = "#F0E5A8", alpha = 0.55) +
    geom_line(aes(y = mean_sent), color = color, linewidth = 1.15) +
    geom_line(aes(y = mean_received), color = color, linewidth = 1.15) +
    geom_point(aes(y = mean_sent), color = color, shape = 18, size = 3.3) +
    geom_point(aes(y = mean_received), color = color, shape = 15, size = 2.9) +
    geom_errorbar(aes(ymin = mean_sent - ci_sent, ymax = mean_sent + ci_sent),
                  color = color, width = 0.045, linewidth = 0.5) +
    geom_errorbar(aes(ymin = mean_received - ci_received, ymax = mean_received + ci_received),
                  color = color, width = 0.045, linewidth = 0.5) +
    annotate("text", x = first_bin$gap,
             y = pmin(first_bin$mean_sent + first_bin$ci_sent, 10.35) - 0.1 * mutual,
             label = "Sent", color = color, fontface = "bold", size = 2.45, vjust = -0.9) +
    annotate("text", x = first_bin$gap, y = pmax(first_bin$mean_received - first_bin$ci_received, 1.35),
             label = "Received", color = color, fontface = "bold", size = 2.45, vjust = 1.25) +
    geom_text(data = s25_rank_labels, aes(y = 0.15, label = label), fontface = "bold", size = 2.2) +
    scale_x_continuous(breaks = seq(-1, 1, 0.5)) +
    scale_y_continuous(breaks = c(0, 5, 10)) +
    coord_cartesian(xlim = c(-1.1, 1.1), ylim = c(-0.35, 11), expand = FALSE) +
    labs(title = "A", x = "Social Ranking GAP", y = "Tokens") +
    s25_panel_theme
}

## ---- s8-figure-s25-gap ----
s25_gap_panel <- function(filtered, observed, color, y_min) {
  observed_color <- scales::alpha(color, 0.4)
  ggplot(filtered, aes(gap, mean_coop_gap)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = color_text) +
    geom_hline(yintercept = 0, linetype = "dashed", color = color_text) +
    geom_line(data = observed, color = observed_color, linewidth = 0.8) +
    geom_errorbar(data = observed, aes(ymin = mean_coop_gap - ci_coop_gap,
                                       ymax = mean_coop_gap + ci_coop_gap),
                  color = observed_color, width = 0.045, linewidth = 0.35) +
    geom_point(data = observed, color = observed_color, size = 3.05) +
    geom_line(color = color, linewidth = 1.2) +
    geom_point(color = color, size = 3.5) +
    geom_errorbar(aes(ymin = mean_coop_gap - ci_coop_gap, ymax = mean_coop_gap + ci_coop_gap),
                  color = color, width = 0.045, linewidth = 0.5) +
    geom_text(data = s25_rank_labels, aes(gap, y_min + 0.15, label = label),
              inherit.aes = FALSE, fontface = "bold", size = 2.2) +
    annotate("text", x = c(0.48, 0.57), y = y_min + 1.45, label = c("Filtered", "Observed"),
             color = c(color, observed_color), hjust = c(1, 0), fontface = "bold", size = 2.45) +
    scale_x_continuous(breaks = seq(-1, 1, 0.5)) +
    scale_y_continuous(breaks = seq(-7.5, 5, 2.5)) +
    coord_cartesian(xlim = c(-1.1, 1.1), ylim = c(y_min - 0.35, 6.65), expand = FALSE) +
    labs(title = "B", x = "Social Ranking GAP", y = "Cooperation GAP\n[Received - Sent]") +
    s25_panel_theme
}

## ---- s8-figure-s25-panels ----
s25_panels <- pmap(select(s25_rows, group, color, y_min), function(group, color, y_min) {
  filtered <- s25_bins %>% filter(.data$group == .env$group, version == "Filtered")
  observed <- s25_bins %>% filter(.data$group == .env$group, version == "Observed")
  list(A = s25_token_panel(filtered, color, group == "Mutual"),
       B = s25_gap_panel(filtered, observed, color, y_min))
}) %>% set_names(s25_rows$roman)

## ---- s8-figure-s25-row-assembly ----
s25_headers <- pmap(select(s25_rows, roman, label, color), friendship_row_header) %>%
  set_names(s25_rows$roman)

s25_assembled_rows <- map2(s25_headers, s25_panels, function(heading, panels) {
  heading / (panels$A | panels$B) + plot_layout(heights = c(0.095, 1))
})

## ---- s8-figure-s25 ----
figure_s25 <- wrap_plots(s25_assembled_rows, ncol = 1)
figure_s25

## ---- s8-threshold-function ----
# 8.2 Re-filter at other retention rates. "global": one cutoff for all
# transfers (the primary choice); "sender": each child keeps her own top
# share of transfers (ties and children with no variation are kept in full).
filter_at <- function(retention, scope = "global") {
  if (scope == "global") {
    cutoff <- sort(dyads$departure, decreasing = TRUE)[ceiling(retention * nrow(dyads))]
    kept <- dyads %>% mutate(keep = departure >= cutoff)
  } else {
    kept <- dyads %>%
      group_by(sender_id) %>%
      mutate(keep = departure >= quantile(departure, 1 - retention)) %>%
      ungroup()
  }
  reverse_keep <- kept %>% select(class_id, sender_id = receiver_id, receiver_id = sender_id, keep_reverse = keep)
  kept %>%
    left_join(reverse_keep, by = c("class_id", "sender_id", "receiver_id")) %>%
    mutate(coop_gap_p = if_else(keep & keep_reverse, coop_gap, NA_real_))
}

threshold_row <- function(retention, scope) {
  d <- filter_at(retention, scope)
  pairs <- d %>% filter(!is.na(coop_gap_p))
  all_fit    <- preferred_model(pairs, "coop_gap_p")
  non_fit    <- preferred_model(filter(pairs, friendship == "Non-friends"), "coop_gap_p")
  mutual_fit <- preferred_model(filter(pairs, friendship == "Mutual"), "coop_gap_p",
                                fixed_effects = "sender_id + class_id + round")
  pooled_fit <- preferred_model(filter(pairs, friendship %in% c("Non-friends", "Mutual")),
                                "coop_gap_p", rhs = "rank_gap * mutual")
  mutual_slope <- if (is.null(pooled_fit) || pooled_fit$df < 10) c(estimate = NA, se = NA, p = NA) else {
    b <- coef(pooled_fit); V <- vcov(pooled_fit); w <- c(1, 0, 1)
    est <- sum(w * b); se <- sqrt(drop(t(w) %*% V %*% w))
    c(estimate = est, se = se, p = 2 * pt(-abs(est / se), pooled_fit$df))
  }
  tibble(scope = scope, target = retention,
         transfers_retained = mean(d$keep), interactions = sum(d$keep),
         dyads_retained = mean(!is.na(d$coop_gap_p)), dyad_n = nrow(pairs),
         pooled_n = sum(pairs$friendship %in% c("Non-friends", "Mutual")),
         mutual_n = sum(pairs$friendship == "Mutual"),
         all = term_estimate(all_fit), non_friends = term_estimate(non_fit),
         mutual = term_estimate(mutual_fit),
         pooled_b1 = term_estimate(pooled_fit, "rank_gap"),
         pooled_b3 = term_estimate(pooled_fit, "rank_gap:mutual"),
         pooled_mutual_slope = mutual_slope[["estimate"]],
         pooled_mutual_slope_se = mutual_slope[["se"]],
         pooled_mutual_slope_p = mutual_slope[["p"]])
}


## ---- s8-thresholds ----
retention_rates <- c(1, 0.7, 0.6, 0.5, 0.4, 0.3, 0.25, 0.2, 0.15, 0.1, 0.05)

threshold_sensitivity <- bind_rows(
  map_dfr(retention_rates, threshold_row, scope = "global"),
  map_dfr(retention_rates, threshold_row, scope = "sender")
) %>%
  unnest(c(all, non_friends, mutual, pooled_b1, pooled_b3), names_sep = "_")

threshold_sensitivity %>%
  select(scope, target, transfers_retained, interactions, dyad_n, mutual_n,
         all_estimate, non_friends_estimate, mutual_estimate, pooled_b3_estimate) %>%
  mutate(across(where(is.double), ~ round(.x, 2)))


## ---- s8-asymmetric-thresholds ----
# 8.2b The one-sided friendships (Sender declares) split by direction, along
# the same global retention grid and all five fixed-effects specifications.
# The sender count shows how much within-sender support remains.
asymmetric_specs <- c("No FE" = "0", "Classroom FE" = "class_id",
                      "Sender + classroom FE" = "sender_id + class_id",
                      "Sender + classroom + round FE" = "sender_id + class_id + round",
                      "Sender + classroom-by-round FE" = "sender_id + class_round")

asymmetric_row <- function(retention) {
  kept <- filter_at(retention, "global") %>%
    filter(friendship == "Sender declares", !is.na(coop_gap_p)) %>%
    mutate(direction = if_else(rank_gap < 0, "Low to high", "High to low"))
  kept %>%
    group_by(direction) %>%
    group_modify(function(d, key) {
      sender_counts <- count(d, sender_id)
      imap_dfr(asymmetric_specs, function(fe, spec) {
        # as in Table S29, column (1) uses conventional standard errors and
        # the fixed-effects columns cluster by classroom
        m <- if (fe == "0") quiet_felm(coop_gap_p ~ rank_gap | 0 | 0 | 0, d) else
          preferred_model(d, outcome = "coop_gap_p", fixed_effects = fe)
        est <- term_estimate(m)
        tibble(spec = spec, n = nrow(d), senders = nrow(sender_counts),
               senders_2plus = sum(sender_counts$n >= 2), est)
      })
    }) %>%
    ungroup() %>%
    mutate(retention = retention, .before = 1)
}

asymmetric_thresholds <- map_dfr(retention_rates, asymmetric_row) %>%
  mutate(spec = factor(spec, levels = names(asymmetric_specs)),
         direction = factor(direction, levels = c("Low to high", "High to low")))

format_asymmetric_thresholds <- function(nomination_direction) {
  asymmetric_thresholds %>%
    filter(direction == nomination_direction) %>%
    mutate(cell = if_else(is.na(estimate), "n.e.",
                          sprintf("%.2f%s (%.2f)", estimate, si_stars(p_value), se))) %>%
    select(retention, n, senders_2plus, spec, cell) %>%
    pivot_wider(names_from = spec, values_from = cell) %>%
    arrange(desc(retention)) %>%
    mutate(retention = if_else(retention == 1, "100% (observed)", scales::percent(retention))) %>%
    rename("Target" = retention, "Decisions" = n, "Senders with 2+ decisions" = senders_2plus)
}

## ---- s8-figure-asymmetric-panels ----
# Asymmetric friendships by direction across the retention grid: (A) Social
# Rank GAP coefficient with 95% CI, three specifications; (B) data left.
# The figure shows the range where the samples still support estimation.
# Low-to-high in orange; retention shares on a reversed axis (100% =
# observed data). Each panel is built on its own and the two are stacked.
figure_range <- asymmetric_thresholds %>% filter(retention >= 0.4)
dodge <- position_dodge(width = 4)
direction_colors <- c("Low to high" = color_highlight, "High to low" = color_slate)
retention_labels <- function(x) if_else(x == 100, "100%\n(observed)", paste0(x, "%"))

coef_panel <- figure_range %>%
  filter(!is.na(estimate), spec %in% levels(spec)[1:3]) %>%
  ggplot(aes(100 * retention, estimate, color = direction)) +
  annotate("rect", xmin = 45, xmax = 55, ymin = -Inf, ymax = Inf, fill = "grey92") +   # the 50% filter
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_line(position = dodge, linewidth = 0.4, alpha = 0.6) +
  geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                width = 0, position = dodge, linewidth = 0.5) +
  geom_point(position = dodge, size = 1.8) +
  facet_wrap(~ spec, nrow = 1) +
  scale_x_reverse(breaks = c(100, 70, 60, 50, 40), labels = retention_labels) +
  scale_color_manual(values = direction_colors, name = NULL) +
  labs(title = "A. Social Rank GAP coefficient (95% CI)", x = NULL, y = "Coefficient") +
  theme_si() +
  theme(legend.position = "top", legend.justification = "left")

support_panel <- figure_range %>%
  distinct(retention, direction, n, senders_2plus) %>%
  pivot_longer(c(n, senders_2plus), names_to = "measure", values_to = "count") %>%
  mutate(measure = recode(measure, n = "Decisions retained",
                          senders_2plus = "Senders with 2+ retained decisions")) %>%
  ggplot(aes(100 * retention, count, color = direction)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.6) +
  facet_wrap(~ measure, nrow = 1, scales = "free_y") +
  scale_x_reverse(breaks = c(100, 70, 60, 50, 40), labels = retention_labels) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = direction_colors, guide = "none") +
  labs(title = "B. Data left for estimation", x = "Share of transfers retained by the filter", y = NULL) +
  theme_si()

## ---- s8-figure-asymmetric-thresholds ----
figure_asymmetric_thresholds <- coef_panel / support_panel + plot_layout(heights = c(1.2, 1))
figure_asymmetric_thresholds

## ---- s8-nomination-cap ----
# 8.3 Rebuild Social Rank with modified popularity nominations and re-run
# the main model: (i) keep at most k nominations per child and item, chosen
# at random (25 draws, median coefficient); (ii) drop the nominations of
# children who used all five on an item.
rank_gap_from <- function(positive, negative) {
  edges <- dyads %>%
    transmute(class_id, from = as.character(sender_id), to = as.character(receiver_id),
              weight = positive - negative + 1)
  ranks <- edges %>%
    group_by(class_id) %>%
    group_modify(function(e, key) {
      pr <- page_rank(graph_from_data_frame(e %>% filter(weight > 0),
                                            vertices = data.frame(name = unique(c(e$from, e$to)))))$vector
      tibble(student_id = as.numeric(names(pr)), rank = (pr - min(pr)) / (max(pr) - min(pr)))
    }) %>% ungroup()
  rank_of <- setNames(ranks$rank, ranks$student_id)
  rank_of[as.character(dyads$sender_id)] - rank_of[as.character(dyads$receiver_id)]
}

cap_at <- function(nominations, k) {
  # for each sender, keep a random subset of at most k of her nominations
  dyads %>%
    mutate(nom = nominations, row = row_number()) %>%
    group_by(sender_id) %>%
    mutate(keep = nom == 1 & rank(runif(n()) - nom) <= k) %>%   # nominated rows first, in random order
    ungroup() %>%
    pull(keep) %>% as.integer()
}

main_with_gap <- function(gap) term_estimate(preferred_model(dyads %>% mutate(rank_gap = gap)))

set.seed(2026)
cap_draws <- map_dfr(c(3, 4), function(k) {
  map_dfr(1:25, function(draw) {
    main_with_gap(rank_gap_from(cap_at(dyads$nom_q4, k), cap_at(dyads$nom_q5, k))) %>%
      mutate(k = k, draw = draw)
  })
})

out_counts <- dyads %>% group_by(sender_id) %>% summarise(q4 = sum(nom_q4), q5 = sum(nom_q5))
reached_q4 <- out_counts$sender_id[out_counts$q4 >= 5]
reached_q5 <- out_counts$sender_id[out_counts$q5 >= 5]

table_s36 <- bind_rows(
  main_with_gap(dyads$rank_gap) %>% mutate(construction = "Baseline (all nominations)"),
  cap_draws %>% filter(k == 3) %>% summarise(across(c(estimate, se, p_value), median)) %>%
    mutate(construction = "Fixed cap k = 3 (median of 25 draws)"),
  cap_draws %>% filter(k == 4) %>% summarise(across(c(estimate, se, p_value), median)) %>%
    mutate(construction = "Fixed cap k = 4 (median of 25 draws)"),
  main_with_gap(rank_gap_from(if_else(dyads$sender_id %in% reached_q4, 0, dyads$nom_q4),
                              if_else(dyads$sender_id %in% reached_q5, 0, dyads$nom_q5))) %>%
    mutate(construction = "Exclude cap-reachers (>= 5)")
) %>% relocate(construction)

table_s36


## ---- s8-figure-s26 ----
# Figure S26: Social Rank GAP coefficient under each nomination-cap
# construction
figure_s26 <- table_s36 %>%
  mutate(label = c("Baseline\n(all nominations)", "Fixed cap (k=3)", "Fixed cap (k=4)",
                   "Exclude cap-reachers (\u22655)"),
         label = fct_rev(fct_inorder(label)),
         baseline = row_number() == 1) %>%
  ggplot(aes(estimate, label, color = baseline)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se), height = 0.15) +
  geom_point(aes(size = baseline)) +
  # The baseline in orange and larger, bold baseline label
  scale_color_manual(values = c(`FALSE` = "grey25", `TRUE` = color_highlight)) +
  scale_size_manual(values = c(`FALSE` = 2, `TRUE` = 3)) +
  scale_x_continuous(limits = c(-0.2, 3.5), breaks = seq(0, 3.5, 0.5)) +
  labs(title = "Nomination-cap sensitivity", x = "Social Rank GAP coefficient (received - sent)", y = NULL) +
  theme_si(10) +
  theme(plot.title = element_text(hjust = 0.5), plot.title.position = "panel",
        axis.text.y = element_text(face = c("plain", "plain", "plain", "bold"), size = 10),
        panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
        panel.grid.major.y = element_blank())
figure_s26

## ---- s8-class-size ----
# 8.4 Does the slope differ by classroom size? Size terciles and a continuous
# interaction (the main effect of size is absorbed by the fixed effects).
dyads <- dyads %>%
  group_by(class_id) %>% mutate(class_size = n_distinct(sender_id)) %>% ungroup() %>%
  mutate(size_group = cut(class_size, c(0, 15, 21, 39), labels = c("Small (<=15)", "Medium (16-21)", "Large (>21)")))

dyads %>% distinct(class_id, size_group) %>% count(size_group)


## ---- s8-class-size-slopes ----
size_slopes <- dyads %>%
  group_by(size_group) %>%
  group_modify(~ term_estimate(preferred_model(.x))) %>% ungroup()
size_slopes


## ---- s8-class-size-joint-test ----
size_interaction <- preferred_model(dyads, rhs = "rank_gap * size_group")
lfe::waldtest(size_interaction, grep("rank_gap:size_group", names(coef(size_interaction))))[["p.F"]]


## ---- s8-class-size-continuous ----
term_estimate(preferred_model(dyads, rhs = "rank_gap * class_size"), "rank_gap:class_size")


## ---- s8-figure-s27-panels ----
# Figure S27: observed Cooperation GAP by sender and receiver rank quartile,
# in small, medium and large classrooms. The three heatmaps share one
# drawing, written as a function.
quartile_cuts <- quantile(c(dyads$sender_rank, dyads$receiver_rank), c(0.25, 0.5, 0.75))
size_titles <- size_slopes %>%
  left_join(dyads %>% distinct(class_id, size_group) %>% count(size_group, name = "classes"), by = "size_group") %>%
  mutate(title = c("A. Small (n \u2264 15)", "B. Medium (16\u201321)", "C. Large (n > 21)"))

cells <- dyads %>%
  mutate(sender_q   = findInterval(sender_rank, quartile_cuts, left.open = TRUE) + 1,
         receiver_q = findInterval(receiver_rank, quartile_cuts, left.open = TRUE) + 1) %>%
  group_by(size_group, sender_q, receiver_q) %>%
  summarise(observed = mean(coop_gap), .groups = "drop")

# one heatmap per size group, with the slope and number of classes below the title
size_panel <- function(i) {
  info <- size_titles[i, ]
  cells %>% filter(size_group == info$size_group) %>%
    ggplot(aes(factor(receiver_q, labels = quartile_labels), factor(sender_q, labels = quartile_labels),
               fill = observed)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%+.2f", observed), color = abs(observed) > 1.1),
              fontface = "bold", size = 3.4) +
    scale_fill_coop_gap() +
    scale_color_manual(values = c(`FALSE` = "grey15", `TRUE` = "white"), guide = "none") +
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
    coord_fixed() +
    labs(title = info$title,
         subtitle = bquote(hat(beta) == .(sprintf("%.2f", info$estimate)) %.% .(info$classes) ~ "classes"),
         x = "Receiver's rank quartile", y = if (i == 1) "Sender's rank quartile" else NULL) +
    theme_heatmap(9) +
    guides(fill = guide_colorbar(title.position = "right",
                                 title.theme = element_text(angle = 90, size = 8)))
}

## ---- s8-figure-s27 ----
figure_s27 <- wrap_plots(map(seq_len(nrow(size_titles)), size_panel), nrow = 1, guides = "collect") +
  figure_title("Observed Cooperation GAP by rank quartile, across classroom sizes", size = 12)
figure_s27

## ---- s8-temporal ----
# 8.5 Does the slope change over the session?
#   progress_c          the sender's progress, centered (-0.5 to 0.5)
#   decile differences  receiver-minus-sender indicators of progress deciles
dyads <- dyads %>%
  group_by(sender_id) %>% arrange(round, .by_group = TRUE) %>%
  mutate(order = row_number(), progress = (order - 1) / (n() - 1),
         progress_c = progress - 0.5,
         decile = pmin(pmax(ceiling(progress * 10), 1), 10)) %>%
  ungroup()

# decile of the receiver in this same encounter
receiver_decile <- dyads %>% select(class_id, sender_id = receiver_id, receiver_id = sender_id,
                                    receiver_decile = decile)
dyads <- dyads %>% left_join(receiver_decile, by = c("class_id", "sender_id", "receiver_id"))
for (k in 2:10) {
  dyads[[paste0("decile_diff_", k)]] <- (dyads$receiver_decile == k) - (dyads$decile == k)
}
decile_controls <- paste(paste0("decile_diff_", 2:10), collapse = " + ")
non_mutual <- dyads %>% filter(friendship %in% c("Non-friends", "Mutual"))

temporal_models <- list(
  "(1) main"                    = preferred_model(dyads),
  "(2) sender linear trends"    = preferred_model(dyads, fixed_effects = "sender_id + class_round + sender_id:progress"),
  "(3) progress deciles"        = preferred_model(dyads, rhs = paste("rank_gap +", decile_controls)),
  "(4) gap x progress"          = preferred_model(dyads, rhs = "rank_gap * progress_c"),
  "(5) non-friends, x progress" = preferred_model(filter(dyads, friendship == "Non-friends"), rhs = "rank_gap * progress_c"),
  "(6) pooled"                  = preferred_model(non_mutual, rhs = "rank_gap * mutual"),
  "(7) pooled + deciles"        = preferred_model(non_mutual, rhs = paste0("rank_gap * mutual + (", decile_controls, ") * mutual")),
  "(8) pooled x progress"       = preferred_model(non_mutual, rhs = "rank_gap * mutual * progress_c")
)

table_s37 <- imap_dfr(temporal_models, function(fit, name) {
  summary(fit)$coefficients %>% as_tibble(rownames = "term") %>%
    filter(str_detect(term, "rank_gap")) %>%
    transmute(model = name, term, estimate = Estimate, se = `Cluster s.e.`, p_value = `Pr(>|t|)`, n = fit$N)
})
table_s37 %>% mutate(across(where(is.double), ~ round(.x, 3)))

# Paired classroom jackknife for the change in the rank-gap coefficient
# between model (1) and model (3)
jackknife <- map_dbl(unique(dyads$class_id), function(c_out) {
  d <- filter(dyads, class_id != c_out)
  coef(preferred_model(d, rhs = paste("rank_gap +", decile_controls)))[["rank_gap"]] -
    coef(preferred_model(d))[["rank_gap"]]
})
difference <- coef(temporal_models[[3]])[["rank_gap"]] - coef(temporal_models[[1]])[["rank_gap"]]
G <- length(jackknife)
jackknife_se <- sqrt((G - 1) / G * sum((jackknife - mean(jackknife))^2))
c(difference = difference, se = jackknife_se,
  ci_low = difference - qt(0.975, G - 1) * jackknife_se,
  ci_high = difference + qt(0.975, G - 1) * jackknife_se,
  p = 2 * pt(-abs(difference / jackknife_se), G - 1))


## ---- s8-placebo ----
# 8.6 Placebo: the rank gap the child faced in her previous decision should
# not predict today's Cooperation GAP once sender and class-round FE are in.
dyads <- dyads %>%
  group_by(sender_id) %>% arrange(order, .by_group = TRUE) %>%
  mutate(lagged_rank_gap = lag(rank_gap)) %>% ungroup()

lagged <- dyads %>% filter(!is.na(lagged_rank_gap))
fe_placebo <- fe_progression(lagged, formula_rhs = "lagged_rank_gap")
regression_table(fe_placebo, terms = "lagged_rank_gap")


## ---- s8-placebo-by-group ----
placebo_samples <- list(
  "All"                    = list(lagged, "coop_gap"),
  "All (filtered)"         = list(lagged, "coop_gap_filtered"),
  "Mutual"                 = list(filter(lagged, friendship == "Mutual"), "coop_gap"),
  "Mutual (filtered)"      = list(filter(lagged, friendship == "Mutual"), "coop_gap_filtered"),
  "Non-friends"            = list(filter(lagged, friendship == "Non-friends"), "coop_gap"),
  "Non-friends (filtered)" = list(filter(lagged, friendship == "Non-friends"), "coop_gap_filtered")
)
table_s39 <- imap_dfr(placebo_samples, function(s, name) {
  d <- s[[1]] %>% filter(!is.na(.data[[s[[2]]]]))
  fit <- preferred_model(d, outcome = s[[2]], rhs = "lagged_rank_gap")
  term_estimate(fit, "lagged_rank_gap") %>% mutate(sample = name, n = nrow(d), .before = 1)
})
table_s39


## ---- s8-sex-grade ----
# The SI reports separate samples and the same model layouts for observed
# and filtered transfers. Filter before fitting or forming subgroups.
dyads <- dyads %>%
  left_join(students %>% select(sender_id = student_id, sender_female = female, grade), by = "sender_id") %>%
  left_join(students %>% select(receiver_id = student_id, receiver_female = female), by = "receiver_id")

sex_models <- function(data, outcome) {
  data <- filter(data, !is.na(.data[[outcome]]))
  class_sender <- "class_id + sender_id"
  girls <- filter(data, sender_female == 1)
  boys <- filter(data, sender_female == 0)
  list(
    "(1) All" = preferred_model(data, outcome, "rank_gap", "class_id"),
    "(2) Sender sex" = preferred_model(data, outcome, "rank_gap + sender_female", "class_id"),
    "(3) Receiver sex" = preferred_model(data, outcome, "rank_gap + receiver_female", class_sender),
    "(4) Interactions" = preferred_model(data, outcome, "rank_gap * sender_female * receiver_female", "class_id"),
    "(5) Girls" = preferred_model(girls, outcome, "rank_gap", class_sender),
    "(6) Girls + receiver sex" = preferred_model(girls, outcome, "rank_gap + receiver_female", class_sender),
    "(7) Boys" = preferred_model(boys, outcome, "rank_gap", class_sender),
    "(8) Boys + receiver sex" = preferred_model(boys, outcome, "rank_gap + receiver_female", class_sender)
  )
}

grade_models <- function(data, outcome) {
  data <- filter(data, !is.na(.data[[outcome]])) %>%
    mutate(grade4 = as.integer(grade == 4), grade5 = as.integer(grade == 5))
  pooled <- quiet_felm(as.formula(paste(outcome, "~ rank_gap + grade4 + grade5 | 0 | 0 | 0")), data)
  by_grade <- map(3:5, function(level) {
    sample <- filter(data, grade == level)
    list(preferred_model(sample, outcome, "rank_gap", "class_id"),
         preferred_model(sample, outcome, "rank_gap", "class_id + sender_id"))
  })
  c(list(pooled), flatten(by_grade)) %>%
    set_names(c("(1) Pooled", "(2) Grade 3", "(3) Grade 3 + sender FE",
                "(4) Grade 4", "(5) Grade 4 + sender FE",
                "(6) Grade 5", "(7) Grade 5 + sender FE"))
}

subgroup_tables <- expand_grid(by = c("sex", "grade"),
                               sample = c("All dyads", "Mutual friends", "Non-friends"),
                               outcome = c("coop_gap", "coop_gap_filtered")) %>%
  mutate(table = pmap(list(by, sample, outcome), function(by, sample, outcome) {
    d <- switch(sample, "All dyads" = dyads,
                "Mutual friends" = filter(dyads, friendship == "Mutual"),
                "Non-friends" = filter(dyads, friendship == "Non-friends"))
    models <- if (by == "sex") sex_models(d, outcome) else grade_models(d, outcome)
    regression_table(models)
  }))

subgroup_table <- function(grouping, friendship_sample, outcome_name) {
  subgroup_tables %>%
    filter(by == grouping, sample == friendship_sample, outcome == outcome_name) %>%
    pull(table) %>% pluck(1)
}


## ---- s8-alternative-ranks ----
# 8.8 The preferred model with the rank gap from each alternative
# construction of Social Rank (built in S4)
alternative_ranks <- read_csv(here("data", "generated", "alternative_ranks.csv"), show_col_types = FALSE)
alternative_labels <- c(social_rank          = "Pop. Social Rank\n(Affine transform)",
                        rank_pn_centrality   = "Signed PN centrality",
                        rank_net_nominations = "Net nominations",
                        rank_pr_minus_neg_in = "Pos. PageRank -\nneg. indegree",
                        rank_two_layer       = "Two-layer PageRank",
                        rank_positive_only   = "Positive-only PageRank")

gap_with <- function(measure) {
  r <- setNames(alternative_ranks[[measure]], alternative_ranks$student_id)
  r[as.character(dyads$sender_id)] - r[as.character(dyads$receiver_id)]
}

# Five samples as in the SI figure, all with the preferred specification.
# One-sided friendships are seen from the receiver ("alter declares") or from
# the sender ("ego declares").
robustness_samples <- c("A. All dyads", "B. Non-friends", "C. Mutual friends",
                        "D. Alter declares", "E. Ego declares")

rank_robustness <- expand_grid(measure = names(alternative_labels), sample = robustness_samples) %>%
  mutate(result = map2(measure, sample, function(m, s) {
    d <- dyads %>% mutate(rank_gap = gap_with(m))
    d <- switch(s,
                "A. All dyads"      = d,
                "B. Non-friends"    = filter(d, friendship == "Non-friends"),
                "C. Mutual friends" = filter(d, friendship == "Mutual"),
                "D. Alter declares" = filter(d, friendship == "Receiver declares"),
                "E. Ego declares"   = filter(d, friendship == "Sender declares"))
    term_estimate(preferred_model(d))
  })) %>%
  unnest(result) %>%
  mutate(primary = measure == "social_rank",
         measure = factor(alternative_labels[measure], levels = rev(alternative_labels)))

rank_robustness %>% select(measure, sample, estimate) %>%
  pivot_wider(names_from = sample, values_from = estimate)


## ---- s8-figure-s28 ----
# Figure S28: Social Rank GAP coefficient for each rank construction and sample
figure_s28 <- ggplot(rank_robustness, aes(estimate, measure, color = primary)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se), height = 0.2) +
  geom_point(size = 1.8) +
  facet_wrap(~ sample, nrow = 1, scales = "free_x") +
  # The primary Social Rank in orange, with a bold label
  scale_color_manual(values = c(`FALSE` = "grey35", `TRUE` = color_highlight)) +
  labs(title = "Social Rank GAP robustness across popularity-network constructions",
       x = "Coefficient", y = NULL) +
  theme_si(8) +
  theme(plot.title = element_text(hjust = 0.5), plot.title.position = "panel",
        axis.text.y = element_text(color = rev(c(color_highlight, rep(color_text, 5))),
                                   face = rev(c("bold", rep("plain", 5)))),
        panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3))
figure_s28

## ---- s8-affiliation-ranks ----
# 8.9 Rank from affiliation networks instead of popularity: PageRank of
# "most like to hang out" (Q1), Q1 minus "least like to hang out" (Q2)
# (standardized PageRanks), and PageRank of best friends (Q3)
pagerank_of <- function(nominations) {
  dyads %>%
    transmute(class_id, from = as.character(sender_id), to = as.character(receiver_id), w = nominations) %>%
    group_by(class_id) %>%
    group_modify(function(e, key) {
      pr <- page_rank(graph_from_data_frame(e %>% filter(w > 0) %>% select(from, to),
                                            vertices = data.frame(name = unique(c(e$from, e$to)))))$vector
      tibble(student_id = as.numeric(names(pr)), pr = pr)
    }) %>% ungroup()
}

z <- function(x) (x - mean(x)) / sd(x)
affiliation_ranks <- pagerank_of(dyads$nom_q1) %>% rename(pr_q1 = pr) %>%
  left_join(pagerank_of(dyads$nom_q2) %>% rename(pr_q2 = pr), by = c("class_id", "student_id")) %>%
  left_join(pagerank_of(dyads$nom_q3) %>% rename(pr_q3 = pr), by = c("class_id", "student_id")) %>%
  group_by(class_id) %>%
  mutate(rank_hangout        = (pr_q1 - min(pr_q1)) / (max(pr_q1) - min(pr_q1)),
         rank_hangout_minus  = { x <- z(pr_q1) - z(pr_q2); (x - min(x)) / (max(x) - min(x)) },
         rank_best_friend    = (pr_q3 - min(pr_q3)) / (max(pr_q3) - min(pr_q3))) %>%
  ungroup() %>%
  left_join(students %>% select(student_id, social_rank), by = "student_id")

affiliation_labels <- c(social_rank = "Pop. Social Rank (primary)",
                        rank_hangout = "Most-like-to-hang-out PageRank",
                        rank_hangout_minus = "Most-minus-least hang-out PageRank",
                        rank_best_friend = "Best-friend PageRank")

table_s52 <- expand_grid(measure = names(affiliation_labels),
                         sample = c("All dyads", "Non-friends", "Mutual friends")) %>%
  mutate(result = map2(measure, sample, function(m, s) {
    r <- setNames(affiliation_ranks[[m]], affiliation_ranks$student_id)
    d <- dyads %>% mutate(rank_gap = r[as.character(sender_id)] - r[as.character(receiver_id)])
    d <- switch(s, "All dyads" = d, "Non-friends" = filter(d, friendship == "Non-friends"),
                "Mutual friends" = filter(d, friendship == "Mutual"))
    term_estimate(preferred_model(d)) %>% mutate(sd_gap = sd(d$rank_gap))
  })) %>%
  unnest(result) %>%
  mutate(measure = affiliation_labels[measure])

table_s52 %>%
  mutate(cell = sprintf("%.3f%s (%.3f)", estimate, si_stars(p_value), se)) %>%
  select(measure, sample, cell) %>%
  pivot_wider(names_from = sample, values_from = cell)


## ---- s8-affiliation-agreement ----
# agreement with the primary rank (median within-class Spearman)
affiliation_ranks %>%
  group_by(class_id) %>%
  summarise(across(c(rank_hangout, rank_hangout_minus, rank_best_friend),
                   ~ cor(.x, social_rank, method = "spearman"))) %>%
  summarise(across(-class_id, median))


## ---- s8-save ----
save_table(regression_table(fe_filtered_all, terms = "rank_gap"),    "S8_table_s25_filtered_all")
save_table(regression_table(fe_filtered_non, terms = "rank_gap"),    "S8_table_s26_filtered_non_friends")
save_table(regression_table(fe_filtered_mutual, terms = "rank_gap"), "S8_table_s27_filtered_mutual")
save_table(regression_table(fe_filtered_pooled, terms = c("rank_gap", "mutual", "rank_gap:mutual")), "S8_table_s28_filtered_pooled")
save_table(regression_table(fe_filtered_lh, terms = "rank_gap"),     "S8_table_s29_filtered_low_to_high")
save_table(regression_table(fe_filtered_hl, terms = "rank_gap"),     "S8_table_s29_filtered_high_to_low")
save_table(table_s30, "S8_table_s30_filtered_contrast")
save_table(threshold_sensitivity, "S8_tables_s31_s34_thresholds")
save_table(asymmetric_thresholds, "S8_table_s35_asymmetric_thresholds")
save_table(table_s36, "S8_table_s36_nomination_cap")
save_table(size_slopes, "S8_class_size_slopes")
save_table(table_s37, "S8_table_s37_temporal")
save_table(regression_table(fe_placebo, terms = "lagged_rank_gap"), "S8_table_s38_placebo")
save_table(table_s39, "S8_table_s39_placebo_samples")
pwalk(subgroup_tables, function(by, sample, outcome, table) {
  save_table(table, paste("S8_subgroup", by, str_replace_all(tolower(sample), "[^a-z]+", "_"), outcome, sep = "_"))
})
save_table(rank_robustness, "S8_rank_constructions")
save_table(table_s52, "S8_table_s52_affiliation_ranks")
save_si_figure(figure_s25, "S8_figure_s25_filtered", 6.5, 7.6)
save_si_figure(figure_asymmetric_thresholds, "S8_figure_asymmetric_thresholds", 9, 6)
save_si_figure(figure_s26, "S8_figure_s26_nomination_cap", 7, 3.4)
save_si_figure(figure_s27, "S8_figure_s27_class_size", 10, 4)
save_si_figure(figure_s28, "S8_figure_s28_rank_constructions", 11, 3.4)
