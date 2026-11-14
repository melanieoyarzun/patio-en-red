# =============================================================================
# S4_social_rank.R   SI Section 4: Construction and validation of Social Rank
#
#   Table S7   Coding of the popularity nominations
#   Table S8   Alternative constructions (computed here, used again in S8)
#   Figure S6  Correlations between the constructions
#   Figure S7  Sensitivity to the weight given to non-nominations
#   Table S9   Social Rank and student characteristics; Figure S8
#   Table S10  Distribution of the Social Rank GAP
#
#   Input : data/generated/analysis_dyads.csv, data/generated/analysis_students.csv
#   Output: output/tables/si/S4_*.csv, output/figures/si/S4_*.(pdf|png)
#           data/generated/alternative_ranks.csv  (used by S8)
# =============================================================================

## ---- s4-setup ----
source(here::here("code", "00_helpers.R"))
source(here("code", "00_style.R"))
library(igraph)
library(patchwork)
dir.create(here("output", "figures", "si"), recursive = TRUE, showWarnings = FALSE)

dyads    <- read_csv(here("data", "generated", "analysis_dyads.csv"),    show_col_types = FALSE)
students <- read_csv(here("data", "generated", "analysis_students.csv"), show_col_types = FALSE)

rescale_01 <- function(x) (x - min(x)) / (max(x) - min(x))
z_score    <- function(x) if (sd(x) > 0) (x - mean(x)) / sd(x) else x * 0


## ---- s4-coding ----
# Table S7: how each combination of the two popularity questions is coded
table_s7 <- dyads %>%
  count(positive = nom_q4, negative = nom_q5) %>%
  mutate(signed_score = positive - negative,
         weight       = signed_score + 1)

table_s7

# 121 pairs named on both items = 242 of the 4,178 popularity nominations
c(conflicts = sum(dyads$nom_q4 & dyads$nom_q5),
  popularity_nominations = sum(dyads$nom_q4) + sum(dyads$nom_q5))


## ---- s4-incoming-weight ----
# The +1 shift adds the same constant (N - 1) to every child's incoming
# weight, so ranking by incoming weight = ranking by net nominations q_j.
check_shift <- dyads %>%
  group_by(class_id, receiver_id) %>%
  summarise(incoming_weight = sum(nom_q4 - nom_q5 + 1),
            net_nominations = sum(nom_q4) - sum(nom_q5),
            n_minus_1       = n(), .groups = "drop")

all(check_shift$incoming_weight == check_shift$n_minus_1 + check_shift$net_nominations)


## ---- s4-pagerank-function ----
# PageRank of every child in one classroom, for any choice of edge weights.
# Edges with weight 0 are dropped (igraph does not accept them as "no edge"
# otherwise); children without incoming edges keep the teleport share.
pagerank_weights <- function(edges) {
  children <- unique(c(edges$from, edges$to))
  network <- graph_from_data_frame(edges %>% filter(weight > 0),
                                   vertices = data.frame(name = children))
  pr <- page_rank(network, damping = 0.85, directed = TRUE)$vector
  tibble(student_id = as.numeric(names(pr)), value = pr)
}

pagerank_by_class <- function(weight_formula) {
  dyads %>%
    transmute(class_id, from = as.character(sender_id), to = as.character(receiver_id),
              weight = {{ weight_formula }}) %>%
    group_by(class_id) %>%
    group_modify(~ pagerank_weights(.x)) %>%
    ungroup()
}


## ---- s4-alternative-ranks ----
# Alternatives (Table S8). Every score is oriented so that higher = higher
# standing and rescaled 0-1 within classroom.

in_degrees <- dyads %>%
  group_by(class_id, student_id = receiver_id) %>%
  summarise(positive_in = sum(nom_q4), negative_in = sum(nom_q5), .groups = "drop")

pr_positive <- pagerank_by_class(nom_q4) %>% rename(pr_positive = value)
pr_negative <- pagerank_by_class(nom_q5) %>% rename(pr_negative = value)

# Signed in-PN centrality (Everett and Borgatti 2014), on the signed network
# A = P+ - 2 P-, computed for incoming ties: PN = (I - A' / (2n - 2))^-1 1
pn_centrality <- function(class_dyads) {
  ids <- sort(unique(class_dyads$sender_id))
  n <- length(ids)
  A <- matrix(0, n, n, dimnames = list(ids, ids))
  A[cbind(as.character(class_dyads$sender_id), as.character(class_dyads$receiver_id))] <-
    class_dyads$nom_q4 - 2 * class_dyads$nom_q5
  pn <- solve(diag(n) - t(A) / (2 * n - 2), rep(1, n))
  tibble(student_id = ids, pn_in = as.numeric(pn))
}

pn_scores <- dyads %>% group_by(class_id) %>% group_modify(~ pn_centrality(.x)) %>% ungroup()

alternative_ranks <- students %>%
  select(student_id, class_id, social_rank) %>%
  left_join(in_degrees,  by = c("class_id", "student_id")) %>%
  left_join(pr_positive, by = c("class_id", "student_id")) %>%
  left_join(pr_negative, by = c("class_id", "student_id")) %>%
  left_join(pn_scores,   by = c("class_id", "student_id")) %>%
  group_by(class_id) %>%
  mutate(rank_positive_only    = rescale_01(pr_positive),
         rank_two_layer        = rescale_01(z_score(pr_positive) - z_score(pr_negative)),
         rank_net_nominations  = rescale_01(z_score(positive_in) - z_score(negative_in)),
         rank_pr_minus_neg_in  = rescale_01(z_score(pr_positive) - z_score(negative_in)),
         rank_pn_centrality    = rescale_01(pn_in)) %>%
  ungroup()

alternative_labels <- c(
  rank_net_nominations = "Net popularity nominations",
  rank_pn_centrality   = "Signed in-PN centrality",
  rank_pr_minus_neg_in = "Positive PageRank minus negative indegree",
  rank_two_layer       = "Two-layer PageRank",
  rank_positive_only   = "Positive-only PageRank"
)


## ---- s4-construction-correlations ----
# Agreement with the primary Social Rank: median within-classroom Spearman
# (reported in the text) and pooled Spearman (Figure S6)
ranks_long <- alternative_ranks %>%
  pivot_longer(all_of(names(alternative_labels)), names_to = "measure", values_to = "rank")

within_class <- ranks_long %>%
  group_by(measure, class_id) %>%
  summarise(spearman = cor(social_rank, rank, method = "spearman"), .groups = "drop") %>%
  group_by(measure) %>%
  summarise(median_within_class_spearman = median(spearman))

construction_agreement <- ranks_long %>%
  group_by(measure) %>%
  summarise(pooled_spearman = cor(social_rank, rank, method = "spearman")) %>%
  left_join(within_class, by = "measure") %>%
  mutate(measure = alternative_labels[measure]) %>%
  arrange(desc(median_within_class_spearman))

construction_agreement


## ---- s4-figure-s6-data ----
# Figure S6: pooled Spearman correlations between every pair of
# constructions (upper triangle)
matrix_labels <- c(social_rank          = "Pop. Social Rank\n(Affine transform)",
                   rank_pn_centrality   = "Signed PN\ncentrality",
                   rank_net_nominations = "Net\nnominations",
                   rank_pr_minus_neg_in = "Pos. PageRank\nminus neg.\nindegree",
                   rank_two_layer       = "Two-layer\nPageRank",
                   rank_positive_only   = "Positive-only\nPageRank")

correlations <- cor(alternative_ranks[, names(matrix_labels)], method = "spearman")
correlation_cells <- as_tibble(correlations, rownames = "row") %>%
  pivot_longer(-row, names_to = "column", values_to = "rho") %>%
  mutate(row_i = match(row, names(matrix_labels)),
         col_i = match(column, names(matrix_labels))) %>%
  filter(col_i >= row_i) %>%
  mutate(row = factor(matrix_labels[row], levels = rev(matrix_labels)),
         column = factor(matrix_labels[column], levels = matrix_labels))

## ---- s4-figure-s6 ----
# Cells in a blue scale from 0 to 1; the primary Social Rank row framed in orange
figure_s6 <- ggplot(correlation_cells, aes(column, row, fill = rho)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", rho), fontface = if_else(rho == 1, "bold", "plain")),
            color = "white", size = 3.6) +
  annotate("rect", xmin = 0.5, xmax = 6.5, ymin = 5.5, ymax = 6.5,
           fill = NA, color = color_highlight, linewidth = 1.2) +
  scale_fill_gradientn(colors = c("#F5F6F7", "#A7C0CF", "#2E6A8E"), limits = c(0, 1),
                       name = "Spearman correlation") +
  coord_fixed(expand = FALSE) +
  labs(title = "Popularity-rank alternatives", x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(), plot.title = element_text(face = "bold"),
        axis.line = element_line(color = "grey75", linewidth = 0.3),
        legend.key.height = unit(1.9, "cm"), legend.key.width = unit(0.35, "cm")) +
  guides(fill = guide_colorbar(title.position = "right", frame.colour = "grey75",
                               title.theme = element_text(angle = 90, size = 8)))
figure_s6

## ---- s4-reference-weight ----
# Figure S7: keep explicit negative = 0 and explicit positive = 2, and give
# non-nominations (and offsetting pairs) a weight w from 0 to 2.
# w = 1 is the primary Social Rank.
weights_to_try <- seq(0, 2, by = 0.25)

rank_with_weight <- map_dfr(weights_to_try, function(w) {
  pagerank_by_class(case_when(nom_q4 - nom_q5 ==  1 ~ 2,
                              nom_q4 - nom_q5 == -1 ~ 0,
                              TRUE                  ~ w)) %>%
    group_by(class_id) %>%
    mutate(rank = rescale_01(value), w = w) %>%
    ungroup()
})

reference_weight_agreement <- rank_with_weight %>%
  left_join(students %>% select(student_id, social_rank), by = "student_id") %>%
  group_by(w, class_id) %>%
  summarise(spearman = cor(rank, social_rank, method = "spearman"), .groups = "drop")

reference_weight_agreement %>%
  group_by(w) %>%
  summarise(median = median(spearman), min = min(spearman))

example_class <- 21   # the 18-child classroom shown in the SI

# a finer grid of weights for the drawing
fine_weights <- seq(0, 2, by = 0.05)
fine_ranks <- map_dfr(fine_weights, function(w) {
  pagerank_by_class(case_when(nom_q4 - nom_q5 ==  1 ~ 2,
                              nom_q4 - nom_q5 == -1 ~ 0,
                              TRUE                  ~ w)) %>%
    group_by(class_id) %>%
    mutate(rank = rescale_01(value), w = w) %>%
    ungroup()
}) %>%
  left_join(students %>% select(student_id, social_rank), by = "student_id")

# share of pairs of children ordered as in the primary rank
pairs_preserved <- function(rank, primary) {
  pairs <- combn(length(rank), 2)
  same <- sign(rank[pairs[1, ]] - rank[pairs[2, ]]) == sign(primary[pairs[1, ]] - primary[pairs[2, ]])
  mean(same)
}

agreement_by_weight <- fine_ranks %>%
  group_by(w, class_id) %>%
  summarise(spearman = cor(rank, social_rank, method = "spearman"),
            pairs    = pairs_preserved(rank, social_rank), .groups = "drop")


## ---- s4-figure-s7-panels ----
# Figure S7: (A) the ranks of one classroom as the weight w of
# non-nominations changes; (B) agreement with the primary order in that
# classroom; (C) agreement in every classroom, with the median and the
# 10-90th percentile band. Each panel is built on its own and the three are
# assembled at the end.

# pieces shared by the three panels
primary_line <- geom_vline(xintercept = 1, linetype = "dashed", color = "grey20", linewidth = 0.4)
w_label <- expression("Reference weight for non-nominations (" * italic(w) * ")")
theme_s7 <- theme_si_frame(base_size = 8, l_axes = TRUE) +
  theme(plot.title = element_text(hjust = 0), plot.title.position = "panel")
x_axis <- scale_x_continuous(limits = c(0, 2), breaks = c(0.5, 1, 1.5, 2), expand = c(0, 0))
y_agreement <- scale_y_continuous(limits = c(0.5, 1.01), breaks = c(0.5, 0.75, 1),
                                  labels = c("0.5", "0.75", "1"), expand = c(0, 0))

# A. one line per child of the example classroom, darker for higher ranks
example <- fine_ranks %>% filter(class_id == example_class)

panel_a <- ggplot(example, aes(w, rank, group = student_id, color = social_rank)) +
  geom_line(linewidth = 0.6) +
  primary_line +
  annotate("text", x = 1, y = 1.04, label = "primary~(italic(w) == 1)", parse = TRUE,
           size = 2.6, color = color_text) +
  scale_color_gradient(low = "#C9D0DE", high = "#2E4A6B") +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  x_axis +
  labs(title = bquote(bold("A. One classroom: Social Rank vs.") ~ italic(w) ~
                        (italic(n) == .(n_distinct(example$student_id)))),
       x = w_label, y = "Social Rank (normalized PageRank)") +
  theme_s7

# B. two measures of agreement with the primary order, same classroom
panel_b <- agreement_by_weight %>%
  filter(class_id == example_class) %>%
  pivot_longer(c(spearman, pairs), names_to = "measure") %>%
  mutate(measure = factor(measure, levels = c("spearman", "pairs"),
                          labels = c("Spearman rank correlation", "Pairwise orderings preserved"))) %>%
  ggplot(aes(w, value, color = measure, linetype = measure)) +
  geom_line(linewidth = 0.9) +
  primary_line +
  scale_color_manual(values = c(color_highlight, "#7A86A8"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dotted"), name = NULL) +
  x_axis + y_agreement +
  labs(title = "B. One classroom: order is preserved", x = w_label,
       y = "Agreement with primary order") +
  theme_s7 +
  theme(legend.position = c(0.55, 0.12), legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.3, "cm"), legend.spacing.y = unit(0, "pt"),
        legend.text = element_text(size = 6.5))

# C. every classroom (gray), median (orange) and 10-90th percentile band
band <- agreement_by_weight %>%
  group_by(w) %>%
  summarise(median = median(spearman), low = quantile(spearman, 0.1), high = quantile(spearman, 0.9))
legend_c <- c("Median across classrooms", "10\u201390th percentile", "Individual classrooms")

panel_c <- ggplot() +
  geom_line(data = agreement_by_weight, aes(w, spearman, group = class_id, linetype = legend_c[3]),
            color = "#C9D0DE", linewidth = 0.25) +
  geom_ribbon(data = band, aes(w, ymin = low, ymax = high, fill = legend_c[2]), alpha = 0.25) +
  geom_line(data = band, aes(w, median, color = legend_c[1]), linewidth = 1) +
  primary_line +
  scale_color_manual(values = setNames(color_highlight, legend_c[1]), name = NULL) +
  scale_fill_manual(values = setNames(color_highlight, legend_c[2]), name = NULL) +
  scale_linetype_manual(values = setNames("solid", legend_c[3]), name = NULL) +
  x_axis + y_agreement +
  labs(title = bquote(bold("C. All classrooms") ~ (italic(k) == .(n_distinct(agreement_by_weight$class_id)))),
       x = w_label, y = "Spearman vs. primary order") +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2),
         linetype = guide_legend(order = 3, override.aes = list(color = "#C9D0DE", linewidth = 0.5))) +
  theme_s7 +
  theme(legend.position = c(0.55, 0.14), legend.spacing.y = unit(0, "pt"),
        legend.margin = margin(0, 2, 0, 2), legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.7, "cm"), legend.text = element_text(size = 6.5),
        legend.background = element_rect(fill = alpha("white", 0.85), color = NA))

## ---- s4-figure-s7 ----
figure_s7 <- (panel_a + panel_b + panel_c) +
  figure_title(paste("Reference-weight robustness of popularity-based Social Rank:",
                     "the within-class ordering is preserved"), size = 10)
figure_s7

## ---- s4-correlates ----
# Table S9, Panel A: correlations between Social Rank and other student
# characteristics. Nomination counts and continuous variables are turned into
# within-classroom z-scores. "Within-class" correlations remove the classroom
# mean of both variables first. SE = sqrt((1 - r^2) / (n - 2)).

# The within-classroom z-scores come from 01_build_measures.R (step
# "student-covariates"); here they only get the short names used below.
correlates <- students %>%
  transmute(student_id, class_id, social_rank, female, repeated_grade,
            nom_q1 = received_q1_z, nom_q2 = received_q2_z, nom_q3 = received_q3_z,
            nom_q4 = received_q4_z, nom_q5 = received_q5_z, nom_q6 = received_q6_z,
            nom_q7 = received_q7_z, nom_q8 = received_q8_z, nom_q9 = received_q9_z,
            net_popularity = net_popularity_z,
            aggression = aggression_z, prosociality = prosociality_z,
            attendance = attendance_z, gpa = gpa_z, age = age_z)

correlate_labels <- c(
  nom_q3 = "Best-friend nominations", nom_q1 = "Most-like-to-hang-out nominations",
  nom_q2 = "Least-like-to-hang-out nominations", net_popularity = "Net popularity nominations",
  nom_q4 = "Most-popular nominations (Q4)", nom_q5 = "Not-popular nominations (Q5)",
  nom_q6 = "Initiate-fights nominations (Q6)", nom_q7 = "Make-fun-of-others nominations (Q7)",
  nom_q8 = "Kind-to-others nominations (Q8)", nom_q9 = "Cooperative nominations (Q9)",
  aggression = "Aggression composite", prosociality = "Prosociality composite",
  attendance = "Attendance (within-class z)", gpa = "GPA (within-class z)",
  age = "Age (within-class z)", female = "Sex (female indicator)",
  repeated_grade = "Grade repetition (repeated indicator)"
)

correlation_row <- function(variable) {
  d <- correlates %>%
    select(class_id, social_rank, x = all_of(variable)) %>%
    filter(!is.na(x)) %>%
    group_by(class_id) %>%
    mutate(rank_within = social_rank - mean(social_rank), x_within = x - mean(x)) %>%
    ungroup()
  r <- c(pooled_pearson  = cor(d$social_rank, d$x),
         pooled_spearman = cor(d$social_rank, d$x, method = "spearman"),
         within_pearson  = cor(d$rank_within, d$x_within),
         within_spearman = cor(d$rank_within, d$x_within, method = "spearman"))
  tibble(measure = correlate_labels[[variable]], n = nrow(d),
         statistic = names(r), r = r, se = sqrt((1 - r^2) / (nrow(d) - 2)))
}

table_s9_a <- map_dfr(names(correlate_labels), correlation_row)

table_s9_a %>%
  select(measure, statistic, r) %>%
  pivot_wider(names_from = statistic, values_from = r) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))


## ---- s4-background ----
# Table S9, Panel B: does Social Rank differ by guardian education (4 levels)?
# p-values from OLS and from a model with classroom FE (clustered SE), and the
# within-classroom partial R2 of the education categories.
# (Religion is also in the SI table; it is not part of the public data.)
ols_model <- lm(social_rank ~ guardian_education, data = students)
fe_model  <- felm(social_rank ~ guardian_education | class_id | 0 | class_id,
                  data = students %>% mutate(class_id = factor(class_id)))
fe_only   <- felm(social_rank ~ 1 | class_id, data = students %>% mutate(class_id = factor(class_id)))

table_s9_b <- tibble(
  characteristic = "Guardian education (4 levels)",
  ols_p          = anova(lm(social_rank ~ 1, data = students), ols_model)$`Pr(>F)`[2],
  # joint test that all education coefficients are zero (clustered)
  fe_cluster_p   = lfe::waldtest(fe_model, seq_along(coef(fe_model)))[["p.F"]],
  partial_r2     = 1 - sum(residuals(fe_model)^2) / sum(residuals(fe_only)^2),
  n              = nrow(students)
)

table_s9_b


## ---- s4-gap-distribution ----
# Table S10: distribution of the Social Rank GAP
gap_distribution <- function(data, label) {
  tibble(sample = label, n = nrow(data), min = min(data$rank_gap),
         p10 = quantile(data$rank_gap, 0.10), p25 = quantile(data$rank_gap, 0.25),
         median = median(data$rank_gap), p75 = quantile(data$rank_gap, 0.75),
         p90 = quantile(data$rank_gap, 0.90), sd = sd(data$rank_gap))
}

table_s10 <- bind_rows(
  gap_distribution(dyads, "All directed interactions"),
  gap_distribution(filter(dyads, friendship == "Non-friends"), "Non-friends"),
  gap_distribution(filter(dyads, friendship %in% c("Sender declares", "Receiver declares")),
                   "Unilateral friendships"),
  gap_distribution(filter(dyads, friendship == "Mutual"), "Mutual friends")
)

table_s10 %>% mutate(across(where(is.double), ~ round(.x, 3)))


## ---- s4-figure-s8-panels ----
# Figure S8: Social Rank against each student characteristic. Gray points are
# children; black points are binned means with 95% CI; the orange line is a
# linear fit, with the pooled Pearson correlation in the box. Box plots for
# the categorical characteristics. The same scatter panel is drawn 15 times
# and the same box panel 3 times, so each is written once as a function.
s8_titles <- c(
  nom_q3 = "A. Best friends", nom_q1 = "B. Most like to hang out",
  nom_q2 = "C. Least like to hang out", net_popularity = "D. Net popularity",
  nom_q4 = "E. Most popular", nom_q5 = "F. Not popular",
  nom_q6 = "G. Initiate fights", nom_q7 = "H. Make fun of others",
  nom_q8 = "I. Kind to others", nom_q9 = "J. Cooperative",
  aggression = "K. Aggression (composite)", prosociality = "L. Prosociality (composite)",
  attendance = "M. Attendance previous term", gpa = "N. Overall GPA previous term",
  age = "O. Age (within-class)"
)
theme_s8 <- theme_si_frame(base_size = 7) +
  theme(axis.title.y = element_text(size = 6))
x_rank <- scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

# One scatter panel: children (gray), binned means with 95% CI (black),
# linear fit and Pearson correlation (orange)
scatter_panel <- function(variable, y_title = NULL) {
  data <- correlates %>%
    transmute(social_rank, value = .data[[variable]]) %>%
    filter(!is.na(value))
  test <- cor.test(data$social_rank, data$value)
  label <- sprintf("R = %.2f, %s", test$estimate, format_p(test$p.value))
  ggplot(data, aes(social_rank, value)) +
    geom_point(color = color_slate, alpha = 0.25, size = 0.4,
               position = position_jitter(width = 0.005, height = 0.05, seed = 1)) +
    geom_smooth(method = "lm", formula = y ~ x, color = color_highlight, fill = color_highlight,
                alpha = 0.2, linewidth = 0.6) +
    stat_summary_bin(fun.data = mean_cl_normal, bins = 10, size = 0.15, linewidth = 0.3) +
    annotate("label", x = 0.5, y = -2.7, label = label, color = color_highlight, size = 2.1,
             fontface = "bold", label.size = 0.3, label.padding = unit(0.12, "lines")) +
    coord_cartesian(ylim = c(-3, 3.2)) +
    scale_y_continuous(breaks = -3:3) +
    x_rank +
    labs(title = s8_titles[[variable]], x = NULL, y = y_title) +
    theme_s8
}

# One box-plot panel for a categorical characteristic, group labels above
# the boxes, a classroom-clustered joint test, and partial R2.
box_panel <- function(data, group, title, fills, y_title = NULL) {
  data <- data %>%
    filter(!is.na(.data[[group]])) %>%
    mutate(group = .data[[group]], class_id = factor(class_id))
  fit <- felm(social_rank ~ group | class_id | 0 | class_id, data = data)
  classroom_only <- felm(social_rank ~ 1 | class_id, data = data)
  group_p <- lfe::waldtest(fit, seq_along(coef(fit)), type = "cluster")[["p.F"]]
  partial_r2 <- 1 - sum(residuals(fit)^2) / sum(residuals(classroom_only)^2)
  counts <- data %>% count(group) %>%
    mutate(y = rev(seq_along(group)), label = paste0(group, " (n=", n, ")"))
  data <- data %>% left_join(counts, by = "group")
  stats <- sprintf("'group p = %.2f,  partial '*italic(R)^2*' = %.3f'", group_p, partial_r2)
  ggplot(data, aes(social_rank, y)) +
    geom_point(color = color_slate, alpha = 0.25, size = 0.4,
               position = position_jitter(height = 0.12, seed = 1)) +
    geom_boxplot(aes(group = y, fill = group), color = "grey25", alpha = 0.75, width = 0.45,
                 outlier.shape = NA, linewidth = 0.3, coef = Inf) +
    geom_text(data = counts, aes(x = 0.5, y = y + 0.42, label = label),
              fontface = "bold", size = 2.2, color = "grey20") +
    annotate("label", x = 0.5, y = 0.35, label = stats, parse = TRUE,
             color = color_highlight, size = 2.1, fontface = "bold", label.size = 0.3) +
    scale_fill_manual(values = setNames(fills, counts$group)) +
    scale_y_continuous(limits = c(0.1, nrow(counts) + 0.6), breaks = NULL) +
    x_rank +
    labs(title = title, x = NULL, y = y_title) +
    theme_s8
}
blues <- function(n) colorRampPalette(c("#DCE6F0", "#4E6E8E"))(n)

categorical <- students %>%
  mutate(sex = factor(if_else(female == 1, "Female", "Male"), levels = c("Male", "Female")),
         # the two primary categories are merged into "Primary or less", as in the SI
         guardian = str_remove(guardian_education, "^[0-9]+ "),
         guardian = if_else(str_detect(guardian, "^Primary"), "Primary or less", guardian),
         guardian = factor(guardian, levels = unique(guardian[order(guardian_education)])),
         repetition = factor(if_else(repeated_grade == 1, "Repeated", "Not repeated"),
                             levels = c("Not repeated", "Repeated")))
n_guardian <- n_distinct(na.omit(categorical$guardian))

## ---- s4-figure-s8 ----
y_titles <- c(nom_q3 = "Peer nominations (Z-score)", nom_q4 = "Peer nominations (Z-score)",
              nom_q8 = "Peer nominations (Z-score)", attendance = "Z-score")
grid_panels <- map(names(s8_titles), function(v) {
  scatter_panel(v, if (v %in% names(y_titles)) y_titles[[v]] else NULL)
})
sex_panel <- box_panel(categorical, "sex", "P. Sex", c(color_male, color_female), "Sex")

# Religion (Panel R in the SI) is not part of the public data
bottom_row <- box_panel(categorical, "guardian", "Q. Guardian education", blues(n_guardian)) +
  box_panel(categorical, "repetition", "S. Grade repetition", blues(3)[2:3]) +
  plot_layout(widths = c(2.6, 1))

figure_s8 <- (wrap_plots(c(grid_panels, list(sex_panel)), ncol = 4) / bottom_row) +
  plot_layout(heights = c(4, 1.25)) +
  plot_annotation(title = "Popularity-based Social Rank and related student characteristics",
                  caption = "Popularity-based Social Rank (normalized PageRank)",
                  theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
                                plot.caption = element_text(hjust = 0.5, size = 9)))
figure_s8

## ---- s4-save ----
save_table(table_s7, "S4_table_s7_coding")
save_table(construction_agreement, "S4_construction_agreement")
save_table(table_s9_a, "S4_table_s9a_correlates")
save_table(table_s9_b, "S4_table_s9b_background")
save_table(table_s10, "S4_table_s10_gap_distribution")
write_csv(alternative_ranks, here("data", "generated", "alternative_ranks.csv"))

save_si_figure <- function(plot, name, width, height) {
  ggsave(here("output", "figures", "si", paste0(name, ".pdf")), plot, width = width, height = height)
  ggsave(here("output", "figures", "si", paste0(name, ".png")), plot, width = width, height = height, dpi = 300)
}
save_si_figure(figure_s6, "S4_figure_s6_construction_correlations", 6.5, 5.5)
save_si_figure(figure_s7, "S4_figure_s7_reference_weight", 10, 3.4)
save_si_figure(figure_s8, "S4_figure_s8_correlates", 9, 10.5)
