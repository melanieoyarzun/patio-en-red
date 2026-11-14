# =============================================================================
# S2_nomination_survey.R      SI Section 2: Peer-nomination survey
#
#   Table S3   Out-nominations per question and the upper limit of five
#   Figure S1  Distribution of nominations sent per student
#   Table S4   Consistency of the two popularity questions
#
#   Input : data/generated/analysis_dyads.csv
#   Output: output/tables/si/S2_*.csv, output/figures/si/S2_*.(pdf|png)
# =============================================================================

## ---- s2-setup ----
source(here::here("code", "00_helpers.R"))
source(here("code", "00_style.R"))
dir.create(here("output", "figures", "si"), recursive = TRUE, showWarnings = FALSE)

dyads <- read_csv(here("data", "generated", "analysis_dyads.csv"), show_col_types = FALSE)

question_labels <- c(nom_q3 = "Best friends (Q3)",
                     nom_q4 = "Most popular & well-known (Q4)",
                     nom_q5 = "Not popular or well-known (Q5)")


## ---- s2-out-nominations ----
# How many classmates each child named in each question
out_nominations <- dyads %>%
  group_by(sender_id) %>%
  summarise(across(c(nom_q3, nom_q4, nom_q5), sum)) %>%
  pivot_longer(-sender_id, names_to = "question", values_to = "nominations") %>%
  mutate(question = question_labels[question])

table_s3 <- out_nominations %>%
  group_by(question) %>%
  summarise(mean      = mean(nominations),
            sd        = sd(nominations),
            median    = median(nominations),
            at_cap    = 100 * mean(nominations == 5),
            below_min = 100 * mean(nominations < 2),
            above_cap = sum(nominations > 5))

table_s3


## ---- s2-figure-s1 ----
figure_s1 <- ggplot(out_nominations, aes(x = nominations)) +
  geom_bar(fill = "grey55", width = 0.8) +
  geom_vline(xintercept = 5.5, linetype = "dashed") +
  facet_wrap(~ question) +
  scale_x_continuous(breaks = 0:5) +
  labs(x = "Nominations sent", y = "Students") +
  theme_paper(base_size = 10) +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold"))

figure_s1


## ---- s2-consistency ----
# Table S4: the same classmate named as "most popular" (Q4) and as
# "not popular" (Q5) by the same child. Each conflict involves two
# nominations, so its share of all popularity nominations is
# 2 x conflicts / (Q4 nominations + Q5 nominations).

conflicts <- dyads %>% filter(nom_q4 == 1, nom_q5 == 1)

table_s4 <- tibble(
  conflicting_pairs     = nrow(conflicts),
  share_of_nominations  = 100 * 2 * nrow(conflicts) / (sum(dyads$nom_q4) + sum(dyads$nom_q5)),
  children_with_conflict = n_distinct(conflicts$sender_id),
  classes_with_conflict  = n_distinct(conflicts$class_id)
)

table_s4


## ---- s2-save ----
save_table(table_s3, "S2_table_s3_nomination_cap")
save_table(table_s4, "S2_table_s4_consistency")
ggsave(here("output", "figures", "si", "S2_figure_s1_nominations.pdf"), figure_s1, width = 8, height = 3)
ggsave(here("output", "figures", "si", "S2_figure_s1_nominations.png"), figure_s1, width = 8, height = 3, dpi = 300)
