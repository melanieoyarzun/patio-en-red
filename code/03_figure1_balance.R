# =============================================================================
# 03_figure1_balance.R
#
# Figure 1D: balance check for the random assignment of partners.
#
# Question: do children's own characteristics (sex, attendance, GPA) predict
# being paired with higher- or lower-ranked partners? If partners were
# assigned at random, they should not.
#
#   Input : data/generated/analysis_dyads.csv, data/generated/analysis_students.csv
#   Output: output/tables/figure1d_balance.csv
#           output/figures/figure1d_balance.(pdf|png)
# =============================================================================

## ---- balance-packages ----
library(tidyverse)
library(lfe)
library(here)
source(here("code", "00_style.R"))

dyads    <- read_csv(here("data", "generated", "analysis_dyads.csv"),    show_col_types = FALSE)
students <- read_csv(here("data", "generated", "analysis_students.csv"), show_col_types = FALSE)


## ---- balance-data ----
# Add the sender's characteristics to each decision
balance_data <- dyads %>%
  left_join(students %>% select(sender_id = student_id, female, attendance_z, gpa),
            by = "sender_id") %>%
  mutate(class_round = paste(class_id, round, sep = "_"))


## ---- expected-partner-rank ----
# Within a classroom and round, a child can only be paired with the other
# children who play that round. So the rank a child should expect for her
# partner is the average rank of the *other* active children in that
# class-round (the "leave-one-out" mean).
#
#   expected partner rank_i = (sum of ranks in class-round - own rank) / (n - 1)
#
# The balance outcome is the assigned partner's rank minus that expectation.

balance_data <- balance_data %>%
  group_by(class_round) %>%
  mutate(n_active = n(),
         expected_partner_rank = (sum(sender_rank) - sender_rank) / (n_active - 1),
         partner_rank_demeaned = receiver_rank - expected_partner_rank) %>%
  ungroup()

summary(balance_data$partner_rank_demeaned)


## ---- balance-model ----
# Regress the demeaned partner rank on the child's characteristics, with
# classroom-by-round fixed effects and standard errors clustered by classroom.
# Coefficients near zero = no systematic sorting toward higher- or lower-ranked
# partners.

balance_model <- felm(partner_rank_demeaned ~ female + attendance_z + gpa |
                        class_round | 0 | class_id,
                      data = balance_data)

summary(balance_model)

balance_table <- summary(balance_model)$coefficients %>%
  as_tibble(rownames = "term") %>%
  rename(estimate = Estimate, se = `Cluster s.e.`, p_value = `Pr(>|t|)`) %>%
  mutate(conf_low  = estimate - 1.96 * se,
         conf_high = estimate + 1.96 * se,
         r2 = summary(balance_model)$r.squared)

balance_table


## ---- balance-plot ----
balance_plot <- balance_table %>%
  mutate(label = recode(term,
                        female       = "Sex\n(is female)",
                        attendance_z = "Attendance",
                        gpa          = "GPA"),
         label = factor(label, levels = c("GPA", "Attendance", "Sex\n(is female)"))) %>%
  ggplot(aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.2,
                 color = "#4E8C8C", linewidth = 0.7) +
  geom_point(size = 2.5, color = "#4E8C8C") +
  annotate("text", x = 0.0009, y = 0.6, size = 3,
           label = sprintf("R2 = %.1e", balance_table$r2[1])) +
  scale_x_continuous(limits = c(-0.0013, 0.0013), breaks = c(-0.001, 0, 0.001)) +
  labs(x = "Balance regression coefficients\non partner rank (demeaned)", y = NULL) +
  theme_paper()

balance_plot


## ---- balance-save ----
write_csv(balance_table, here("output", "tables", "figure1d_balance.csv"))
save_figure(balance_plot, "figure1d_balance", width = 3.6, height = 2.6)
