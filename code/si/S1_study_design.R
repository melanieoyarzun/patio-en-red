# =============================================================================
# S1_study_design.R          SI Section 1: Study design and data description
#
#   Table S1  Classroom-level descriptive statistics
#   Table S2  Completeness of the variables
#
#   Input : data/generated/analysis_students.csv, data/generated/analysis_dyads.csv
#   Output: output/tables/si/S1_*.csv
# =============================================================================

## ---- s1-setup ----
source(here::here("code", "00_helpers.R"))

students <- read_csv(here("data", "generated", "analysis_students.csv"), show_col_types = FALSE)
dyads    <- read_csv(here("data", "generated", "analysis_dyads.csv"),    show_col_types = FALSE)


## ---- s1-table-classrooms ----
# Table S1: one row per classroom.
# Age: two computed ages above 15 years are birth-date errors and are left
# out of the age summaries (as stated in the SI); four children have no
# birth date.

tokens_by_class <- dyads %>%
  group_by(class_id) %>%
  summarise(cooperation = mean(tokens_sent))

table_s1 <- students %>%
  mutate(age = if_else(age > 15, NA_real_, age)) %>%
  group_by(class_id) %>%
  summarise(school_id          = first(school_id),
            age_mean           = mean(age, na.rm = TRUE),
            age_sd             = sd(age, na.rm = TRUE),
            female_pct         = 100 * mean(female),
            guardian_secondary = 100 * mean(guardian_secondary),
            gpa                = mean(gpa),
            class_size         = n(),
            attendance         = mean(attendance_2017, na.rm = TRUE)) %>%
  left_join(tokens_by_class, by = "class_id")

## ---- s1-sample-totals ----
# Totals quoted in the SI and the main text
students %>%
  mutate(age = if_else(age > 15, NA_real_, age)) %>%
  summarise(children   = n(),
            classrooms = n_distinct(class_id),
            schools    = n_distinct(school_id),
            age_mean   = mean(age, na.rm = TRUE),
            age_sd     = sd(age, na.rm = TRUE),
            female_pct = 100 * mean(female))

## ---- s1-classroom-composition ----
# Classroom composition by sex: mixed, all girls, all boys
table_s1 %>%
  count(composition = case_when(female_pct == 100 ~ "all girls",
                                female_pct == 0   ~ "all boys",
                                TRUE              ~ "mixed"))


## ---- s1-table-completeness ----
# Table S2: share of non-missing values of each variable
table_s2 <- tribble(
  ~variable,                         ~n,               ~complete_pct,
  "Popularity nominations (Q4, Q5)", nrow(students),   100 * mean(!is.na(dyads$nom_q4) & !is.na(dyads$nom_q5)),
  "Best-friend nominations (Q3)",    nrow(students),   100 * mean(!is.na(dyads$nom_q3)),
  "Directed game decisions",         nrow(dyads),      100 * mean(!is.na(dyads$tokens_sent)),
  "Sex",                             nrow(students),   100 * mean(!is.na(students$female)),
  "GPA (previous term)",             nrow(students),   100 * mean(!is.na(students$gpa)),
  "Attendance (previous term)",      nrow(students),   100 * mean(!is.na(students$attendance)),
  "Age",                             nrow(students),   100 * mean(!is.na(students$age)),
  "Guardian education",              nrow(students),   100 * mean(!is.na(students$guardian_secondary))
)

table_s2

# Every classroom has the full census of directed decisions, n(n - 1)
dyads %>%
  count(class_id, name = "decisions") %>%
  left_join(students %>% count(class_id, name = "n"), by = "class_id") %>%
  summarise(all_complete = all(decisions == n * (n - 1)))


## ---- s1-save ----
save_table(table_s1, "S1_table_s1_classrooms")
save_table(table_s2, "S1_table_s2_completeness")
