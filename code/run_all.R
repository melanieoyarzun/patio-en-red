# =============================================================================
# run_all.R
#
# Runs the whole replication, in order. From the replication_package/ folder:
#
#     Rscript code/run_all.R          # main text and Supporting Information
#     Rscript code/run_all.R main     # main text only (Table 1, Figures 1D-3)
#
# Each script can also be run on its own (after 01, which builds the data;
# S8 also needs S4).
# =============================================================================

library(here)

dir.create(here("output", "tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "tables", "si"),  recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures", "si"), recursive = TRUE, showWarnings = FALSE)

scripts <- c(
  "01_build_measures.R",       # raw data -> analysis variables
  "02_table1.R",               # Table 1 and pooled interaction
  "03_figure1_balance.R",      # Figure 1D
  "04_figure2_sociograms.R",   # Figure 2
  "05_figure3_cooperation.R",  # Figure 3
  "06_numbers_in_text.R",      # other numbers in the main text
  # Supporting Information, one script per SI section
  "si/S1_study_design.R",
  "si/S2_nomination_survey.R",
  "si/S3_relational_structure.R",
  "si/S4_social_rank.R",          # also writes data/generated/alternative_ranks.csv (used by S8)
  "si/S5_game_and_time.R",
  "si/S6_identification.R",
  "si/S7_main_regressions.R",
  "si/S8_robustness.R",
  "si/S9_behavioral_rank.R"
)

# "main" as argument: only the main-text scripts (the first six)
if ("main" %in% commandArgs(trailingOnly = TRUE)) {
  scripts <- scripts[!startsWith(scripts, "si/")]
}

for (script in scripts) {
  message("Running ", script)
  source(here("code", script), local = new.env())
}

message("Done. Tables in output/tables, figures in output/figures.")
