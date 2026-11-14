# =============================================================================
# 00_helpers.R
#
# Small functions used by several SI scripts. Only things that would
# otherwise be copied many times live here.
#
#   fe_progression()   the five fixed-effects specifications of the SI tables
#   regression_table() turns a list of felm models into a table like the SI
#   stars()            significance stars
# =============================================================================

## ---- helpers ----
library(tidyverse)
library(lfe)
library(here)

# Some subgroup models are saturated (more fixed effects than information).
# summary() of such models makes lfe print "Error in chol..." notes about an
# F-test we do not use; quiet_summary() keeps those notes out of the output.
quiet_summary <- function(model) {
  old <- options(try.outFile = nullfile())   # where lfe's try() writes its notes
  on.exit(options(old))
  suppressWarnings(summary(model))
}

# felm() that returns NULL instead of stopping when a model cannot be estimated
quiet_felm <- function(formula, data) {
  old <- options(try.outFile = nullfile())
  on.exit(options(old))
  tryCatch(suppressWarnings(felm(formula, data = data)), error = function(e) NULL)
}

# Significance stars, as in the paper
stars <- function(p) {
  case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "")
}

si_stars <- function(p) {
  if_else(!is.na(p) & p >= 0.05 & p < 0.1, "†", stars(p))
}

# The SI regression tables report the same model with five sets of fixed
# effects, from none to the preferred one:
#   (1) no fixed effects, ordinary standard errors
#   (2) classroom FE
#   (3) classroom + sender FE
#   (4) classroom + sender + round FE
#   (5) sender + classroom-by-round FE   <- preferred (Table 1)
# Columns (2) to (5) cluster standard errors by classroom.
#
# `formula_rhs` is the right-hand side, for example "rank_gap" or
# "rank_gap * mutual".
fe_progression <- function(data, outcome = "coop_gap", formula_rhs = "rank_gap") {
  data <- data %>%
    mutate(class_round = paste(class_id, round, sep = "_"),
           across(c(sender_id, class_id, class_round, round), as.factor))

  f <- function(fixed_effects, cluster) {
    as.formula(paste(outcome, "~", formula_rhs, "|", fixed_effects, "| 0 |", cluster))
  }

  # a model that cannot be estimated (too few observations for its fixed
  # effects) is returned as NULL and marked as not estimable
  fit <- function(formula) quiet_felm(formula, data)

  list(
    "(1)" = fit(f("0", "0")),
    "(2)" = fit(f("class_id", "class_id")),
    "(3)" = fit(f("class_id + sender_id", "class_id")),
    "(4)" = fit(f("class_id + sender_id + round", "class_id")),
    "(5)" = fit(f("sender_id + class_round", "class_id"))
  )
}

# One row per coefficient (estimate and SE), plus N, R2 and adjusted R2,
# in the layout of the SI tables. Works for any list of felm models.
regression_table <- function(models, terms = NULL, digits = 3) {
  specifications <- names(models)
  fitted <- compact(models)
  models <- keep(fitted, ~ .x$N > .x$p)
  if (length(models) == 0L) {
    table <- tibble(row = c(terms, "Observations", "R2", "Adj. R2"))
    for (name in specifications) table[[name]] <- "n.e."
    return(table)
  }
  coef_rows <- map_dfr(names(models), function(name) {
    ct <- quiet_summary(models[[name]])$coefficients
    se_col <- if ("Cluster s.e." %in% colnames(ct)) "Cluster s.e." else "Std. Error"
    tibble(model    = name,
           term     = rownames(ct),
           estimate = ct[, "Estimate"],
           se       = ct[, se_col],
           p_value  = ct[, ncol(ct)])
  })
  if (!is.null(terms)) coef_rows <- coef_rows %>% filter(term %in% terms)

  fit_rows <- map_dfr(names(models), function(name) {
    s <- quiet_summary(models[[name]])
    tibble(model = name,
           Observations = format(models[[name]]$N, big.mark = ","),
           R2           = sprintf(paste0("%.", digits, "f"), s$r.squared),
           `Adj. R2`    = sprintf(paste0("%.", digits, "f"), s$adj.r.squared))
  })

  coefficients <- coef_rows %>%
    mutate(valid = is.finite(estimate) & is.finite(se) & is.finite(p_value),
           cell_estimate = if_else(valid, paste0(sprintf(paste0("%.", digits, "f"), estimate), si_stars(p_value)), "n.e."),
           cell_se = if_else(valid, paste0("(", sprintf(paste0("%.", digits, "f"), se), ")"), "")) %>%
    select(model, term, cell_estimate, cell_se) %>%
    pivot_longer(c(cell_estimate, cell_se), names_to = "what", values_to = "value") %>%
    mutate(row = if_else(what == "cell_estimate", term, "")) %>%
    select(-what) %>%
    pivot_wider(id_cols = c(term, row), names_from = model, values_from = value,
                values_fn = list) %>%
    unnest(cols = -c(term, row)) %>%
    select(-term)

  fits <- fit_rows %>%
    pivot_longer(-model, names_to = "row") %>%
    pivot_wider(names_from = model, values_from = value)

  table <- bind_rows(coefficients, fits) %>%
    mutate(across(everything(), ~ replace_na(.x, "")))

  for (name in setdiff(specifications, names(models))) {
    table[[name]] <- if_else(table$row == "", "", "n.e.")
    if (!is.null(fitted[[name]])) {
      table[[name]][table$row == "Observations"] <- format(fitted[[name]]$N, big.mark = ",")
    }
  }
  table[, c("row", specifications)]
}

# Numbers (estimate, SE, p, N, R2) of one term in every model, for checks
coefficient_summary <- function(models, term) {
  map_dfr(names(models), function(name) {
    ct <- quiet_summary(models[[name]])$coefficients
    se_col <- if ("Cluster s.e." %in% colnames(ct)) "Cluster s.e." else "Std. Error"
    tibble(model = name, estimate = ct[term, "Estimate"], se = ct[term, se_col],
           p_value = ct[term, ncol(ct)], n = models[[name]]$N,
           r2 = quiet_summary(models[[name]])$r.squared,
           r2_adj = quiet_summary(models[[name]])$adj.r.squared)
  })
}

# Save a table as csv in output/tables/si
save_table <- function(table, name) {
  dir.create(here("output", "tables", "si"), recursive = TRUE, showWarnings = FALSE)
  write_csv(table, here("output", "tables", "si", paste0(name, ".csv")))
}
