# =============================================================================
# S2_figures.R
#
# Figures of Supporting Information, Section 2 (peer-nomination survey), with the SI layout, colors, panel titles and annotations.
#
# The analysis script code/si/S2_*.R computes the data and calls these
# functions. The notebooks show a quick version of each figure and keep this
# code in a collapsed box.
# =============================================================================

## ---- plot-figure-s1 ----
# Figure S1: share of children who named 0-5 classmates in each question; the
# bar at the limit of five in orange, the median as a dotted line.
plot_figure_s1 <- function(out_nominations, question_labels) {
  panel_letters <- c("A. ", "B. ", "C. ")
  names(panel_letters) <- question_labels

  bars <- out_nominations %>%
    count(question, nominations) %>%
    group_by(question) %>%
    mutate(percent = 100 * n / sum(n),
           at_cap  = nominations == 5,
           panel   = paste0(panel_letters[question], question)) %>%
    ungroup()

  medians <- out_nominations %>%
    group_by(question) %>%
    summarise(median = median(nominations)) %>%
    mutate(panel = paste0(panel_letters[question], question))

  figure_s1 <- ggplot(bars, aes(x = nominations, y = n)) +
    geom_col(aes(fill = at_cap), width = 0.8) +
    # percentages inside the bars (white), or above the shortest ones (dark)
    geom_text(data = filter(bars, n >= 40),
              aes(label = paste0(round(percent), "%"),
                  fontface = if_else(at_cap, "bold", "plain")),
              vjust = 1.5, color = "white", size = 2.6) +
    geom_text(data = filter(bars, n < 40), aes(label = paste0(round(percent), "%")),
              vjust = -0.5, color = color_text, size = 2.6) +
    geom_vline(data = medians, aes(xintercept = median), linetype = "dotted") +
    facet_wrap(~ panel) +
    scale_fill_manual(values = c(`FALSE` = color_slate, `TRUE` = color_highlight)) +
    scale_x_continuous(breaks = 0:5) +
    labs(x = "Nominations sent (out-degree)", y = "Students") +
    theme_si()
  figure_s1
}
