# =============================================================================
# 05_figure3_cooperation.R
#
# Figure 3: cooperation across friendship contexts and social rank.
# Three rows (I non-friends, II mutual friends, III sender-declared one-sided
# friendships) and three panels per row:
#   A. heatmap of the tokens exchanged within each pair
#   B. mean tokens sent and received along the Social Rank GAP
#   C. Cooperation GAP along the Social Rank GAP (observed and null-model filtered)
#
# Each panel is saved on its own (9 files). The paper assembles them.
#
#   Input : data/generated/analysis_dyads.csv
#   Output: output/figures/figure3_{I,II,III}_{A,B,C}.(pdf|png)
#           output/tables/figure3_heatmap_cells.csv
#           output/tables/figure3_binned_means.csv
# =============================================================================

## ---- fig3-packages ----
library(tidyverse)
library(here)
source(here("code", "00_style.R"))

dyads <- read_csv(here("data", "generated", "analysis_dyads.csv"), show_col_types = FALSE)


## ---- heatmap-data ----
# Panel A. Every pair exchanges two numbers: what i sends to j and what j
# sends to i. The order does not matter for the heatmap (5 and 2 is the same
# pair as 2 and 5), so we keep the larger number on the x axis and the
# smaller on the y axis. That is why the heatmap is a triangle.
#
# One-sided friendships appear twice in the data (as "Sender declares" from
# one side and "Receiver declares" from the other), so for the heatmap we put
# both together in one "Asymmetric" group. Percentages are computed within
# each group.

heatmap_cells <- dyads %>%
  mutate(group = case_when(friendship == "Non-friends" ~ "Non-friends",
                           friendship == "Mutual"      ~ "Mutual friends",
                           TRUE                        ~ "Asymmetric"),
         high = pmax(tokens_sent, tokens_received),
         low  = pmin(tokens_sent, tokens_received)) %>%
  count(group, high, low) %>%
  group_by(group) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup() %>%
  # add the combinations that never happened, with 0%
  complete(group, high = 0:10, low = 0:10, fill = list(n = 0, percent = 0)) %>%
  filter(low <= high)

# Three regions highlighted in the paper
heatmap_regions <- heatmap_cells %>%
  group_by(group) %>%
  summarise(
    low_cooperation  = sum(percent[high <= 1]),                 # both sent 0 or 1 (magenta)
    asymmetric       = sum(percent[high == 10 & low <= 5]),     # one sent 10, the other 5 or less (blue)
    full_cooperation = sum(percent[high == 10 & low == 10])     # both sent 10 (black)
  )

heatmap_regions


## ---- heatmap-plot ----
plot_heatmap <- function(this_group) {
  cells   <- heatmap_cells   %>% filter(group == this_group)
  regions <- heatmap_regions %>% filter(group == this_group)

  ggplot(cells, aes(x = high, y = low, fill = percent)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(percent, 1)), size = 2) +
    # magenta: low or no cooperation
    annotate("rect", xmin = -0.5, xmax = 1.5, ymin = -0.5, ymax = 1.5,
             color = "#F000D0", fill = NA, linewidth = 0.8) +
    annotate("label", x = 0.5, y = 2, label = round(regions$low_cooperation, 1),
             color = "#F000D0", size = 2.3, fontface = "bold") +
    # blue: highly asymmetric
    annotate("rect", xmin = 9.5, xmax = 10.5, ymin = -0.5, ymax = 5.5,
             color = "#173BFF", fill = NA, linewidth = 0.8) +
    annotate("label", x = 11, y = 5.5, label = round(regions$asymmetric, 1),
             color = "#173BFF", size = 2.3, fontface = "bold") +
    # black: full mutual cooperation
    annotate("rect", xmin = 9.5, xmax = 10.5, ymin = 9.5, ymax = 10.5,
             color = "black", fill = NA, linewidth = 0.9) +
    scale_fill_gradientn(colours = c("#FFF4D6", "#F7E08A", "#E7B56A", "#D9874C", "#C95A3A", "#A51C1C"),
                         values = scales::rescale(c(0, 2, 5, 10, 15, 20)),
                         limits = c(0, 20), oob = scales::squish, name = "%") +
    scale_x_continuous(breaks = 0:10, expand = c(0, 0)) +
    scale_y_continuous(breaks = 0:10, expand = c(0, 0)) +
    coord_fixed(clip = "off") +
    labs(x = "Tokens i to j", y = "Tokens j to i", title = this_group) +
    theme_paper(base_size = 9) +
    theme(panel.border = element_blank(), legend.position = "right")
}

plot_heatmap("Non-friends")


## ---- binned-data ----
# Panels B and C follow the logic of the original analysis:
#   1. for each child (sender), average over her partners in a given
#      friendship group: tokens sent, tokens received, Cooperation GAP, and
#      the median Social Rank GAP of those partners;
#   2. split children into 7 equal-width bins of that median rank gap;
#      (children whose transfers were all removed by the filter drop out of
#      the filtered version, but the bins stay the same)
#   3. in each bin, average across children, with a 95% confidence interval
#      (mean +/- 1.96 * SD / sqrt(n)).
#
# Panel C repeats this with the null-model filtered tokens (only transfers
# that depart most from each child's usual giving; see 01_build_measures.R).

binned_means <- function(data, sent, received, version) {
  data %>%
    group_by(sender_id) %>%
    summarise(gap      = median(rank_gap),
              sent     = mean({{ sent }}, na.rm = TRUE),
              received = mean({{ received }}, na.rm = TRUE),
              coop_gap = mean({{ received }} - {{ sent }}, na.rm = TRUE)) %>%
    # bins are defined with all children, so the observed and the filtered
    # versions use exactly the same bins
    mutate(bin = cut_interval(gap, 7)) %>%
    filter(!is.na(coop_gap)) %>%
    group_by(bin) %>%
    summarise(gap           = mean(gap),
              n             = n(),
              mean_sent     = mean(sent),
              ci_sent       = 1.96 * sd(sent) / sqrt(n),
              mean_received = mean(received),
              ci_received   = 1.96 * sd(received) / sqrt(n),
              mean_coop_gap = mean(coop_gap),
              ci_coop_gap   = 1.96 * sd(coop_gap) / sqrt(n)) %>%
    mutate(version = version)
}

rank_groups <- c("Non-friends", "Mutual", "Sender declares")

binned <- map_dfr(rank_groups, function(this_group) {
  group_data <- dyads %>% filter(friendship == this_group)
  bind_rows(
    binned_means(group_data, tokens_sent, tokens_received, "Observed"),
    binned_means(group_data, tokens_sent_filtered, tokens_received_filtered, "Filtered")
  ) %>%
    mutate(group = this_group)
})

binned %>% filter(group == "Non-friends", version == "Observed")


## ---- panel-b-plot ----
mean_tokens <- mean(dyads$tokens_sent)   # dashed horizontal line: overall mean

plot_tokens <- function(this_group, color) {
  bins <- binned %>% filter(group == this_group, version == "Observed")

  ggplot(bins, aes(x = gap)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    geom_hline(yintercept = mean_tokens, linetype = "dashed", color = "grey50") +
    # yellow area between the two lines = the Cooperation GAP
    geom_ribbon(aes(ymin = mean_received, ymax = mean_sent), fill = "#F0E5A8", alpha = 0.7) +
    # sent: rhombi
    geom_line(aes(y = mean_sent), color = color, linewidth = 0.9) +
    geom_errorbar(aes(ymin = mean_sent - ci_sent, ymax = mean_sent + ci_sent),
                  color = color, width = 0.04) +
    geom_point(aes(y = mean_sent), color = color, shape = 18, size = 3) +
    # received: squares
    geom_line(aes(y = mean_received), color = color, linewidth = 0.9) +
    geom_errorbar(aes(ymin = mean_received - ci_received, ymax = mean_received + ci_received),
                  color = color, width = 0.04) +
    geom_point(aes(y = mean_received), color = color, shape = 15, size = 2.5) +
    annotate("text", x = c(-0.55, 0.55), y = 0.4, size = 2.3, fontface = "bold",
             label = c("Receiver higher rank", "Sender higher rank")) +
    scale_x_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, 0.5)) +
    scale_y_continuous(limits = c(0, 11), breaks = c(0, 5, 10)) +
    labs(x = "Social Rank GAP", y = "Tokens") +
    theme_paper(base_size = 9)
}

plot_tokens("Non-friends", color_non_friends)


## ---- panel-c-plot ----
plot_coop_gap <- function(this_group, color, y_limits = c(-6, 6)) {
  bins <- binned %>% filter(group == this_group)

  ggplot(bins, aes(x = gap, y = mean_coop_gap, alpha = version)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_line(color = color, linewidth = 0.9) +
    geom_errorbar(aes(ymin = mean_coop_gap - ci_coop_gap, ymax = mean_coop_gap + ci_coop_gap),
                  color = color, width = 0.04) +
    geom_point(color = color, size = 2.8) +
    scale_alpha_manual(values = c(Observed = 1, Filtered = 0.4)) +
    annotate("text", x = c(-0.55, 0.55), y = y_limits[1] + 0.4, size = 2.3, fontface = "bold",
             label = c("Receiver higher rank", "Sender higher rank")) +
    annotate("text", x = 1.05, y = y_limits[2] - c(0.3, 1.1), hjust = 1, size = 2.5,
             fontface = "bold", color = color, alpha = c(1, 0.4),
             label = c("Observed data", "Null-model filtered")) +
    scale_x_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, 0.5)) +
    scale_y_continuous(limits = y_limits, breaks = seq(-7.5, 5, 2.5)) +
    labs(x = "Social Rank GAP", y = "Cooperation GAP\n[Received - Sent]") +
    theme_paper(base_size = 9)
}

plot_coop_gap("Non-friends", color_non_friends)


## ---- all-panels ----
rows <- tribble(
  ~row,  ~heatmap_group,   ~rank_group,       ~color,            ~y_min,
  "I",   "Non-friends",    "Non-friends",     color_non_friends, -6,
  "II",  "Mutual friends", "Mutual",          color_mutual,      -6,
  "III", "Asymmetric",     "Sender declares", color_asymmetric,  -8.5
)

for (k in seq_len(nrow(rows))) {
  r <- rows[k, ]
  save_figure(plot_heatmap(r$heatmap_group),
              paste0("figure3_", r$row, "_A_heatmap"), width = 3.6, height = 3.2)
  save_figure(plot_tokens(r$rank_group, r$color),
              paste0("figure3_", r$row, "_B_tokens"), width = 3.2, height = 3)
  save_figure(plot_coop_gap(r$rank_group, r$color, y_limits = c(r$y_min, 6)),
              paste0("figure3_", r$row, "_C_coop_gap"), width = 3.2, height = 3)
}


## ---- fig3-save ----
write_csv(heatmap_cells, here("output", "tables", "figure3_heatmap_cells.csv"))
write_csv(binned,        here("output", "tables", "figure3_binned_means.csv"))
