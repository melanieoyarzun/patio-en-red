# =============================================================================
# S3_relational_structure.R   SI Section 3: Classroom relational structure
#
#   Figure S2    All relational layers of one classroom (class 20)
#   Figures S3-S5 Sociograms of the three focal questions for a small (16),
#                 an intermediate (40) and a large (1) classroom
#   Table S5     Summary of the three focal networks over the 45 classrooms
#   Table S6     The same statistics, classroom by classroom
#
#   Input : data/generated/analysis_dyads.csv, data/generated/analysis_students.csv
#   Output: output/tables/si/S3_*.csv, output/figures/si/S3_*.(pdf|png)
# =============================================================================

## ---- s3-setup ----
source(here::here("code", "00_helpers.R"))
source(here("code", "00_style.R"))
source(here("code", "si", "figures", "S3_figures.R"))   # plotting functions of this section
library(igraph)
library(ggraph)
library(patchwork)
dir.create(here("output", "figures", "si"), recursive = TRUE, showWarnings = FALSE)

dyads    <- read_csv(here("data", "generated", "analysis_dyads.csv"),    show_col_types = FALSE)
students <- read_csv(here("data", "generated", "analysis_students.csv"), show_col_types = FALSE)

question_names <- c(
  nom_q1 = "Most like to hang out (Q1)",  nom_q2 = "Least like to hang out (Q2)",
  nom_q3 = "Best friends (Q3)",            nom_q4 = "Most popular (Q4)",
  nom_q5 = "Not popular (Q5)",             nom_q6 = "Start fights (Q6)",
  nom_q7 = "Make fun of others (Q7)",      nom_q8 = "Kind to others (Q8)",
  nom_q9 = "Cooperate (Q9)"
)


## ---- s3-network-statistics ----
# For one classroom and one question, the directed nomination network and
# its statistics. Ties are nominations i -> j; "no tie" is an observed
# non-nomination, not missing data.

network_statistics <- function(this_class, question) {
  class_dyads <- dyads %>% filter(class_id == this_class)
  n_children <- n_distinct(class_dyads$sender_id)
  nominations <- class_dyads %>% filter(.data[[question]] == 1)

  received <- class_dyads %>% group_by(id = receiver_id) %>% summarise(k_in = sum(.data[[question]]))
  sent     <- class_dyads %>% group_by(id = sender_id)   %>% summarise(k_out = sum(.data[[question]]))
  degrees  <- full_join(received, sent, by = "id")

  # reciprocity: share of ties whose reverse tie also exists
  reciprocated <- nominations %>%
    semi_join(nominations, by = c("sender_id" = "receiver_id", "receiver_id" = "sender_id"))

  tibble(class_id      = this_class,
         question      = question,
         n             = n_children,
         ties          = nrow(nominations),
         density       = nrow(nominations) / (n_children * (n_children - 1)),
         mean_received = mean(degrees$k_in),
         min_received  = min(degrees$k_in),
         max_received  = max(degrees$k_in),
         zero_received = sum(degrees$k_in == 0),
         reciprocity   = nrow(reciprocated) / nrow(nominations),
         isolates      = sum(degrees$k_in == 0 & degrees$k_out == 0))
}

table_s6 <- expand_grid(this_class = sort(unique(dyads$class_id)),
                        question   = c("nom_q3", "nom_q4", "nom_q5")) %>%
  pmap_dfr(network_statistics) %>%
  left_join(students %>% distinct(class_id, school_id), by = "class_id") %>%
  mutate(network = question_names[question], .after = n)

table_s6 %>% filter(class_id == 1)


## ---- s3-network-summary ----
# Table S5: median and range over the 45 classrooms
median_range <- function(x, digits = 2) {
  paste0(format(round(median(x), digits), nsmall = digits),
         " [", format(round(min(x), digits), nsmall = digits), "-",
         format(round(max(x), digits), nsmall = digits), "]")
}

table_s5 <- table_s6 %>%
  group_by(network) %>%
  summarise(`Classroom size`             = median_range(n, 0),
            `Directed ties`              = median_range(ties, 0),
            Density                      = median_range(density),
            `Mean nominations received`  = median_range(mean_received),
            `Max nominations received`   = median_range(max_received, 0),
            Reciprocity                  = median_range(reciprocity),
            `Students with zero received` = median_range(zero_received, 0),
            Isolates                     = median_range(isolates, 0)) %>%
  pivot_longer(-network, names_to = "statistic") %>%
  pivot_wider(names_from = network, values_from = value)

## ---- s3-sociogram-function ----
# One sociogram: one classroom, one layer. Node size = nominations received
# (or tokens received/sent), fill = sex. The layout is fixed per classroom
# so the same child stays in the same place in every panel.

class_layout <- function(this_class, seed = 1) {
  class_dyads <- dyads %>% filter(class_id == this_class)
  links <- class_dyads %>%
    filter(nom_q3 == 1 | nom_q4 == 1 | nom_q5 == 1) %>%
    transmute(from = as.character(sender_id), to = as.character(receiver_id))
  nodes <- students %>% filter(class_id == this_class) %>%
    transmute(name = as.character(student_id), sex = factor(if_else(female == 1, "Female", "Male"), levels = c("Female", "Male")))
  set.seed(seed)
  xy <- layout_with_fr(graph_from_data_frame(links, vertices = nodes))
  nodes %>% mutate(x = xy[, 1], y = xy[, 2])
}

draw_sociogram <- function(this_class, layer, nodes, title = layer) {
  class_dyads <- dyads %>% filter(class_id == this_class) %>%
    mutate(from = as.character(sender_id), to = as.character(receiver_id))

  if (layer %in% c("tokens_sent", "tokens_received")) {
    links <- class_dyads %>% filter(tokens_sent > 0) %>% mutate(width = tokens_sent)
    size  <- class_dyads %>%
      group_by(name = if (layer == "tokens_sent") from else to) %>%
      summarise(size = sum(tokens_sent))
  } else {
    links <- class_dyads %>% filter(.data[[layer]] == 1) %>% mutate(width = 1)
    size  <- class_dyads %>% group_by(name = to) %>% summarise(size = sum(.data[[layer]]))
  }

  # share of ties between two children of the same sex
  sex_of <- setNames(nodes$sex, nodes$name)
  same_sex <- 100 * mean(sex_of[links$from] == sex_of[links$to])

  nodes <- nodes %>% left_join(size, by = "name")
  network <- graph_from_data_frame(links %>% select(from, to, width), vertices = nodes)

  ggraph(network, layout = "manual", x = V(network)$x, y = V(network)$y) +
    geom_edge_link(aes(width = width), color = "grey45", alpha = 0.6,
                   arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
                   end_cap = circle(2.5, "mm")) +
    geom_node_point(aes(size = size, fill = sex), shape = 21, color = "grey20") +
    scale_edge_width(range = c(0.2, 1)) +
    scale_fill_manual(values = c(Female = color_female, Male = color_male)) +
    scale_size_continuous(range = c(1.5, 7)) +
    labs(title = title, subtitle = sprintf("%.0f%% same-sex ties", same_sex)) +
    theme_void(base_size = 8) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))
}


## ---- s3-figure-s2-minimal ----
# Figure S2, minimal version: every layer of class 20 as a grid of sociograms
# (same positions in every panel)
nodes_20 <- class_layout(20, seed = 20)
layers <- c("tokens_sent", "tokens_received", names(question_names))
layer_titles <- c("Tokens sent (out-strength)", "Tokens received (in-strength)", question_names)

figure_s2_minimal <- map2(layers, layer_titles, ~ draw_sociogram(20, .x, nodes_20, .y)) %>%
  wrap_plots(ncol = 3)

figure_s2_minimal


## ---- s3-figure-s2 ----
# Figure S2 as in the SI: the same layers drawn as tilted planes stacked from
# top to bottom and grouped by kind: the game (tokens), friendship, positive
# and negative nominations. Each child keeps the same position in every plane.
figure_s2 <- plot_figure_s2(dyads, nodes_20)
figure_s2


## ---- s3-figures-s3-s5 ----
# Figures S3-S5: the three focal questions for three classroom sizes.
# Gray arrows are nominations; a reciprocated pair is drawn once, curved,
# with a head at each end. Node size = nominations received in that network.
# Quick look: the three focal networks of the small classroom
nodes_16 <- class_layout(16, seed = 16)
map(c("nom_q3", "nom_q4", "nom_q5"), ~ draw_sociogram(16, .x, nodes_16)) %>%
  wrap_plots(nrow = 1)

## ---- s3-figures-s3-s5-si ----
# Supporting Information layout (plotting code in figures/S3_figures.R)
figure_s3 <- plot_focal_layers(dyads, class_layout(16, seed = 16), 16)   # small, 9 children
figure_s4 <- plot_focal_layers(dyads, class_layout(40, seed = 40), 40)   # intermediate, 18 children (all girls)
figure_s5 <- plot_focal_layers(dyads, class_layout(1, seed = 1), 1)      # large, 33 children

figure_s3


## ---- s3-save ----
save_table(table_s5, "S3_table_s5_network_summary")
save_table(table_s6, "S3_table_s6_network_by_class")
save_si_figure <- function(plot, name, width, height) {
  ggsave(here("output", "figures", "si", paste0(name, ".pdf")), plot, width = width, height = height)
  ggsave(here("output", "figures", "si", paste0(name, ".png")), plot, width = width, height = height, dpi = 300)
}
save_si_figure(figure_s2, "S3_figure_s2_class20_layers", 6.5, 10)
save_si_figure(figure_s3, "S3_figure_s3_class16", 9, 3.6)
save_si_figure(figure_s4, "S3_figure_s4_class40", 9, 3.6)
save_si_figure(figure_s5, "S3_figure_s5_class1", 9, 3.6)
