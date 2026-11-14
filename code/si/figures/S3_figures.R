# =============================================================================
# S3_figures.R
#
# Figures of Supporting Information, Section 3 (classroom relational structure), with the SI layout, colors, panel titles and annotations.
#
# The analysis script code/si/S3_*.R computes the data and calls these
# functions. The notebooks show a quick version of each figure and keep this
# code in a collapsed box.
# =============================================================================

## ---- plot-figure-s2 ----
# Figure S2 as in the SI: the layers of one classroom drawn as tilted planes,
# stacked from top to bottom and grouped by kind (game, friendship, positive
# and negative nominations). Each child keeps the same position in every plane.
plot_figure_s2 <- function(dyads, nodes_20) {
  nodes_20 <- nodes_20 %>%
    mutate(x = scales::rescale(x, to = c(-1, 1)), y = scales::rescale(y, to = c(-1, 1)))

  layer_info <- tribble(
    ~layer,            ~label,                          ~group,
    "tokens_sent",     "Tokens sent\nout-strength",     "Cooperation game",
    "tokens_received", "Tokens received\nin-strength",  "Cooperation game",
    "nom_q3",          "Q3  Best friends",              "Friendship",
    "nom_q1",          "Q1  Most like to hang out",     "Positive nominations",
    "nom_q4",          "Q4  Most popular and well-known", "Positive nominations",
    "nom_q8",          "Q8  Kind",                      "Positive nominations",
    "nom_q9",          "Q9  Cooperates",                "Positive nominations",
    "nom_q5",          "Q5  Not popular or well-known", "Negative nominations",
    "nom_q2",          "Q2  Least like to hang out",    "Negative nominations",
    "nom_q7",          "Q7  Makes fun of others",       "Negative nominations",
    "nom_q6",          "Q6  Starts fights",             "Negative nominations"
  ) %>%
    mutate(row    = row_number(),
           center = -1.15 * (row - 1),       # vertical position of each plane
           shift  = -0.08 * (row - 1),       # each plane a little to the left
           color  = c(`Cooperation game` = "grey25", Friendship = color_mutual,
                      `Positive nominations` = "#1F3A5F", `Negative nominations` = color_highlight)[group])

  tilt_plane <- function(x, y, center, shift) {
    tibble(X = 1.6 * x + 0.35 * y + shift, Y = 0.36 * y + center)
  }

  class20 <- dyads %>% filter(class_id == 20) %>%
    mutate(from = as.character(sender_id), to = as.character(receiver_id))
  sex_of <- setNames(nodes_20$sex, nodes_20$name)

  # links of every layer (tokens: every transfer of at least one token)
  layer_links <- map_dfr(layer_info$layer, function(this_layer) {
    links <- if (str_starts(this_layer, "tokens")) filter(class20, tokens_sent > 0)
             else filter(class20, .data[[this_layer]] == 1)
    links %>% transmute(layer = this_layer, from, to,
                        mutual = paste(to, from) %in% paste(from, to),
                        weight = if (str_starts(this_layer, "tokens")) tokens_sent else 1)
  })

  # node size: tokens or nominations received (tokens sent for the first plane)
  layer_sizes <- map_dfr(layer_info$layer, function(this_layer) {
    if (this_layer == "tokens_sent") {
      class20 %>% group_by(name = from) %>% summarise(value = sum(tokens_sent))
    } else if (this_layer == "tokens_received") {
      class20 %>% group_by(name = to) %>% summarise(value = sum(tokens_sent))
    } else {
      class20 %>% group_by(name = to) %>% summarise(value = sum(.data[[this_layer]]))
    }
  } %>% mutate(layer = this_layer, size = 0.6 + 2.6 * sqrt(value / max(value))))

  # share of same-sex ties (for tokens, weighted by the number of tokens)
  same_sex <- layer_links %>%
    group_by(layer) %>%
    summarise(same_sex = round(100 * weighted.mean(sex_of[from] == sex_of[to], weight)))

  plane_frames <- layer_info %>%
    rowwise() %>%
    reframe(layer, color, tilt_plane(c(-1.15, 1.15, 1.15, -1.15), c(-1.25, -1.25, 1.25, 1.25), center, shift))

  plane_nodes <- layer_sizes %>%
    left_join(nodes_20, by = "name") %>%
    left_join(layer_info, by = "layer") %>%
    mutate(tilt_plane(x, y, center, shift))

  plane_links <- layer_links %>%
    left_join(nodes_20 %>% select(from = name, x0 = x, y0 = y), by = "from") %>%
    left_join(nodes_20 %>% select(to = name, x1 = x, y1 = y), by = "to") %>%
    left_join(layer_info, by = "layer") %>%
    mutate(tilt_plane(x0, y0, center, shift) %>% rename(X0 = X, Y0 = Y),
           tilt_plane(x1, y1, center, shift) %>% rename(X1 = X, Y1 = Y),
           # friendship: mutual ties in green, one-way ties light
           link_color = case_when(layer == "nom_q3" & mutual ~ color_mutual,
                                  layer == "nom_q3"           ~ "#9FCFC8",
                                  TRUE                        ~ "grey55"))

  plane_labels <- layer_info %>%
    left_join(same_sex, by = "layer") %>%
    mutate(text = paste0(label, if_else(str_starts(layer, "tokens"), " | ", "\n"),
                         same_sex, "% same-sex"),
           X = 1.6 * 1.15 + 0.35 * 1.25 + shift + 0.15)

  group_brackets <- layer_info %>%
    group_by(group, color) %>%
    summarise(top = max(center) + 0.5, bottom = min(center) - 0.5, .groups = "drop") %>%
    mutate(X = -3.25)

  figure_s2 <- ggplot() +
    geom_polygon(data = plane_frames, aes(X, Y, group = layer, color = I(color)),
                 fill = "grey99", linewidth = 0.5) +
    geom_segment(data = plane_links, aes(x = X0, y = Y0, xend = X1, yend = Y1, color = I(link_color)),
                 linewidth = 0.15, alpha = 0.7,
                 arrow = arrow(length = unit(0.6, "mm"), type = "closed")) +
    geom_point(data = plane_nodes, aes(X, Y, size = I(size), fill = sex),
               shape = 21, color = "grey20", stroke = 0.15) +
    geom_text(data = plane_labels, aes(X, center, label = text, color = I(color)),
              hjust = 0, size = 2.1, lineheight = 0.9) +
    geom_segment(data = group_brackets, aes(x = X, xend = X, y = bottom, yend = top, color = I(color)),
                 linewidth = 0.6) +
    geom_segment(data = group_brackets, aes(x = X, xend = X + 0.12, y = top, yend = top, color = I(color)),
                 linewidth = 0.6) +
    geom_segment(data = group_brackets, aes(x = X, xend = X + 0.12, y = bottom, yend = bottom, color = I(color)),
                 linewidth = 0.6) +
    geom_text(data = group_brackets, aes(X - 0.12, (top + bottom) / 2, label = group, color = I(color)),
              angle = 90, fontface = "bold", size = 2.4) +
    scale_fill_manual(values = c(Female = color_female, Male = color_male), name = NULL) +
    coord_fixed(clip = "off", xlim = c(-3.5, 4.1)) +
    labs(title = "Relational layers in one classroom") +
    theme_void(base_size = 9) +
    theme(plot.title = element_text(face = "bold"), legend.position = "top",
          legend.justification = "left")
  figure_s2
}


## ---- plot-focal-layers ----
focal <- c(nom_q3 = "Best friends (Q3)",
           nom_q4 = "Most popular and well-known (Q4)",
           nom_q5 = "Not popular or well-known (Q5)")

draw_focal_layer <- function(dyads, this_class, question, nodes, title, max_size) {
  class_dyads <- dyads %>% filter(class_id == this_class) %>%
    mutate(from = as.character(sender_id), to = as.character(receiver_id))
  links <- class_dyads %>% filter(.data[[question]] == 1) %>% select(from, to)

  # reciprocated pairs: keep one row per pair and mark it
  links <- links %>%
    mutate(mutual = paste(to, from) %in% paste(from, to)) %>%
    filter(!mutual | from < to)

  received <- class_dyads %>% group_by(name = to) %>% summarise(received = sum(.data[[question]]))
  sent     <- class_dyads %>% group_by(name = from) %>% summarise(sent = sum(.data[[question]]))
  nodes <- nodes %>% left_join(received, by = "name") %>% left_join(sent, by = "name")
  isolates <- sum(nodes$received == 0 & nodes$sent == 0)
  ties <- sum(class_dyads[[question]])

  network <- graph_from_data_frame(links, vertices = nodes)
  arrow_head <- function(ends) arrow(length = unit(1.4, "mm"), type = "closed", ends = ends)

  ggraph(network, layout = "manual", x = V(network)$x, y = V(network)$y) +
    geom_edge_arc(aes(filter = !mutual), strength = 0.1, color = "grey50",
                  arrow = arrow_head("last"), end_cap = circle(2.2, "mm"), width = 0.35) +
    geom_edge_arc(aes(filter = mutual), strength = 0.25, color = "grey50",
                  arrow = arrow_head("both"), start_cap = circle(2.2, "mm"),
                  end_cap = circle(2.2, "mm"), width = 0.35) +
    geom_node_point(aes(size = received, fill = sex), shape = 21, color = "grey20", stroke = 0.3) +
    scale_fill_manual(values = c(Female = color_female, Male = color_male), name = NULL) +
    scale_size(range = c(1, 6), limits = c(0, max_size), name = "Nominations received",
               breaks = pretty(c(0, max_size), n = 4)) +
    labs(title = paste0(title, "\n", ties, " ties, ", isolates, " isolates")) +
    theme_void(base_size = 8) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 8, color = color_text))
}

plot_focal_layers <- function(dyads, nodes, this_class) {
  # one size scale for the three panels: the largest number received
  max_received <- dyads %>% filter(class_id == this_class) %>%
    group_by(receiver_id) %>%
    summarise(across(c(nom_q3, nom_q4, nom_q5), sum)) %>%
    select(-receiver_id) %>% max()
  map2(names(focal), focal, ~ draw_focal_layer(dyads, this_class, .x, nodes, .y, max_received)) %>%
    wrap_plots(nrow = 1, guides = "collect") &
    theme(legend.position = "bottom") &
    guides(size = guide_legend(nrow = 1), fill = guide_legend(override.aes = list(size = 3)))
}
