library(tidyverse)
library(igraph)
library(showtext)
library(rcartocolor)

# add fonts
font_add_google(name = "Henny Penny", family = "henny")
showtext_auto()

goldilocks <- tibble(from = c("Goldilocks",
                              "Porridge", "Porridge", "Porridge",
                              "Just right",
                              "Chairs", "Chairs", "Chairs",
                              "Just right2",
                              "Beds", "Beds", "Beds",
                              "Just right3"),
                     to = c("Porridge",
                            "Too cold", "Too hot", "Just right",
                            "Chairs",
                            "Still too big", "Too big", "Just right2",
                            "Beds",
                            "Too soft", "Too hard", "Just right3",
                            "Bears!"))

g = graph_from_data_frame(goldilocks, directed = TRUE)
coords = layout_as_tree(g)
colnames(coords) = c("x", "y")

output_df = as_tibble(coords) %>%
  mutate(step = vertex_attr(g, "name"),
         x = x*-1,
         type = factor(c(1, 2, 3, 2, 3, 2, 3, 3, 3, 3, 3, 3, 3, 1)),
         label = gsub("\\d+$", "", step))

plot_nodes = output_df %>%
  mutate(xmin = x - 0.35,
         xmax = x + 0.35,
         ymin = y - 0.25,
         ymax = y + 0.25)

plot_edges = goldilocks %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c("from", "to"),
               names_to = "s_e",
               values_to = "step") %>%
  left_join(plot_nodes, by = "step") %>%
  select(-c(label, type, y, xmin, xmax)) %>%
  mutate(y = ifelse(s_e == "from", ymin, ymax)) %>%
  select(-c(ymin, ymax))

ggplot() +
  geom_rect(data = plot_nodes,
            mapping = aes(xmin = xmin,
                          ymin = ymin,
                          xmax = xmax,
                          ymax = ymax,
                          fill = type,
                          colour = type),
            alpha = 0.5,
            linejoin = "round") +
  geom_text(data = plot_nodes,
            mapping = aes(x = x,
                          y = y,
                          label = label),
            family = "henny",
            color = "#585c45") +
  geom_path(data = plot_edges,
            mapping = aes(x = x,
                          y = y,
                          group = id),
            colour = "#585c45",
            arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  scale_fill_carto_d(palette = "Antique") +
  scale_colour_carto_d(palette = "Antique") +
  labs(title = "The Goldilocks Decision Tree",
       caption = "N. Rennie\n\nData: Robert Southey. Goldilocks and the Three Bears. 1837.\n\nImage: New York Public Library\n\n#30DayChartChallenge") +
  theme_void() +
  theme(plot.margin = unit(c(1, 1, 0.5, 1), "cm"),
        legend.position = "none",
        plot.background = element_rect(colour = "#f2e4c1", fill = "#f2e4c1"),
        panel.background = element_rect(colour = "#f2e4c1", fill = "#f2e4c1"),
        plot.title = element_text(family = "henny", hjust = 0, face = "bold",
                                  size = 40, color = "#585c45",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.caption = element_text(family = "henny", hjust = 0,
                                    size = 10, color = "#585c45",
                                    margin = margin(t = 10)))




# image added with Inkscape
# https://digitalcollections.nypl.org/items/69b7beff-1648-5768-e040-e00a18061dc2
















