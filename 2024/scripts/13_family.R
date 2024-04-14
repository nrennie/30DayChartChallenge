library(tidyverse)
library(ggflowchart)
library(ggtext)
library(showtext)
library(glue)
library(nrBrand)
library(camcorder)

# read in data
lemurs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv")

# load fonts
font_add_google("Oswald")
font_add_google("Bangers")
showtext_auto()

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "recording"),
  device = "png",
  width = 7.5,
  height = 5.5,
  units = "in",
  dpi = 300
)

# choose species
sort(table(lemurs$taxon))
species <- "EFUL"

# lemur relationships
rel_data <- lemurs |>
  filter(
    taxon == species,
    hybrid == "N",
    sex %in% c("F", "M")
  ) |>
  select(name, dam_name, sire_name) |>
  distinct() |>
  pivot_longer(
    cols = dam_name:sire_name,
    values_to = "parent",
    names_to = "dam_sire"
  ) |>
  select(-dam_sire) |>
  drop_na() |>
  rename(
    from = parent,
    to = name
  ) |>
  select(from, to) |>
  mutate(across(c(from, to), ~ stringr::str_to_title(.x)))

# initial plot to choose a family
ggflowchart(rel_data)

# define family
parents <- c("Clarence", "Lyn")
children <- rel_data |>
  filter(from %in% parents) |>
  pull(to) |>
  unique()
grand_children <- rel_data |>
  filter(from %in% children) |>
  pull(to) |>
  unique()
great_grand_children <- rel_data |>
  filter(from %in% grand_children) |>
  pull(to) |>
  unique()

# filter
plot_data <- rel_data |>
  filter(
    (from %in% parents & to %in% children) |
      (from %in% children & to %in% grand_children) |
      (from %in% grand_children & to %in% great_grand_children)
  )

# gender
lemur_data <- lemurs |>
  filter(
    taxon == species,
    hybrid == "N",
    sex %in% c("F", "M")
  ) |>
  select(name, sex) |>
  mutate(name = stringr::str_to_title(name)) |>
  distinct() |>
  add_row(name = c("Lyn", "Clarence"), sex = c("F", "M"))

# layout
g <- igraph::graph_from_data_frame(
  select(plot_data, c(from, to)),
  directed = TRUE
)
coords <- igraph::layout_as_tree(g)
colnames(coords) <- c("x", "y")
node_data <- tibble::as_tibble(coords) %>%
  mutate(name = igraph::vertex_attr(g, "name")) |>
  left_join(lemur_data, by = "name") |>
  mutate(label = stringr::str_to_title(name)) |>
  unique()

# create flowchart
f <- ggflowchart(plot_data,
                 node_data = node_data,
                 layout = "custom",
                 fill = sex,
                 alpha = 0.6,
                 family = "Oswald",
                 text_size = 8,
                 x_nudge = 0.45,
                 y_nudge = 0.2,
                 arrow_size = 0.15,
                 arrow_linewidth = 0.3
)

# colours
green_col <- "#a1b70d"
grey_col <- "#8a8d8f"
bg_col <- "#fafafa"

# additional text
social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#a1b70d;'>&#xf099;</span><span style='color:#fafafa;'>.</span><span style='font-family:Oswald;color:#8a8d8f;'>@nrennie35</span><span style='color:#fafafa;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#a1b70d;'>&#xf4f6;</span><span style='color:#fafafa;'>.</span><span style='font-family:Oswald;color:#8a8d8f;'>fosstodon.org/@nrennie</span><span style='color:#fafafa;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#a1b70d;'>&#xf09b;</span><span style='color:#fafafa;'>.</span><span style='font-family:Oswald;color:#8a8d8f;'>nrennie</span><span style='color:#fafafa;'>..</span>"
st <- glue("The family tree of common brown lemurs Lyn and Clarence showing
<span style='color:{grey_col}'>male</span> and <span style='color:{green_col}'>female</span>
descendants.")

# styling
f +
  scale_fill_manual(values = c(green_col, grey_col)) +
  labs(title = "Lemurs at Duke Lemur Center",
       subtitle = st,
       caption = social) +
  theme(
    legend.position = "none",
    plot.caption = element_textbox_simple(
      lineheight = 0.5,
      colour = grey_col,
      margin = margin(b = 10, t = 10),
      maxwidth = 0.75,
      halign = 0.5,
      hjust = 0.5,
      size = 24
    ),
    plot.title = element_text(
      hjust = 0.5,
      family = "Bangers",
      colour = "black",
      size = 44
    ),
    plot.subtitle = element_textbox_simple(
      hjust = 0.5,
      halign = 0.5,
      family = "Oswald",
      colour = "#606264",
      margin = margin(t = 10),
      size = 30
    ),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col)
  )

# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "viz", "gifs", paste0("day_13", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)

unlink("2024/recording/", recursive = TRUE)
