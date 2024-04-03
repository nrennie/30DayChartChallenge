# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(scales)


# Load data ---------------------------------------------------------------

raw_bechdel <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv")
movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu", "ubuntu")
font_add_google("Righteous", "righteous")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "grey15"
text_col <- "white"
highlight_col <- "#EF476F"

body_font <- "ubuntu"
title_font <- "righteous"


# Data wrangling ----------------------------------------------------------

plot_data <- movies |>
  select(year, imdb_rating, binary) |>
  drop_na() |>
  mutate(
    decade = cut(year, breaks = seq(1970, 2011, by = 5)),
    decade = as.character(decade),
    decade = str_extract(decade, "^(.+?),"),
    decade = parse_number(decade),
    decade = as.character(replace_na(decade, 2010))
  ) |>
  mutate(binary = case_when(
    binary == "PASS" ~ 1,
    binary == "FAIL" ~ 0
  )) |>
  mutate(
    imdb_rating = floor(imdb_rating),
    imdb_rating = paste0(imdb_rating, "-", imdb_rating + 1)
  ) |>
  group_by(decade, imdb_rating) |>
  summarise(
    n = n(),
    pass = sum(binary)
  ) |>
  mutate(perc_pass = 100 * pass / n) |>
  ungroup() |>
  complete(
    decade, imdb_rating
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "recording"),
  device = "png",
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = "@nrennie",
  linkedin = NA
)
title <- "The Bechdel Test"
st <- "For a film to pass the Bechdel test, it must:<br>(i) contain at least two
named women,<br>(ii) have a conversation between those two women at some point,
and<br>(iii) that conversation isn’t about a male character.<br><br>The following
heatmap shows the relationship between IMDb ratings of movies, and how many of them
pass the Bechdel test. Films that pass the Bechdel test are generally rated lower,
but this relationship is lessening over time.<br><br>"
cap <- paste0(
  st,
  "**Data**: FiveThirtyEight<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(plot_data) +
  geom_tile(
    mapping = aes(x = decade, y = imdb_rating, fill = perc_pass),
    colour = text_col,
    linewidth = 0.7
  ) +
  labs(
    title = title, tag = cap,
    x = "", y = "IMDb Rating\n\n"
  ) +
  scale_fill_viridis_c(
    limits = c(0, 100),
    na.value = bg_col,
    option = "C",
    name = "Percentage of movies passing the Bechdel test."
  ) +
  guides(fill = guide_colourbar(title.position = "top")) +
  theme_minimal(base_size = 27, base_family = body_font) +
  theme(
    plot.margin = margin(5, 10, 5, 240),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = -10, t = 10, l = -230),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(2.6)
    ),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 0),
      lineheight = 0.5,
      family = body_font,
      maxwidth = 0.85
    ),
    plot.tag.position = c(-0.7, 0.45),
    axis.text.x = element_text(
      colour = text_col,
      margin = margin(t = -5, b = -5),
      hjust = 1
    ),
    axis.text.y = element_text(
      colour = text_col,
      margin = margin(r = -5),
    ),
    axis.title.y = element_text(
      colour = text_col,
      angle = 0,
      hjust = 1,
      margin = margin(r = -12, b = 10),
      face = "bold"
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank(),
    legend.text = element_text(colour = text_col),
    legend.key.width = unit(1.9, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.position = "bottom",
    legend.title = element_text(
      hjust = 0.5,
      margin = margin(t = -35, b = 5),
      colour = text_col
    ),
    legend.ticks = element_line(
      colour = bg_col,
      linewidth = 0.5
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "viz", "gifs", paste0("day_03", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)

unlink("2024/recording/", recursive = TRUE)
