# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

# Download from: https://ourworldindata.org/natural-disasters
disasters <- readr::read_csv("2024/data/share-deaths-from-natural-disasters.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()


# Data wrangling ----------------------------------------------------------

plot_data <- disasters |>
  filter(Entity %in% c(
    "World Bank High Income",
    "World Bank Low Income",
    "World Bank Lower Middle Income",
    "World Bank Upper Middle Income"
  )) |>
  mutate(Entity = factor(Entity, levels = c(
    "World Bank Low Income",
    "World Bank Lower Middle Income",
    "World Bank Upper Middle Income",
    "World Bank High Income"
  ))) |>
  rename(Perc = `Deaths - Exposure to forces of nature - Sex: Both - Age: All Ages (Percent)`) |>
  filter(Perc > 0)


# Define colours and fonts-------------------------------------------------

bg_col <- "grey95"
text_col <- "black"

body_font <- "roboto"
title_font <- "roboto_slab"


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = "@nrennie",
)
title <- "Share of deaths from natural disasters, 1990 to 2019"
st <- '"While natural disasters account for a small fraction of all deaths
globally, they can have a large impact, especially on vulnerable populations in
low-to-middle-income countries with insufficient infrastructure to protect
and respond effectively."'
cap <- paste0(
  "**Data**: Our World in Data<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(
    x = Year,
    y = Entity,
    size = Perc
  )
) +
  geom_point(
    pch = 21,
    fill = alpha(text_col, 0.3)
  ) +
  labs(
    x = "", y = "",
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_size_area(
    max_size = 10,
    guide = guide_legend(
      nrow = 1,
      title = "Percentage of deaths",
      theme(legend.title.position = "left")
    )
  ) +
  scale_y_discrete(labels = scales::label_wrap(16)) +
  theme_minimal(base_size = 25, base_family = body_font) +
  theme(
    plot.margin = margin(5, 10, 5, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.3,
      family = title_font,
      face = "bold",
      size = rel(1.6)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    axis.text.y = element_text(
      family = body_font,
      lineheight = 0.5,
      margin = margin(r = -5)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.5,
                                      colour = alpha(text_col, 0.1)),
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "inside",
    legend.position.inside = c(0.7, -0.2)
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "viz", "gifs", paste0("day_07", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)

unlink("2024/recording/", recursive = TRUE)
