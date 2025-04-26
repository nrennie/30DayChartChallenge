# Packages ----------------------------------------------------------------

library(ggplot2)
library(showtext)
library(nrBrand)
library(ggtext)
library(dplyr)
library(tidyr)
library(sf)
library(rnaturalearth)
library(ggpattern)


# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Colours -----------------------------------------------------------------

bg_col <- "grey90"
text_col <- "black"
highlight_col <- "#ffd600"


# Fonts -------------------------------------------------------------------

font_add_google("Georama")
showtext_auto()
showtext_opts(dpi = 300)

body_font <- "Georama"
title_font <- "Georama"


# Data wrangling ----------------------------------------------------------

plot_data <- income |>
  filter(Year == 2023) |>
  filter(
    stringr::str_detect(Country, "(WID)", negate = TRUE),
    Country != "World"
  ) |>
  mutate(Country = stringr::str_remove(Country, " \\(country\\)")) |>
  drop_na() |>
  rename(
    Income = `Income share of the richest 1% (before tax) (World Inequality Database)`,
    name = Country
  ) |>
  mutate(
    name = if_else(
      name == "United States", "United States of America", name
    )
  )

world <- ne_countries(scale = "medium", returnclass = "sf")

map_data <- world |>
  filter(name != "Antarctica") |>
  left_join(
    plot_data,
    by = "name"
  )


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- stringr::str_to_upper("Share of income received by the richest 1%")
st <- "Income is measured before payment of taxes and non-pension benefits but after the payment of public and private pensions. Higher values indicate more unequal income distribution. This chart shows the share of income received by the richest 1% of the population for each country in 2023, with darker colours indicating a higher percentage. Yellow stripes indicate regions where data is unavailable."
cap <- paste0(
  "**Data**: World Inequality Database. Processed by Our World in Data<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf_pattern(
    data = map_data,
    mapping = aes(
      fill = Income,
      pattern_density = as.numeric(is.na(Income))
    ),
    colour = text_col,
    pattern = "stripe",
    pattern_size = 0.5,
    pattern_spacing = 0.03
  ) +
  labs(title = title, subtitle = st, caption = cap) +
  scale_fill_gradientn(
    colours = PrettyCols::prettycols("Greys", direction = -1),
    na.value = highlight_col
  ) +
  theme_void(base_family = body_font, base_size = 9.5) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(5, 0, 5, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    # Text
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      size = rel(1.8),
      family = title_font,
      face = "bold",
      maxwidth = 1
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 5),
      family = body_font,
      maxwidth = 1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 15),
      family = body_font,
      maxwidth = 1
    )
  )


# Save --------------------------------------------------------------------

ggsave("2025/viz/day_30.png", height = 4.5, width = 7, bg = bg_col)
