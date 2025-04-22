# Packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(showtext)
library(nrBrand)
library(ggtext)
library(lemon)
library(ggrepel)


# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Data wrangling ----------------------------------------------------------

plot_data <- income |>
  dplyr::filter(stringr::str_detect(Country, "(WID)")) |>
  dplyr::rename(Income = `Income share of the richest 1% (before tax) (World Inequality Database)`) |>
  dplyr::mutate(Country = stringr::str_remove(Country, " \\(WID\\)")) |>
  dplyr::mutate(Country = dplyr::if_else(
    Country == "MENA", "Middle East and North Africa", Country
  )) |>
  dplyr::filter(Year == 2023) |>
  dplyr::filter(Country %in% c("North America", "Europe", "Latin America", "Middle East and North Africa", "Oceania"))

star_data <- plot_data |>
  select(-Year) |>
  mutate(Compare = 1) |>
  tidyr::pivot_longer(
    -Country
  ) |>
  mutate(
    theta = seq(
      from = 0,
      to = 2 * pi,
      length.out = nrow(plot_data) * 2
    ),
    x = value * cos(theta),
    y = value * sin(theta)
  )

# Colours -----------------------------------------------------------------

bg_col <- "#151D28"
text_col <- "#FDFDEC"


# Fonts -------------------------------------------------------------------

font_add_google("Sriracha")
showtext_auto()
showtext_opts(dpi = 300)

body_font <- "Sriracha"


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "The share of income received by the richest 1% of the population in 2023."
st <- "Income is measured before payment of taxes and non-pension benefits but after the payment of public and private pensions."
cap <- paste0(
  "**Data**: World Inequality Database (WID). Processed by Our World in Data<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = star_data,
  mapping = aes(x = x, y = y)
) +
  geom_path(
    data = rbind(
      star_data,
      star_data[1,]
    ),
    colour = text_col, linewidth = 0.3) +
  geom_point(
    mapping = aes(pch = name),
    colour = text_col, size = 3
  ) +
  geom_text_repel(
    data = filter(star_data, name == "Income"),
    mapping = aes(
      x = x, y = y,
      label = paste0(Country, "\n(", round(value, 1), "%)")
    ),
    colour = text_col,
    family = body_font,
    size = 2.5,
    seed = 22
  ) +
  labs(title = title, subtitle = st, caption = cap) +
  scale_x_symmetric() +
  scale_y_symmetric() +
  scale_shape_manual(values = c(20, 8)) +
  coord_fixed() +
  theme_void(base_family = body_font, base_size = 9) +
  theme(
    plot.margin = margin(10, 10, 5, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none",
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      family = body_font,
      margin = margin(b = 10)
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      family = body_font,
      face = "bold",
      size = rel(1.6),
      margin = margin(b = 5)
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      lineheight = 0.5,
      family = body_font,
      margin = margin(t = 15)
    )
  )


# Save --------------------------------------------------------------------

ggsave("2025/viz/day_22.png", height = 5, width = 5, bg = bg_col)

