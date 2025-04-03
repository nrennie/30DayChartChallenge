# Packages ----------------------------------------------------------------

library(ggplot2)
library(showtext)
library(nrBrand)
library(ggtext)
library(geomtextpath)
library(ggh4x)


# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Data wrangling ----------------------------------------------------------

plot_data <- income |>
  dplyr::filter(stringr::str_detect(Country, "(WID)")) |>
  dplyr::rename(Income = `Income share of the richest 1% (before tax) (World Inequality Database)`) |>
  dplyr::filter(Year %in% c(1820, 2020)) |>
  dplyr::mutate(Country = stringr::str_remove(Country, " \\(WID\\)")) |>
  dplyr::group_by(Country) |>
  dplyr::mutate(n = dplyr::n()) |>
  dplyr::filter(n == 2) |>
  dplyr::ungroup() |>
  dplyr::mutate(Country = dplyr::if_else(
    Country == "MENA", "Middle East and North Africa", Country
  )) |>
  dplyr::select(-n)


# Colours -----------------------------------------------------------------

bg_col <- "#f2e7e3"
text_col <- "#3d3835"


# Fonts -------------------------------------------------------------------

font_add_google("Yesteryear", "Yesteryear")
showtext_auto()
showtext_opts(dpi = 300)

body_font <- "Yesteryear"


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "The share of income received by the richest 1% of the population."
st <- "`Income is measured before payment of taxes and non-pension benefits but after the payment of public and private pensions. Higher values indicate more unequal wealth distribution, meaning that, after 200 years, Oceania has remained the most equal region in terms of wealth distribution."
cap <- paste0(
  "**Data**: World Inequality Database (WID). Processed by Our World in Data<br>**Graphic**: ", social
)


# Layout ------------------------------------------------------------------

design <- "
  ABB
  ABB
  #BB
  #BB
"

# Plot --------------------------------------------------------------------

ggplot(data = plot_data) +
  geom_col(
    mapping = aes(
      x = Country, y = Income
    ),
    fill = "#becdd2",
    linewidth = 0.05,
    colour = text_col,
    width = 1
  ) +
  geom_col(
    mapping = aes(
      x = Country, y = 1
    ),
    fill = "#a29791",
    linewidth = 0.1,
    colour = text_col,
    width = 1
  ) +
  geom_textpath(
    mapping = aes(
      x = Country, y = Income + 1,
      label = stringr::str_wrap(Country, 12)
    ),
    colour = text_col,
    family = body_font,
    size = 2.5,
    lineheight = 0.8,
    vjust = 1
  ) +
  facet_manual(vars(Year), design = design) +
  labs(title = stringr::str_to_upper(title), tag = st, caption = cap) +
  coord_curvedpolar(clip = "off") +
  theme_void(base_family = body_font, base_size = 11) +
  theme(
    plot.margin = margin(10, 10, 5, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    strip.placement = "inside",
    plot.tag.position = c(0.2, 0.25),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      lineheight = 0.5,
      family = body_font,
      maxwidth = 0.6,
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      lineheight = 0.5,
      family = body_font,
      margin = margin(b = 10)
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

ggsave("2025/viz/day_06.png", height = 5.5, width = 7, bg = bg_col)
