# Packages ----------------------------------------------------------------

library(ggplot2)
library(showtext)
library(nrBrand)
library(ggtext)


# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Colours -----------------------------------------------------------------

bg_col <- "grey97"
text_col <- "black"
highlight_col <- "#D81159"


# Data wrangling ----------------------------------------------------------

plot_data <- income |>
  dplyr::rename(Income = `Income share of the richest 1% (before tax) (World Inequality Database)`) |>
  dplyr::filter(
    stringr::str_detect(Country, "(WID)", negate = TRUE),
    Country != "World",
    Year >= 1980
  ) |>
  dplyr::mutate(Country = stringr::str_remove(Country, " \\(country\\)")) |>
  dplyr::mutate(
    Continent = countrycode::countrycode(
      sourcevar = Country,
      origin = "country.name",
      destination = "continent"
    )
  ) |>
  dplyr::filter(Continent == "Europe") |>
  dplyr::select(-Continent) |>
  dplyr::mutate(
    Colour = dplyr::case_when(
      Country %in% c("Norway", "Sweden", "Denmark") ~ "Yes",
      TRUE ~ Country
    ),
    Highlight = dplyr::case_when(
      Country %in% c("Norway", "Sweden", "Denmark") ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  tidyr::drop_na()


# Fonts -------------------------------------------------------------------

font_add_google("Open Sans", "Open")
showtext_auto()
showtext_opts(dpi = 300)

body_font <- "Open"
title_font <- "Open"


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "The share of income received by the richest 1% of the population."
st <- "In *The Clash of Civilizations*, Samuel Huntington describes **Kin-country syndrome** as the tendency for countries of a common civilizational background to support each other in international conflicts. The Scandinavian countries of **Norway, Sweden, and Denmark** share linguistic similarities, cultural traditions, and historical ties. Their cooperation is evident in regional organisations and joint initiatives. They also exhibit similar patterns in the share of income received by the richest 1% of the population, where income is measured before payment of taxes and non-pension benefits but after the payment of public and private pensions."
cap <- paste0(
  "**Data**: World Inequality Database (WID). Processed by Our World in Data<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_line(
    data = dplyr::filter(plot_data, !Highlight)[-1],
    mapping = aes(x = Year, y = Income, group = Colour),
    colour = "grey50",
    alpha = 0.5
  ) +
  geom_line(
    data = dplyr::filter(plot_data, Highlight),
    mapping = aes(x = Year, y = Income, group = Country),
    colour = highlight_col,
    linewidth = 1
  ) +
  facet_wrap(~Country) +
  scale_y_continuous(limits = c(0, 35)) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = "",
    y = "Percentage of income received\nby richest 1%."
  ) +
  coord_cartesian(expand = FALSE) +
  theme_bw(base_family = body_font, base_size = 10) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    text = element_text(colour = text_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.3)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 20, t = 5),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    plot.margin = margin(5, 10, 5, 10),
    panel.spacing = unit(0.8, "lines"),
    strip.text = element_text(
      face = "bold", hjust = 0
    ),
    strip.background = element_rect(fill = "transparent", colour = "transparent")
  )


# Save --------------------------------------------------------------------

ggsave("2025/viz/day_14.png", height = 5, width = 7, bg = bg_col)
