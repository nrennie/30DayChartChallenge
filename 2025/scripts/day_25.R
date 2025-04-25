# Packages ----------------------------------------------------------------

library(ggplot2)
library(showtext)
library(nrBrand)
library(ggtext)
library(geofacet)
library(dplyr)
library(tidyr)


# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Colours -----------------------------------------------------------------

bg_col <- "#E5E5E5"
bg_col2 <- "#bbbbbb"
text_col <- "#14213D"
missing_col <- "grey30"


# Fonts -------------------------------------------------------------------

font_add_google("Libre Franklin", "libre")
font_add_google("Domine", "domine")
showtext_auto()
showtext_opts(dpi = 300)

body_font <- "libre"
title_font <- "domine"


# Data wrangling ----------------------------------------------------------

income_data <- income |>
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
    non_Income = 100 - Income,
    missing_perc = 100 - (Income + non_Income)
  ) |>
  select(name, Income, non_Income, missing_perc) |>
  pivot_longer(-name, names_to = "income", values_to = "perc")

plot_data <- world_countries_grid1 |>
  left_join(income_data, by = "name") |>
  as_tibble() |>
  select(code_alpha3, income, perc) |>
  filter(
    !((income == "Income" | income == "non_Income") & is.na(perc))
  ) |>
  mutate(
    perc = replace_na(perc, 100)
  )


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "Share of income received by the richest 1%."
st <- glue::glue("Income is measured before payment of taxes and non-pension benefits but after the payment of public and private pensions. Higher values indicate more unequal income distribution. This chart shows the <span style='color:{text_col}'>**share of income received by the richest 1% of the population**</span> for each country in 2023.")
st <- paste0(
  st, "<br><br>**Data**: World Inequality Database. Processed by Our World in Data<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data
) +
  geom_col(
    mapping = aes(x = 1, y = perc, fill = income),
    show.legend = FALSE
  ) +
  #facet_wrap(~code_alpha3) +
  facet_geo(~code_alpha3, grid = "world_countries_grid1", label = "code_alpha3") +
  labs(title = title, subtitle = st) +
  scale_fill_manual(
    values = c(
      "Income" = text_col, "non_Income" = bg_col2,
      "missing_perc" = missing_col
    )
  ) +
  coord_polar(theta = "y", direction = -1) +
  theme_void(base_family = body_font, base_size = 9.5) +
  theme(
    strip.text = element_blank(),
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(0, 5, 0, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.spacing = unit(0, "lines"),
    # Text
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      size = rel(1.4),
      face = "bold",
      family = title_font,
      maxwidth = 1
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 50, t = 5),
      family = body_font,
      maxwidth = 1
    )
  )


# Save --------------------------------------------------------------------

ggsave("2025/viz/day_25.png", height = 6, width = 5, bg = bg_col)

