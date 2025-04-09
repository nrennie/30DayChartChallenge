# Packages ----------------------------------------------------------------

library(ggplot2)
library(ggtext)
library(showtext)
library(nrBrand)
library(ggridges)
library(PrettyCols)


# Read in data ------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu", "ubuntu")
showtext_auto()
showtext_opts(dpi = 300)


# Parameters --------------------------------------------------------------

highlight_col <- "#B53737"
text_col <- "#00344A"
bg_col <- "#E2EDF4"

body_font <- "ubuntu"
title_font <- "ubuntu"


# Data wrangling ----------------------------------------------------------

plot_data <- income |>
  dplyr::filter(Year >= 1980) |>
  dplyr::filter(
    stringr::str_detect(Country, "(WID)", negate = TRUE),
    Country != "World"
  ) |>
  dplyr::mutate(Country = stringr::str_remove(Country, " \\(country\\)")) |>
  tidyr::drop_na() |>
  dplyr::rename(
    Income = `Income share of the richest 1% (before tax) (World Inequality Database)`
  )


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA,
  linkedin = NA,
  bluesky = NA
)
title <- "<span style='font-size:20pt;'>**More alike, but more unequal**</span><br><br>"
st <- "The share of income received by the richest 1% of the population where income is measured before payment of taxes and non-pension benefits but after the payment of public and private pensions. Countries are becoming more similar in their wealth inequality, but more unequal on average.<br><br>"
cap <- paste0(title, st,
  "**Data**: World Inequality Database (WID). Processed by Our World in Data<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = Income, y = as.factor(Year), fill = after_stat(x))
) +
  geom_density_ridges_gradient(linewidth = 0.2, colour = text_col) +
  scale_fill_gradient2(midpoint = mean(plot_data$Income), limits = c(0, 35),
                       low = "#00567A", high = highlight_col) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0, 35), expand = c(0, 0)) +
  labs(x = "% of income received by richest 1%", y = "",
       tag = cap) +
  theme_minimal(base_size = 8.5, base_family = body_font) +
  theme(
    plot.margin = margin(5, 10, 5, 225),
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag.position = c(-0.4, 0.5),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      lineheight = 1,
      family = body_font,
      maxwidth = 0.8
    ),
    panel.grid = element_blank(),
    text = element_text(colour = text_col)
  )


# Save --------------------------------------------------------------------

ggsave("2025/viz/day_10.png", height = 7, width = 7)
