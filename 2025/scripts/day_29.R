# Packages ----------------------------------------------------------------

library(ggplot2)
library(showtext)
library(nrBrand)
library(ggtext)
library(geomtextpath)
library(ggh4x)
library(scales)


# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Data wrangling ----------------------------------------------------------

plot_data <- income |>
  dplyr::filter(
    stringr::str_detect(Country, "(WID)", negate = TRUE),
    Country != "World"
  ) |>
  dplyr::mutate(Country = stringr::str_remove(Country, " \\(country\\)")) |>
  tidyr::drop_na() |>
  dplyr::rename(
    Income = `Income share of the richest 1% (before tax) (World Inequality Database)`
  )

sd_data <- plot_data |>
  dplyr::group_by(Country) |>
  dplyr::summarise(sd = sd(Income)) |>
  dplyr::filter(sd > 0)


# Colours -----------------------------------------------------------------

bg_col <- "#F58A07"
text_col <- "#073D74"


# Fonts -------------------------------------------------------------------

font_add_google("Covered By Your Grace", "Grace")
font_add_google("Kablammo", db_cache = FALSE)
showtext_auto()
showtext_opts(dpi = 300)

body_font <- "Grace"
title_font <- "Kablammo"


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "This is a bad chart..."
st <- "...designed in the style of an extraterrestial who had never heard of good data visualisation principles. How many chart crimes can you spot?"
cap <- paste0(
  "**Data**: World Inequality Database (WID). Processed by Our World in Data<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

coeff <- 30
ggplot() +
  geom_point(
    data = plot_data,
    mapping = aes(
      x = Country, y = Income,
      colour = as.factor(Year),
    ),
    alpha = 0.2,
    size = 4
  ) +
  geom_line(
    data = sd_data,
    mapping = aes(x = Country, y = sd / coeff, group = "1"),
    linewidth = 1,
    colour = "#509ff4"
  ) +
  geom_point(
    data = sd_data,
    mapping = aes(x = Country, y = sd / coeff),
    pch = 25,
    size = 5,
    fill = "white",
    alpha = 0.5,
    colour = bg_col
  ) +
  scale_y_log10(
    limits = c(0.001, 100),
    breaks = c(0.1, 1, 10, 100),
    labels = c("0.1%", "1%", "10%", "100%"),
    sec.axis = sec_axis(~ . * coeff,
      name = "Income (Standard Deviation)",
      labels = label_comma()
    )
  ) +
  labs(title = title, subtitle = st, caption = cap, x = "") +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_grey(base_family = body_font, base_size = 13) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = "#DA7B07", colour = "#DA7B07"),
    panel.grid = element_line(colour = text_col),
    axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.3)),
    text = element_text(colour = text_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 20, t = 5),
      family = body_font,
      maxwidth = 0.8
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 0),
      family = body_font
    ),
    plot.margin = margin(5, 10, 5, 10)
  )





# Save --------------------------------------------------------------------

ggsave("2025/viz/day_29.png", height = 7, width = 7, bg = bg_col)
