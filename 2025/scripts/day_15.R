# Packages ----------------------------------------------------------------

library(showtext)
library(nrBrand)
library(tidyplots)
library(ggplot2)
library(ggtext)


# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Colours -----------------------------------------------------------------

bg_col <- "grey97"
text_col <- "black"
highlight_col <- "#88201B"


# Data wrangling ----------------------------------------------------------

plot_data <- income |>
  dplyr::filter(
    Country == "World",
    Year >= 1980
  ) |>
  dplyr::rename(
    Income = `Income share of the richest 1% (before tax) (World Inequality Database)`
  )


# Fonts -------------------------------------------------------------------

font_add_google("Open Sans", "Open")
showtext_auto()
showtext_opts(dpi = 300)

body_font <- "Open"
title_font <- "Open"


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "The share of income received by the richest 1% of the population."
st <- "Income is measured before payment of taxes and non-pension benefits but after the payment of public and private pensions."
cap <- paste0(
  "**Data**: World Inequality Database (WID). Processed by Our World in Data<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

plot_data |>
  # Main plot
  tidyplot(x = Year, y = Income) |>
  add_area(colour = highlight_col, fill = highlight_col, alpha = 0.9) |>
  # Axes
  adjust_x_axis(padding = c(0, 0)) |>
  remove_x_axis_title() |>
  adjust_y_axis(limits = c(0, 100)) |>
  adjust_y_axis_title("Percentage of income received by richest 1% of population.") |>
  # Text
  add_title(title = title) |>
  add(ggplot2::labs(subtitle = st)) |>
  add_caption(caption = cap) |>
  adjust_font(family = body_font, color = text_col, fontsize = 9) |>
  # Theme
  adjust_theme_details(
    # Background
    panel.background = element_rect(fill = "#DD5B55", colour = "#DD5B55"),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    # Text
    plot.title = element_textbox_simple(
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_textbox_simple(margin = margin(b = 10)),
    plot.caption = element_textbox_simple(margin = margin(t = 10)),
    # Layout
    plot.margin = margin(10, 10, 10, 10)
  ) |>
  adjust_size(NA, NA) |>
  save_plot("2025/viz/day_15.png", width = 5, height = 5, units = "in",
            dpi = 300, bg = bg_col)

record_polaroid()
