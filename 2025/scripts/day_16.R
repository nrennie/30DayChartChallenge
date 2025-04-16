# Packages ----------------------------------------------------------------

library(ggplot2)
library(showtext)
library(nrBrand)
library(ggtext)
library(waterfalls) # Forked from https://github.com/HughParsonage/waterfalls


# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Colours -----------------------------------------------------------------

bg_col <- "#FDEBC4"
text_col <- "#45200D"
highlight_col <- "#64B6AC"


# Data wrangling ----------------------------------------------------------

plot_data <- income |>
  dplyr::rename(Income = `Income share of the richest 1% (before tax) (World Inequality Database)`) |>
  dplyr::filter(
    Country == "South Korea",
    Year >= 1979,
    Year <= 2015
  ) |>
  dplyr::select(-Country) |>
  dplyr::mutate(Change = c(NA, diff(Income))) |>
  tidyr::drop_na() |>
  dplyr::mutate(
    Year = as.factor(Year)
  )


# Fonts -------------------------------------------------------------------

font_add_google("Poetsen One", "poetsen", db_cache = FALSE)
font_add_google("Nunito Sans", "nunito")
showtext_auto()
showtext_opts(dpi = 300)

body_font <- "nunito"
title_font <- "poetsen"


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "The share of income received by the richest 1% of the population in South Korea."
st <- glue::glue("In the early 1990s, the income distribution in South Korea became <span style='color:{highlight_col};'>**more equal**</span>. However since the late 1990s, the overall trend is that income distribution is becoming <span style='color:#BA5624;'>**less equal**</span>, reaching a record high in 2010.")
cap <- paste0(
  "**Data**: World Inequality Database (WID). Processed by Our World in Data<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

waterfall(
  values = round(plot_data$Change, 2),
  labels = plot_data$Year,
  draw_axis.x = "none",
  rect_text_size = 0.6,
  put_rect_text_outside_when_value_below = 50,
  fill_colours = c(highlight_col, "#BA5624"),
  rect_border = text_col
) +
  scale_y_continuous(
    breaks = seq(-2 - 0.83, 14, 5),
    labels = seq(-2 - 0.83, 14, 5) + plot_data$Income[1],
    limits = c(-2 - 0.83, 14 - 0.83)
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = "",
    y = "Percentage of income received\nby richest 1%."
  ) +
  theme_minimal(base_size = 9, base_family = body_font) +
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
    axis.text.x = element_text(
      size = rel(0.8)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = alpha(text_col, 0.3), linewidth = 0.3
    ),
    plot.margin = margin(5, 10, 5, 10)
  )


# Save --------------------------------------------------------------------

ggsave("2025/viz/day_16.png", height = 4, width = 8, bg = bg_col)

