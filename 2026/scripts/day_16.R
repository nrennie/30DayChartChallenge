
# Packages ----------------------------------------------------------------

library(tidyverse)
library(nzbabynames)
library(showtext)
library(ggtext)
library(ggview)


# Data --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-02-17")
sheep <- tuesdata$dataset


# Wrangling ---------------------------------------------------------------

sheep_data <- sheep |>
  filter(measure %in% c("Total Sheep")) |>
  select(year_ended_june, measure, value) |>
  mutate(sheep = value / 1000000) |>
  select(year = year_ended_june, sheep) |>
  drop_na() |>
  filter(year >= 1950, year <= 1995)

neil_data_nz <- nzbabynames |>
  filter(Name == "Neil",
         Year >= min(sheep_data$year),
         Year <= max(sheep_data$year)) |>
  filter(Year >= 1950, Year <= 1995) |>
  group_by(Year) |>
  summarise(neils = sum(Count))

# Correlation
k <- sheep_data |>
  left_join(neil_data_nz, by = c("year" = "Year")) |>
  drop_na()
cor(k$sheep, k$neils)


# Colours -----------------------------------------------------------------

bg_col <- "white"
text_col <- "black"
highlight_col <- "#ba0f27"


# Fonts -------------------------------------------------------------------

font_add_google("Open Sans", "open")
showtext_auto()
showtext_opts(dpi = 300)
body_font <- "open"


# Text --------------------------------------------------------------------

title <- glue::glue(
  "<span style='color:{text_col}; font-size:20pt;'>**Number of sheep**</span><br>negatively correlates with<span style='color:{highlight_col}; font-size:20pt;'>**<br>Popularity of the name 'Neil'**</span>"
)


# Plot --------------------------------------------------------------------

# In the style of https://www.tylervigen.com/spurious-correlations
coeff <- 3
ggplot() +
  # Sheep
  geom_line(
    data = sheep_data,
    mapping = aes(x = year, y = sheep, colour = "Sheep"),
    linetype = "dotted",
    linewidth = 1
  ) +
  geom_point(
    data = sheep_data,
    mapping = aes(x = year, y = sheep, colour = "Sheep"),
    pch = 18,
    size = 3
  ) +
  # Neil
  geom_line(
    data = neil_data_nz,
    mapping = aes(x = Year, y = neils / coeff, colour = "Neil")
  ) +
  geom_point(
    data = neil_data_nz,
    mapping = aes(x = Year, y = neils / coeff, colour = "Neil")
  ) +
  # Styling
  labs(x = NULL, title = title) +
  scale_colour_manual(
    values = c(highlight_col, text_col),
    labels = c("Number of babies born in New Zealand named Neil. Source: data.govt.nz.", "Total number of sheep in New Zealand (millions). Source: StatsNZ.")
  ) +
  scale_y_continuous(
    name = "Total number of sheep in New Zealand (millions).",
    limits = c(0, 85),
    sec.axis = sec_axis(~ . * coeff,
                        name = "Number of babies in New Zealand named Neil."
    )
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_family = body_font, base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(colour = "grey30"),
    axis.title.y = element_text(color = text_col),
    axis.title.y.right = element_text(
      color = highlight_col,
      margin = margin(l = 10)
    ),
    axis.title.y.left = element_text(
      color = text_col,
      margin = margin(r = 10)
    ),
    axis.text.y.right = element_text(color = highlight_col),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      lineheight = 1,
      margin = margin(b = 10)
    ),
    axis.line.x.bottom = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_line(),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(10, 10, 5, 10)
  ) +
  canvas(
    width = 7, height = 5.5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = "2026/viz/day_16.png"
)
