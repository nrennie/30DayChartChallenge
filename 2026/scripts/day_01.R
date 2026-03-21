# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggforce)


# Load data ---------------------------------------------------------------

absolute_judgements <- read_csv("2026/data/absolute_judgements.csv")
respondent_metadata <- read_csv("2026/data/respondent_metadata.csv")
pairwise_comparisons <- read_csv("2026/data/pairwise_comparisons.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#7F055F"


# Data wrangling ----------------------------------------------------------

# % of people who think X is more likely than *likely*
plot_data <- pairwise_comparisons |>
  filter(term1 == "Likely" | term2 == "Likely") |>
  mutate(
    selected = if_else(selected == "Likely", selected, "Other"),
    term = case_when(
      term1 == "Likely" ~ term2,
      TRUE ~ term1
    )
  ) |>
  select(term, selected) |>
  count(term, selected) |>
  group_by(term) |>
  mutate(
    n_total = sum(n),
    p = 100 * n / n_total
  ) |>
  filter(selected == "Other") |>
  ungroup() |>
  arrange(desc(p)) |>
  mutate(term = factor(term, levels = term)) |>
  mutate(end = (p / 100) * (2 * pi))


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "How likely is 'likely'?"
st <- "Percentage of respondents who believe each term is *more likely than likely* in a pairwise comparison."
cap <- source_caption(source = "Kucharski AJ (2026) CAPphrase: Comparative and Absolute Probability phrase dataset. DOI: 10.5281/zenodo.18750055", graphic = social)


# Plot --------------------------------------------------------------------

ggplot(data = plot_data) +
  geom_arc_bar(
    mapping = aes(x0 = 0, y0 = 0, r0 = 2, r = 4, start = 0, end = 2 * pi),
    fill = "grey70",
    colour = NA
  ) +
  geom_arc_bar(
    mapping = aes(x0 = 0, y0 = 0, r0 = 2, r = 4, start = 0, end = end),
    fill = highlight_col,
    colour = NA
  ) +
  geom_text(
    mapping = aes(x = 0, y = 0, label = paste0(round(p, 1), "%"))
  ) +
  facet_wrap(~term, nrow = 3) +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  coord_fixed() +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.6)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10),
      hjust = 0.5,
      halign = 0.5,
      size = rel(0.8)
    ),
    panel.grid.minor = element_blank()
  ) +
  canvas(
    width = 7, height = 5.25,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = "2026/viz/day_01.png"
)
