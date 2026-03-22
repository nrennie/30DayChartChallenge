# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggdist)


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
highlight_col2 <- "#197176"


# Data wrangling ----------------------------------------------------------

plot_data <- absolute_judgements |>
  filter(term %in% c("May Happen", "Might Happen")) |>
  select(-order)

diff_data <- plot_data |>
  pivot_wider(names_from = term, values_from = probability) |>
  mutate(diff = `May Happen` - `Might Happen`)

big_pos <- diff_data |>
  slice_max(diff) |>
  pivot_longer(-response_id, names_to = "term", values_to = "probability")

big_neg <- diff_data |>
  slice_min(diff) |>
  pivot_longer(-response_id, names_to = "term", values_to = "probability")

big_pos_text <- big_pos |>
  pivot_wider(names_from = term, values_from = probability) |>
  left_join(respondent_metadata)

big_neg_text <- big_neg |>
  pivot_wider(names_from = term, values_from = probability) |>
  left_join(respondent_metadata)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Might happen? May happen?"
st <- "In an online quiz, participants assigned numerical values (0–100%) to each of 19 probabilistic phrases. The phrases *might happen* and *may happen* received very similar repsonses overall, but not everyone agreed!"
cap <- source_caption(source = "Kucharski AJ (2026) CAPphrase: Comparative and Absolute Probability phrase dataset. DOI: 10.5281/zenodo.18750055", graphic = social)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = term, y = probability, group = response_id)
) +
  geom_line(colour = alpha("grey70", 0.5), linewidth = 0.4) +
  stat_slab(
    data = filter(plot_data, term == "May Happen"),
    mapping = aes(x = term, y = probability),
    side = "left",
    inherit.aes = FALSE
  ) +
  stat_slab(
    data = filter(plot_data, term == "Might Happen"),
    mapping = aes(x = term, y = probability),
    side = "right",
    inherit.aes = FALSE
  ) +
  geom_line(
    data = filter(big_neg, term != "diff"),
    colour = highlight_col, linewidth = 1.2
  ) +
  geom_line(
    data = filter(big_pos, term != "diff"),
    colour = highlight_col2, linewidth = 1.2
  ) +
  geom_textbox(
    data = big_neg_text,
    mapping = aes(
      x = "Might Happen", y = `Might Happen`,
      label = glue("One person rated *might happen* as <span style='color:{highlight_col}'>**{abs(diff)}pp higher**</span> than *may happen*.")
    ),
    hjust = 0,
    halign = 0,
    fill = alpha(bg_col, 0.35),
    box.colour = "transparent"
  ) +
  geom_textbox(
    data = big_pos_text,
    mapping = aes(
      x = "Might Happen", y = `Might Happen`,
      label = glue("One person rated *might happen* as <span style='color:{highlight_col2}'>**{abs(diff)}pp lower**</span> than *may happen*.")
    ),
    hjust = 0,
    halign = 0,
    fill = alpha(bg_col, 0.35),
    box.colour = "transparent"
  ) +
  geom_textbox(
    data = big_pos_text,
    mapping = aes(
      x = "May Happen", y = 0,
      label = "↓ Less likely"
    ),
    box.margin = unit(c(0, 70, 0, 0), "pt"),
    hjust = 1,
    halign = 1,
    vjust = 0,
    valign = 0,
    fill = alpha(bg_col, 0.35),
    box.colour = "transparent"
  ) +
  geom_textbox(
    data = big_pos_text,
    mapping = aes(
      x = "May Happen", y = 100,
      label = "↑ More likely"
    ),
    box.margin = unit(c(0, 70, 0, 0), "pt"),
    hjust = 1,
    halign = 1,
    vjust = 1,
    valign = 1,
    fill = alpha(bg_col, 0.35),
    box.colour = "transparent"
  ) +
  geom_point(
    data = filter(big_pos, term != "diff"),
    colour = highlight_col2, size = 2
  ) +
  geom_point(
    data = filter(big_neg, term != "diff"),
    colour = highlight_col, size = 2
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = NULL,
    y = "Probability (%)"
  ) +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
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
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 5),
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
      size = rel(0.9)
    ),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(angle = 0, hjust = 1,
                                vjust = 1.02,
                               margin = margin(r = -68))
  ) +
  canvas(
    width = 7, height = 5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = "2026/viz/day_04.png"
)
