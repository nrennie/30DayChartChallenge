# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)


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
  select(-order) |>
  pivot_wider(names_from = term, values_from = probability) |>
  mutate(Diff = `May Happen` - `Might Happen`,
         Diff_col = case_when(
           Diff < 0 ~ highlight_col2,
           Diff > 0 ~ highlight_col,
           TRUE ~ text_col
         ))

pol1 <- data.frame(
  x = c(0, 100, 100, 0),
  y = c(0, 0, 100, 0)
)
pol2 <- data.frame(
  x = c(0, 0, 100, 0),
  y = c(0, 100, 100, 0)
)

may_might <- round(100 * sum(plot_data$`May Happen` > plot_data$`Might Happen`) / nrow(plot_data))
may_might_eq <- round(100 * sum(plot_data$`May Happen` == plot_data$`Might Happen`) / nrow(plot_data))
might_may <- round(100 * sum(plot_data$`May Happen` < plot_data$`Might Happen`) / nrow(plot_data))
mm_cor <- round(cor(plot_data$`May Happen`, plot_data$`Might Happen`), 2)

annot1 <- glue("{may_might}% believe *may happen* means **more likely** than *might happen*.")
annot2 <- glue("{might_may}% believe *may happen* means **less likely** than *might happen*.")


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Might happen? May happen?"
st <- glue("In an online quiz, participants assigned numerical values (0–100%) to each of 19 probabilistic phrases. The phrases *might happen* and *may happen* received very similar repsonses overall, with {may_might_eq}% of respondents rating the phrases as exactly equal. The correlation between the two is {mm_cor}.")
cap <- source_caption(source = "Kucharski AJ (2026) CAPphrase: Comparative and Absolute Probability phrase dataset. DOI: 10.5281/zenodo.18750055", graphic = social)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_polygon(
    data = pol1,
    mapping = aes(x = x, y = y),
    fill = highlight_col,
    colour = "transparent",
    alpha = 0.25
  ) +
  geom_polygon(
    data = pol2,
    mapping = aes(x = x, y = y),
    fill = highlight_col2,
    colour = "transparent",
    alpha = 0.25
  ) +
  geom_point(
    data = plot_data,
    mapping = aes(x = `May Happen`, y = `Might Happen`,
                  colour = I(Diff_col)),
    alpha = 0.5,
    size = 1.75
  ) +
  annotate(
    "segment",
    x = 0, y = 0, xend = 100, yend = 100,
    colour = text_col
  ) +
  geom_textbox(
    data = data.frame(
      x = c(99, 1),
      y = c(12, 88),
      label = c(annot1, annot2),
      colour = c(highlight_col, highlight_col2),
      hjust = c(1, 0)
    ),
    mapping = aes(x = x, y = y, label = label,
                  hjust = hjust, halign = hjust),
    box.colour = "transparent",
    fill = alpha(bg_col, 0.5),
    family = body_font
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = "Probability for 'May Happen' (%)",
    y = "Probability for 'Might Happen' (%)"
  ) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(5, 15, 5, 5),
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
      margin = margin(b = 30, t = 0),
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
    axis.title.x = element_text(
      hjust = 0.96
    ),
    axis.title.y = element_text(
      angle = 0, hjust = 1,
      vjust = 1.07,
      margin = margin(r = -155)
    )
  ) +
  canvas(
    width = 7, height = 5.5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = "2026/viz/day_15.png"
)
