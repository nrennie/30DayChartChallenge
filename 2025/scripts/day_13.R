# Packages ----------------------------------------------------------------

library(ggplot2)
library(showtext)
library(nrBrand)
library(ggtext)
library(lemon)


# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Colours -----------------------------------------------------------------

bg_col <- "grey97"
text_col <- "black"
highlight_col <- "#275DAD"
highlight_col2 <- "#A31621"


# Data wrangling ----------------------------------------------------------

plot_data <- income |>
  dplyr::filter(
    stringr::str_detect(Country, "(WID)", negate = TRUE),
    Country != "World",
    Year %in% c(2022, 2023)
  ) |>
  dplyr::mutate(Country = stringr::str_remove(Country, " \\(country\\)")) |>
  tidyr::drop_na() |>
  dplyr::rename(
    Income = `Income share of the richest 1% (before tax) (World Inequality Database)`
  ) |>
  tidyr::pivot_wider(names_from = Year, values_from = Income) |>
  dplyr::mutate(
    Change = `2023` - `2022`
  ) |>
  dplyr::rename(Income = `2023`) |>
  dplyr::select(-`2022`) |>
  dplyr::mutate(
    col = dplyr::case_when(
      Change > 0 ~ highlight_col2,
      Change == 0 ~ "grey60",
      Change < 0 ~ highlight_col
    )
  )

label_data <- data.frame(
  x = c(27.5, 7.5, 27.5, 7.5),
  y = c(-3, -3, 3, 3),
  label = c(
    "Higher inequality, but improving",
    "Lower inequality, and still improving",
    "Higher inequality, and getting worse",
    "Lower inequality, but getting worse"
  )
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
  font_family = body_font
)
title <- "The share of income received by the richest 1% of the population."
st <- "Income is measured before payment of taxes and non-pension benefits but after the payment of public and private pensions."
cap <- paste0(
  "**Data**: World Inequality Database (WID). Processed by Our World in Data<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = median(plot_data$Income)) +
  geom_point(
    data = plot_data,
    mapping = aes(
      x = Income, y = Change,
      fill = col,
      colour = col
    ),
    pch = 21,
    alpha = 0.3,
    size = 3,
  ) +
  geom_text(
    data = label_data,
    mapping = aes(
      x = x, y = y,
      label = stringr::str_wrap(label, 12)
    ),
    family = body_font,
    colour = text_col,
    size = 3.5,
    fontface = "bold",
    lineheight = 1
  ) +
  # annotate
  annotate("text",
    x = 11, y = 3, label = "Slovakia",
    size = 3.5, family = body_font, fontface = "italic"
  ) +
  annotate("curve",
    x = 11,
    xend = 10.5,
    y = 3.2,
    yend = 3.5,
    color = text_col,
    curvature = 0.5,
    arrow = arrow(
      length = unit(1.5, "mm"), type = "closed"
    )
  ) +
  annotate("text",
    x = 18, y = -3, label = "Mongolia",
    size = 3.5, family = body_font, fontface = "italic"
  ) +
  annotate("curve",
           x = 18,
           xend = 16.5,
           y = -2.8,
           yend = -2.7,
           color = text_col,
           curvature = 0.5,
           arrow = arrow(
             length = unit(1.5, "mm"), type = "closed"
           )
  ) +
  annotate("text",
    x = 21, y = 3, label = "Lebanon",
    size = 3.5, family = body_font, fontface = "italic"
  ) +
  annotate("curve",
           x = 20.3,
           xend = 20.8,
           y = 2.8,
           yend = 2.1,
           color = text_col,
           curvature = 0.5,
           arrow = arrow(
             length = unit(1.5, "mm"), type = "closed"
           )
  ) +
  # style
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(limits = c(5, 30)) +
  scale_y_symmetric(limits = c(-4, 4)) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = "Percentage of income received by richest 1% in 2023.",
    y = "Percentage point increase between 2022 and 2023."
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_family = body_font, base_size = 10) +
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
      family = body_font,
      maxwidth = 0.8
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    plot.margin = margin(5, 10, 5, 10)
  )


# Save --------------------------------------------------------------------

ggsave("2025/viz/day_13.png", height = 5, width = 7, bg = bg_col)
