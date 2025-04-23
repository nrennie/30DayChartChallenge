# Packages ----------------------------------------------------------------

library(ggplot2)
library(showtext)
library(nrBrand)
library(ggtext)


# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Colours -----------------------------------------------------------------

bg_col <- "grey90"
text_col <- "black"
highlight_col <- PrettyCols::prettycols("Dark")[4]


# Data wrangling ----------------------------------------------------------

plot_data <- income |>
  dplyr::rename(Income = `Income share of the richest 1% (before tax) (World Inequality Database)`) |>
  dplyr::filter(
    Country == "World"
  ) |>
  dplyr::select(-Country) |>
  dplyr::arrange(Year) |>
  tibble::add_row(
    Year = c(2025, 1999, 1899, 1800), Income = c(NA, NA, NA, NA)
  ) |>
  dplyr::mutate(Century = as.character(Year - (Year %% 100)))

label_data <- data.frame(
  x = c(1850, 1950, 2012.5),
  y = rep(28, 3),
  label = c(
    "A century of little data",
    "A century of change",
    "A century of stability?"
  )
) |>
  dplyr::mutate(Century = as.character(x - (x %% 100)))


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
title <- "The share of income received by the richest 1% of the population."
st <- "Income is measured before payment of taxes and non-pension benefits but after the payment of public and private pensions. Higher values indicate more unequal income distribution."
cap <- paste0(
  "**Data**: World Inequality Database (WID). Processed by Our World in Data<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(
    x = Year, y = Income, fill = Century,
    colour = Century, group = Century
  )
) +
  geom_smooth(
    data = dplyr::filter(plot_data, Century != "1800"),
    alpha = 0.3
  ) +
  geom_point() +
  geom_text(
    data = label_data,
    mapping = aes(
      x = x, y = y, label = label
    ),
    family = title_font
  ) +
  facet_wrap(~Century, nrow = 1, scales = "free_x") +
  scale_colour_manual(
    values = PrettyCols::prettycols("Dark")[c(1, 2, 4)]
  ) +
  scale_fill_manual(
    values = PrettyCols::prettycols("Dark")[c(1, 2, 4)]
  ) +
  scale_y_continuous(limits = c(15, 30)) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = "",
    y = "Percentage of income received by richest 1%."
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 9, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(
      fill = alpha("white", 0.2),
      colour = bg_col
    ),
    text = element_text(colour = text_col),
    panel.spacing = unit(0, "lines"),
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
    panel.grid.major = element_line(
      colour = alpha(text_col, 0.1),
      linewidth = 0.3
    ),
    plot.margin = margin(5, 10, 5, 10),
    strip.text = element_blank()
  )


# Save --------------------------------------------------------------------

ggsave("2025/viz/day_23.png", height = 5, width = 7, bg = bg_col)

