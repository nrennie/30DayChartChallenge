# Packages ----------------------------------------------------------------

library(ggplot2)
library(showtext)
library(nrBrand)
library(ggtext)


# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")

# mhttps://www.ers.usda.gov/data-products/food-availability-per-capita-data-system

dairy <- readr::read_csv("2025/data/dymfg.csv")


# Colours -----------------------------------------------------------------

bg_col <- "grey97"
text_col <- "black"
d_col <- "#275DAD"
r_col <- "#A31621"


# Data wrangling ----------------------------------------------------------

dairy_data <- dairy |>
  dplyr::filter(
    Year >= 1953,
    Attribute == "Food availability-Per capita-Total-Pounds",
    Commodity == "All dairy products (milk-fat milk-equivalent basis): Supply and use"
  ) |>
  dplyr::select(Year, Value) |>
  dplyr::mutate(Value = as.numeric(Value))

plot_data <- income |>
  dplyr::filter(
    Country == "United States",
    Year >= 1953
  ) |>
  dplyr::left_join(
    dairy_data,
    by = "Year"
  ) |>
  tidyr::drop_na() |>
  dplyr::rename(
    `Percentage of income received by richest 1%` = `Income share of the richest 1% (before tax) (World Inequality Database)`,
    `Availabillity of dairy products (pounds per capita)` = Value
  ) |>
  dplyr::select(-Country) |>
  dplyr::mutate(
    Year = lubridate::ymd(paste0(Year, "-01-01"))
  ) |>
  tidyr::pivot_longer(
    -Year,
    names_to = "type"
  )

label_data <- data.frame(
  x = lubridate::ymd(c("19540101", "19540101")),
  y = c(22, 10.5),
  label = c("Higher income inequality", "Lower income inequality"),
  type = c("Percentage of income received by richest 1%", "Percentage of income received by richest 1%")
)

label_data2 <- data.frame(
  x = lubridate::ymd(c("20200101", "20200101")),
  y = c(720, 535),
  label = c("Higher food availability", "Lower food availability"),
  type = c("Availabillity of dairy products (pounds per capita)", "Availabillity of dairy products (pounds per capita)")
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
title <- "Are dairy availability and income inequality economic indicators?"
st <- "In the United States, the percentage of income received by the richest 1% of the population (where income is measured before payment of taxes and non-pension benefits but after the payment of public and private pensions) tracks closely with availability of dairy products per capita. It's unlikely the either directly affects the other, and instead there is a confounding factor impacting both."
cap <- paste0(
  "**Data**: World Inequality Database (processed by Our World in Data), USDA (Economic Research Service), and data.gov <br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_rect(
    data = presidential,
    mapping = aes(
      xmin = start, xmax = end,
      ymin = -Inf, ymax = Inf,
      fill = party
    ),
    alpha = 0.3
  ) +
  geom_line(
    data = plot_data,
    mapping = aes(x = Year, y = value)
  ) +
  geom_text(
    data = label_data,
    mapping = aes(
      x = x, y = y, label = label
    ),
    family = body_font,
    size = 3,
    hjust = 0,
    fontface = "italic"
  ) +
  geom_text(
    data = label_data2,
    mapping = aes(
      x = x, y = y, label = label
    ),
    family = body_font,
    size = 3,
    hjust = 1,
    fontface = "italic"
  ) +
  facet_wrap(~type, ncol = 1, scales = "free") +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = "",
    y = NULL
  ) +
  scale_fill_manual(values = c("Republican" = r_col, "Democratic" = d_col)) +
  scale_x_date(expand = c(0, 0)) +
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
      maxwidth = 1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      family = body_font
    ),
    plot.margin = margin(5, 15, 5, 10),
    panel.spacing = unit(0.5, "lines"),
    strip.text = element_text(
      face = "bold",
      hjust = 0, size = rel(1.1)
    )
  )


# Save --------------------------------------------------------------------

ggsave("2025/viz/day_12.png", height = 5, width = 7, bg = bg_col)
