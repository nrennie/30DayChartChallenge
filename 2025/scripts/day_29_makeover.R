# Set up ------------------------------------------------------------------

library(ggplot2)
library(showtext)
library(nrBrand)
library(ggtext)
library(geomtextpath)
library(ggh4x)
library(scales)
library(countrycode)

income <- readr::read_csv("content/blog/2025-05-23-chart-makeover/income-share-of-the-richest-1.csv")


# Version 0 ---------------------------------------------------------------

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

bg_col <- "#F58A07"
text_col <- "#073D74"

font_add_google("Covered By Your Grace", "Grace")
font_add_google("Kablammo", db_cache = FALSE)
showtext_auto()
showtext_opts(dpi = 300)

body_font <- "Grace"
title_font <- "Kablammo"

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

ggsave("content/blog/2025-05-23-chart-makeover/images/v0.png",
  height = 6, width = 6, bg = bg_col
)

# Version 1 ---------------------------------------------------------------

bg_col <- "#FAFAFA"
text_col <- "black"
font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)
body_font <- "Ubuntu"
title_font <- "Ubuntu"
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
cap <- paste0(
  "**Data**: World Inequality Database (WID). Processed by Our World in Data<br>**Graphic**: ", social
)

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
  theme_grey(base_family = body_font, base_size = 11) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
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
ggsave("content/blog/2025-05-23-chart-makeover/images/v1.png",
  height = 6, width = 6, bg = bg_col
)


# Version 2 ---------------------------------------------------------------

new_plot_data <- plot_data |>
  left_join(sd_data, by = "Country") |>
  pivot_longer(cols = c(Income, sd))
ggplot() +
  geom_point(
    data = new_plot_data,
    mapping = aes(
      x = Country, y = value,
      colour = as.factor(Year),
    ),
    alpha = 0.2,
    size = 4
  ) +
  facet_wrap(~name, ncol = 2, scales = "free_y") +
  labs(title = title, subtitle = st, caption = cap, x = "") +
  theme_grey(base_family = body_font, base_size = 11) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
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
ggsave("content/blog/2025-05-23-chart-makeover/images/v2.png",
  height = 6, width = 6, bg = bg_col
)


# Version 3 ---------------------------------------------------------------

ggplot() +
  geom_point(
    data = new_plot_data,
    mapping = aes(
      y = Country,
      x = value,
      colour = as.factor(Year),
    ),
    alpha = 0.2,
    size = 4
  ) +
  facet_wrap(~name, ncol = 2, scales = "free_x") +
  labs(title = title, subtitle = st, caption = cap, y = "") +
  theme_grey(base_family = body_font, base_size = 11) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
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
ggsave("content/blog/2025-05-23-chart-makeover/images/v3a.png",
  height = 6, width = 6, bg = bg_col
)

new_plot_data_2 <- new_plot_data |>
  mutate(
    Continent = countrycode::countrycode(
      sourcevar = Country,
      origin = "country.name",
      destination = "continent"
    )
  ) |>
  drop_na(Continent)
ggplot() +
  geom_point(
    data = new_plot_data_2,
    mapping = aes(
      y = Country,
      x = value,
      colour = as.factor(Year),
    ),
    alpha = 0.2,
    size = 4
  ) +
  facet_grid(Continent ~ name, scales = "free", space = "free") +
  labs(title = title, subtitle = st, caption = cap, y = "") +
  theme_grey(base_family = body_font, base_size = 8) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.3)),
    axis.ticks.y = element_blank(),
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
ggsave("content/blog/2025-05-23-chart-makeover/images/v3b.png",
  height = 9, width = 6, bg = bg_col
)


# Version 4 ---------------------------------------------------------------

new_plot_data_3 <- new_plot_data_2 |>
  filter(Year %in% c(2013, 2023), name == "Income") |>
  pivot_wider(names_from = Year) |>
  drop_na(c(`2023`, `2013`)) |>
  filter(`2023` != `2013`) |>
  pivot_longer(
    cols = c(`2023`, `2013`),
    names_to = "Year", values_to = "Income"
  ) |>
  select(-name)
ggplot() +
  geom_point(
    data = new_plot_data_3,
    mapping = aes(
      y = reorder(Country, Income),
      x = Income,
      colour = as.factor(Year),
    ),
    alpha = 0.2,
    size = 4
  ) +
  facet_grid(Continent ~ "1",
    space = "free", scale = "free_y",
    switch = "y"
  ) +
  labs(
    title = title, subtitle = st, caption = cap,
    x = "Share of income received by richest 1% of population (%)",
    y = ""
  ) +
  theme_grey(base_family = body_font, base_size = 8) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.ticks.y = element_blank(),
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
      margin = margin(b = 10, t = 5),
      family = body_font,
      maxwidth = 1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 5),
      family = body_font
    ),
    strip.background.y = element_blank(),
    strip.text.x = element_blank(),
    strip.clip = "off",
    panel.spacing.y = unit(1, "lines"),
    strip.text.y.left = element_text(
      face = "bold",
      hjust = 1, vjust = 1,
      angle = 0,
      size = rel(1.2),
      margin = margin(t = -5, l = -35)
    ),
    plot.margin = margin(5, 10, 5, 10),
  )
ggsave("content/blog/2025-05-23-chart-makeover/images/v4.png",
  height = 9, width = 6, bg = bg_col
)


# Version 5 ---------------------------------------------------------------

new_plot_data_4 <- new_plot_data_3 |>
  filter(
    !(Continent == "Oceania" & Income > 15),
    !(Continent == "Europe" & Income %in% c(11.5, 10.400001))
  ) |>
  pivot_wider(names_from = Year, values_from = Income) |>
  mutate(diff = `2023` - `2013`) |>
  filter(diff < 0) |>
  pivot_longer(cols = c(`2023`, `2013`), names_to = "Year", values_to = "Income")

c_levels <- new_plot_data_4 |>
  filter(Year == 2023) |>
  arrange(-diff) |>
  pull(Country)
new_plot_data_4$Country <- factor(new_plot_data_4$Country, levels = c_levels)

con_levels <- new_plot_data_4 |>
  filter(Year == 2023) |>
  group_by(Continent) |>
  slice_min(diff) |>
  arrange(diff) |>
  pull(Continent)
new_plot_data_4$Continent <- factor(new_plot_data_4$Continent, levels = con_levels)

ggplot() +
  geom_line(
    data = new_plot_data_4,
    mapping = aes(
      y = Country,
      x = Income,
      group = Country
    ),
    colour = text_col
  ) +
  geom_point(
    data = new_plot_data_4,
    mapping = aes(
      y = reorder(Country, Income),
      x = Income,
      colour = as.factor(Year),
      shape = as.factor(Year)
    ),
    size = 3
  ) +
  facet_grid(Continent ~ "1",
    space = "free", scale = "free_y",
    switch = "y"
  ) +
  scale_colour_manual(values = c("#274C77", "#D90368")) +
  scale_x_continuous(limits = c(5, 30)) +
  labs(
    title = title, subtitle = st, caption = cap,
    x = "Share of income received by richest 1% of population (%)",
    y = ""
  ) +
  theme_grey(base_family = body_font, base_size = 8) +
  theme(
    legend.position = "inside",
    legend.background = element_rect(
      fill = alpha(text_col, 0.1),
      colour = text_col
    ),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.position.inside = c(0.86, 0.05),
    legend.title = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.ticks.y = element_blank(),
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
      margin = margin(b = 10, t = 5),
      family = body_font,
      maxwidth = 1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 5),
      family = body_font
    ),
    strip.background.y = element_blank(),
    strip.text.x = element_blank(),
    strip.clip = "off",
    panel.spacing.y = unit(1, "lines"),
    strip.text.y.left = element_text(
      face = "bold",
      hjust = 1, vjust = 1,
      angle = 0,
      size = rel(1.2),
      margin = margin(t = -5, l = -37)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = alpha(text_col, 0.3),
      linewidth = 0.3
    ),
    panel.grid.minor.x = element_line(
      colour = alpha(text_col, 0.2),
      linewidth = 0.2
    ),
    plot.margin = margin(5, 10, 5, 10),
  )
ggsave("content/blog/2025-05-23-chart-makeover/images/v5.png",
  height = 9, width = 6, bg = bg_col
)


# Version 6 ---------------------------------------------------------------

title <- "El Salvador makes largest improvement in income inequality."
st <- "Here, income inequality is measured by the percentage of income received by the richest 1% of the population, where income is measured before payment of taxes and non-pension benefits but after the payment of public and private pensions. Higher values indicate more unequal income distribution. In the 10 years between 2013 and 2023, of countries where the percentage decreased, El Salvador made the biggest change. Data is not available for all countries."
ggplot() +
  geom_line(
    data = new_plot_data_4,
    mapping = aes(
      y = Country,
      x = Income,
      group = Country
    ),
    colour = text_col
  ) +
  geom_point(
    data = new_plot_data_4,
    mapping = aes(
      y = reorder(Country, Income),
      x = Income,
      colour = as.factor(Year),
      shape = as.factor(Year)
    ),
    size = 3
  ) +
  facet_grid(Continent ~ "1",
             space = "free", scale = "free_y",
             switch = "y"
  ) +
  scale_colour_manual(values = c("#274C77", "#D90368")) +
  scale_x_continuous(limits = c(5, 30)) +
  labs(
    title = title, subtitle = st, caption = cap,
    x = "Share of income received by richest 1% of population (%)",
    y = ""
  ) +
  theme_grey(base_family = body_font, base_size = 8) +
  theme(
    legend.position = "inside",
    legend.background = element_rect(
      fill = alpha(text_col, 0.1),
      colour = text_col
    ),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.position.inside = c(0.86, 0.05),
    legend.title = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.ticks.y = element_blank(),
    text = element_text(colour = text_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 5),
      family = body_font,
      maxwidth = 1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    strip.background.y = element_blank(),
    strip.text.x = element_blank(),
    strip.clip = "off",
    panel.spacing.y = unit(1, "lines"),
    strip.text.y.left = element_text(
      face = "bold",
      hjust = 1, vjust = 1,
      angle = 0,
      size = rel(1.2),
      margin = margin(t = -5, l = -37)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = alpha(text_col, 0.3),
      linewidth = 0.3
    ),
    panel.grid.minor.x = element_line(
      colour = alpha(text_col, 0.2),
      linewidth = 0.2
    ),
    plot.margin = margin(5, 10, 5, 10),
  )
ggsave("content/blog/2025-05-23-chart-makeover/images/v6.png",
  height = 9, width = 6, bg = bg_col
)
