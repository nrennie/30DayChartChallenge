# Packages ----------------------------------------------------------------

library(tidyverse)


# Data --------------------------------------------------------------------

# Data: https://www.gov.uk/government/statistics/wild-bird-populations-in-the-uk/wild-bird-populations-in-the-uk-and-england-1970-to-2023#native-breeding-wild-bird-populations
# Data: https://www.bto.org/community/news/202305-birds-your-doorstep-highlighting-50-years-change-bird-populations
birds <- readODS::read_ods("2025/data/birds.ods", sheet = "1", skip = 5)[, 1:5]
income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Wrangling ---------------------------------------------------------------

birds_data <- birds |>
  rename(Year = Year...1, Birds = `Unsmoothed index...2`) |>
  select(Year, Birds) |>
  filter(Year >= 1980) |>
  mutate(Birds = as.numeric(Birds) * 232 / 100) |>
  drop_na()

income_data <- income |>
  filter(Country == "United Kingdom") |>
  rename(Income = `Income share of the richest 1% (before tax) (World Inequality Database)`) |>
  select(Year, Income) |>
  drop_na()

# Correlation
k <- income_data |>
  left_join(birds_data) |>
  drop_na()
cor(k$Income, k$Birds) # -0.6358492


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
  "<span style='color:{text_col}; font-size:20pt;'>**Wealth inequality**</span><br>negatively correlates with<span style='color:{highlight_col}; font-size:20pt;'>**<br>Numbers of wild birds**</span>"
)


# Plot --------------------------------------------------------------------

# In the style of https://www.tylervigen.com/spurious-correlations
coeff <- 22
ggplot() +
  # Income
  geom_line(
    data = income_data,
    mapping = aes(x = Year, y = Income, colour = "Income"),
    linetype = "dotted",
    linewidth = 1
  ) +
  geom_point(
    data = income_data,
    mapping = aes(x = Year, y = Income, colour = "Income"),
    pch = 18,
    size = 3
  ) +
  # Birds
  geom_line(
    data = birds_data,
    mapping = aes(x = Year, y = Birds / coeff, colour = "Birds")
  ) +
  geom_point(
    data = birds_data,
    mapping = aes(x = Year, y = Birds / coeff, colour = "Birds")
  ) +
  # Styling
  labs(x = NULL, title = title) +
  scale_colour_manual(
    values = c(highlight_col, text_col),
    labels = c("Number of wild birds in UK (millions). Source: British Trust for Ornithology (BTO).", "Share of income received by richest 1% in UK. Source: World Inequality Database (WID). Processed by Our World in Data")
  ) +
  scale_y_continuous(
    name = "Share of income received by the richest 1%.",
    limits = c(6.5, 16),
    sec.axis = sec_axis(~ . * coeff,
      name = "Wild bird population (millions)"
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
  )


# Save --------------------------------------------------------------------

ggsave("2025/viz/day_17.png", height = 5.5, width = 7, bg = bg_col)

