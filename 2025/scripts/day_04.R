
# Packages ----------------------------------------------------------------

library(ggplot2)
library(ggtext)
library(showtext)
library(nrBrand)


# Read in data ------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu", "ubuntu")
showtext_auto()
showtext_opts(dpi = 300)


# Parameters --------------------------------------------------------------

highlight_col <- "#3943B7"
text_col <- "#13173E"
bg_col <- "#E0E2F5"

body_font <- "ubuntu"


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA,
  linkedin = NA,
  bluesky = NA
)
cap <- paste0(
  "**Data**: World Inequality Database (WID). Processed by Our World in Data<br>**Graphic**: ", social
)


# Data wrangling ----------------------------------------------------------

income_world <- income |>
  dplyr::filter(Country == "World") |>
  dplyr::slice_max(Year, n = 1) |>
  dplyr::rename(Income = `Income share of the richest 1% (before tax) (World Inequality Database)`)
plot_data <- data.frame(
  type = c("People", "Wealth"),
  Yes = c(1, income_world$Income)
) |>
  dplyr::mutate(No = 100 - Yes) |>
  tidyr::pivot_longer(
    -type,
    values_to = "n",
    names_to = "YN"
  ) |>
  dplyr::filter(YN == "Yes")


# Plot --------------------------------------------------------------------


ggplot(
  data = plot_data
) +
  geom_rect(mapping = aes(xmin = 0, xmax = sqrt(n),
            ymin = 0, ymax = sqrt(n)),
            fill = highlight_col, alpha = 0.6) +
  # labels
  annotate("text", x = 2, y = 2, label = "1% of people...",
           family = body_font) +
  annotate("text", x = 7, y = 2, label = "... have 21% of the wealth.",
           family = body_font) +
  # Arrows
  annotate("curve",
           x = 2,
           xend = 1.3,
           y = 1.4,
           yend = 0.5,
           color = text_col,
           curvature = -0.5,
           arrow = arrow(
             length = unit(1.5, "mm"), type = "closed"
           )
  ) +
  annotate("curve",
           x = 6,
           xend = 5,
           y = 2.6,
           yend = 3.5,
           color = text_col,
           curvature = 0.5,
           arrow = arrow(
             length = unit(1.5, "mm"), type = "closed"
           )
  ) +
  labs(tag = cap) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_family = body_font, base_size = 7) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      valign = 1,
      vjust = 1,
      margin = margin(l = 5, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    plot.tag.position = c(0, 1)
  )


# Save --------------------------------------------------------------------

ggsave("2025/viz/day_04.png", height = 5, width = 5)

