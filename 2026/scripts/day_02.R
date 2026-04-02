library(ggplot2)
library(ggview)
library(showtext)
library(ggtext)
library(nrBrand)
library(dplyr)
library(ggimage)


# Load fonts --------------------------------------------------------------

font_add_google("Irish Grover", "Grover")
font_add_google("Vend Sans", "Vend")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Grover"
body_font <- "Vend"


# Define colours and fonts-------------------------------------------------

bg_col <- "#C6E6C7"
text_col <- "#193919"
highlight_col <- "#4caf50"

# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Can you find a four-leaf clover?"
st <- "A four-leaf clover is a rare mutation of the common three-leaf clover (*Trifolium repens*), often considered a symbol of good luck, faith, hope, and love. The frequency of four-leaf clovers is thought to be around 5,000 to 1, with a 2017 study estimating it at 5,076 to 1. There are 5,077 clover icons in this grid. Can you find the four-leaf clover?"
cap <- paste0("**Icons**: flaticon.com | ", source_caption(source = "sharetheluck.ch", graphic = social))


# Data --------------------------------------------------------------------

num_clovers <- 5076
grid_size <- num_clovers + 1

set.seed(2)
clover_pos <- sample(1:num_clovers, 1)

plot_data <- expand.grid(
  x = 1:floor(sqrt(grid_size)),
  y = 1:ceiling(sqrt(grid_size))
) |>
  as_tibble() |>
  filter(row_number() <= grid_size) |>
  mutate(img = if_else(
    row_number() == clover_pos,
    "2026/images/clover.png",
    "2026/images/shamrock.png"
  ))

clover_data <- plot_data |>
  filter(row_number() == clover_pos)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data
) +
  # geom_point(
  #   mapping = aes(x = x, y = y, colour = img)
  # ) +
  geom_image(
    mapping = aes(x = x, y = y, image = img),
    size = 0.015,
    by = "height"
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
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
      margin = margin(b = 10, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      family = body_font
    )
  ) +
  canvas(
    width = 7, height = 8,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p

p_reveal <- p +
  annotate("rect",
    xmin = clover_data$x - 0.75,
    xmax = clover_data$x + 0.75,
    ymin = clover_data$y - 0.75,
    ymax = clover_data$y + 0.75,
    colour = "#FF7600",
    fill = "transparent"
  )


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = "2026/viz/day_02.png"
)

save_ggplot(
  plot = p_reveal,
  file = "2026/viz/day_02_reveal.png"
)
