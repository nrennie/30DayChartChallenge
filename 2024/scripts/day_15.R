# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggforce)
library(scales)


# Data --------------------------------------------------------------------

# pie data
# based on: https://flowingdata.com/2012/10/09/history-of-earth-in-24-hour-clock/
total <- 12*60*60 ## seconds in 12 hours
humans <- 38.5 ## 77 seconds / 2
pre <- total - (humans)
plot_data <- data.frame(
  Humans = humans,
  Before = pre
) |>
  pivot_longer(everything()) |>
  mutate(
    name = factor(name, levels = c("Before", "Humans"))
  ) |>
  arrange(name)

# clock data
r <- 1.1
theta <- seq(0, (2 * pi), length.out = 13)[1:12]
clock_data <- tibble(
  x = r * cos(theta),
  y = r * sin(theta),
  angle = 90 + 360 * (theta / (2 * pi)),
  label = c("III", "II", "I", "XII", "XI", "X", "IX", "VIII", "VII", "VI", "V", "IV")
)
theta2 <- seq(0, (2 * pi), length.out = 61)[1:60]
clock_data2 <- tibble(
  x = r * cos(theta2),
  y = r * sin(theta2)
)
clock_data3 <- tibble(
  x = c(0.8, 0.6) * cos(theta[c(2, 6)]),
  y = c(0.8, 0.6) * sin(theta[c(2, 6)]),
  grp = c(1, 2)
)

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "grey10"

body_font <- "roboto"
title_font <- "roboto_slab"


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "recording"),
  device = "png",
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "The History of Earth"
st <- "If the history of Earth was compressed into 12 hours, humanity would only
have been here for around 39 seconds."
cap <- paste0(
  "**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_arc_bar(
    data = plot_data,
    mapping = aes(
      x0 = 0, y0 = 0, r0 = 0, r = 0.95,
      amount = value,
      fill = name
    ),
    stat = "pie",
    color = NA
  ) +
  # text
  annotate("text", x = 0, y = 1,
           label = "11:59:21 - Humanity", size = 7,
           family = body_font, colour = text_col) +
  # clock
  geom_point(
    data = data.frame(),
    mapping = aes(x = 0, y = 0),
    size = 3,
    colour = text_col
  ) +
  geom_segment(
    data = clock_data3,
    mapping = aes(x = 0, y = 0, xend = x, yend = y, group = grp),
    linewidth = 1,
    colour = text_col
  ) +
  geom_point(
    data = clock_data2,
    mapping = aes(x = x, y = y),
    size = 0.5,
    colour = text_col
  ) +
  geom_label(
    data = clock_data,
    mapping = aes(x = x, y = y, label = label),
    family = title_font,
    size = 8,
    label.size = 0,
    fill = bg_col,
    colour = text_col,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("gray80", "gray50", "gray20")
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_fixed() +
  theme_void(base_size = 24, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.6)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 10),
      lineheight = 0.5,
      family = body_font,
      size = rel(0.9)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "viz", "gifs", paste0("day_15", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)

unlink("2024/recording/", recursive = TRUE)
