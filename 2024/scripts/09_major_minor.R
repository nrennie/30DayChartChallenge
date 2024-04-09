# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(scales)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "recording"),
  device = "png",
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)


# Function ----------------------------------------------------------------

plot_chords <- function(chords, nrow = 1, size = 30) {

  # base plot

  base_plot <- ggplot() +
    geom_linerange(
      data = data.frame(
        x = 1:6,
        ymin = rep(0, 6),
        ymax = rep(5, 6)
      ),
      mapping = aes(x = x, ymin = ymin, ymax = ymax),
      lineend = "square"
    ) +
    geom_linerange(
      data = data.frame(
        y = 0:5,
        xmin = rep(1, 6),
        xmax = rep(6, 6)
      ),
      mapping = aes(y = y, xmin = xmin, xmax = xmax),
      lineend = "square"
    ) +
    geom_linerange(
      data = data.frame(
        y = 0.1,
        xmin = 1,
        xmax = 6
      ),
      mapping = aes(y = y, xmin = xmin, xmax = xmax),
      linewidth = 5
    ) +
    scale_y_reverse(limits = c(5.1, -0.75)) +
    theme_void(base_size = size) +
    theme(
      plot.background = element_rect(fill = "white", colour = "white"),
      plot.margin = margin(10, 10, -5, 10),
      strip.background = element_rect(fill = "gray80"),
      strip.text = element_text(margin = margin(t = 10, b = 10),
                                face = "bold")
    )

  # process data

  plot_data <- chords |>
    separate_longer_position(play, 1) |>
    mutate(x = rep(1:6, times = nrow(chords))) |>
    mutate(y =
             case_when(
               play %in% c("X", "0") ~ -0.5,
               TRUE ~ as.numeric(play)
             )
    )

  not_played <- plot_data |>
    filter(play %in% c("X", "0"))

  played <- plot_data |>
    filter(!(play %in% c("X", "0")))

  # plot data

  base_plot +
    geom_point(
      data = played,
      mapping = aes(x = x, y = y),
      size = size/3.75
    ) +
    geom_text(
      data = not_played,
      mapping = aes(x = x, y = y, label = play),
      size = size/3
    ) +
    facet_wrap(~chord, nrow = nrow)

}


# Example -----------------------------------------------------------------


chords <- data.frame(
  chord = c("Am", "A", "Em", "E"),
  play = c("X02210", "X02220", "022000", "022100")
)

plot_chords(chords, size = 24)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "viz", "gifs", paste0("day_09", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)

unlink("2024/recording/", recursive = TRUE)

