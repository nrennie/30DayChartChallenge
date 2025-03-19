# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(sf)
library(roughsf)


# Load data ---------------------------------------------------------------

bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Gloria Hallelujah", "gloria")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#f5f5dc"
text_col <- "#5C4033"


# Data wrangling ----------------------------------------------------------

colour_lookup <- bob_ross |>
  select(colors, color_hex) |>
  distinct() |>
  # process colours
  mutate(colors = str_remove_all(colors, "\\[")) |>
  mutate(colors = str_remove_all(colors, "\\]")) |>
  mutate(colors = str_remove_all(colors, "'")) |>
  mutate(colors = str_remove_all(colors, "\\\\n")) |>
  mutate(colors = str_remove_all(colors, "\\\\r")) |>
  # process hex
  mutate(color_hex = str_remove_all(color_hex, "\\[")) |>
  mutate(color_hex = str_remove_all(color_hex, "\\]")) |>
  mutate(color_hex = str_remove_all(color_hex, "'")) |>
  # unlist
  mutate(colors = strsplit(colors, ",")) |>
  mutate(color_hex = strsplit(color_hex, ", ")) |>
  unnest(c(colors, color_hex)) |>
  distinct() |>
  mutate(colors = str_trim(colors)) |>
  distinct()

# number of times each colour used per season (avg/episode)
count_data <- bob_ross |>
  select(c(Black_Gesso:Alizarin_Crimson)) |>
  summarise(across(Black_Gesso:Alizarin_Crimson, ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(
    cols = everything(),
    names_to = "colors",
    values_to = "n"
  ) |>
  mutate(colors = str_replace_all(colors, "_", " ")) |>
  left_join(colour_lookup, by = "colors") |>
  mutate(perc = round(100 * n / sum(n))) |>
  select(color_hex, perc) |>
  group_by(color_hex) |>
  summarise(perc = sum(perc)) |>
  ungroup()

# check if rounds to 100 by chance?
sum(count_data$perc) == 100

# uncount
plot_data <- count_data |>
  uncount(perc) |>
  rename(fill = color_hex)

# function to make sf square given starting point
make_square <- function(x0, y0, width = 1) {
  st_polygon(
    list(
      cbind(
        c(x0, x0 + width, x0 + width, x0, x0),
        c(y0, y0, y0 + width, y0 + width, y0)
      )
    )
  )
}

poly_list <- purrr::map2(
  .x = rep(1:10, times = 10),
  .y = rep(1:10, each = 10),
  .f = ~ make_square(.x, .y, width = 0.8)
)

plot_sf <- st_sf(plot_data, geometry = poly_list)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "recording"),
  device = "png",
  width = 4,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

title <- "The Colours of Bob Ross Paintings"
cap <- "Graphic: Nicola Rennie"


# Plot --------------------------------------------------------------------

plot_sf$fillstyle <- "cross-hatch"
plot_sf$fillweight <- 0.8

rsf <- roughsf::roughsf(plot_sf,
  title = title,
  title_font = "48px Pristina",
  caption = cap,
  caption_font = "30px Pristina",
  roughness = 3, bowing = 2, simplification = 4,
  width = 800, height = 800,
)
rsf


# Save image --------------------------------------------------------------

save_roughsf(rsf, "2024/viz/day_04.png", background = bg_col, wait = 4)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "viz", "gifs", paste0("day_04", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)

unlink("2024/recording/", recursive = TRUE)
