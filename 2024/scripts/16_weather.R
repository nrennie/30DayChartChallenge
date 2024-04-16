# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(scales)

# Load data ---------------------------------------------------------------

# Data from HADCET: https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_monthly_totals.txt
df <- as_tibble(read.table("2024/data/hadcet.txt", header = T))


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "white"
text_col <- "black"
col_palette <- rcartocolor::carto_pal(n = 5, "Geyser")
highlight_col <- col_palette[5]

body_font <- "roboto"
title_font <- "roboto_slab"


# Data wrangling ----------------------------------------------------------

plot_data <- df |>
  pivot_longer(cols = `Jan`:`Dec`,
               names_to = "month",
               values_to = "temp") |>
  mutate(month = factor(month,
                        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) |>
  mutate(
    temp = if_else(temp == -99.9, NA, temp)
  ) |>
  mutate(temp_cut = cut(temp, breaks = 5)) |>
  select(-Annual)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "recording"),
  device = "png",
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "HadCET: Hadley Centre Central England Temperature"
st <- "The CET dataset is the longest instrumental record of temperature in the
world. The mean, minimum and maximum datasets are updated monthly, with data for
a month usually available by the 3rd of the next month. A provisional CET value
for the current month is calculated on a daily basis. The mean daily data series
begins in 1772 and the mean monthly data in 1659. Mean maximum and minimum daily
and monthly data are also available, beginning in 1878.<br><br>Average monthly temperatures:"
cap <- paste0(
  "**Data**: Met Office<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_tile(data = plot_data,
            mapping = aes(month, Year, fill = temp),
            colour = NA,
            linewidth = 0) +
  scale_x_discrete(limits = rev) +
  rcartocolor::scale_fill_carto_c(palette = "Geyser") +
  guides(fill = guide_legend(label.position = "bottom",
                             title.position = "top",
                             title = "Temperature")) +
  labs(title = title,
       subtitle = st,
       caption = cap,
       x = NULL, y = NULL) +
  coord_flip(expand = FALSE) +
  theme_minimal(base_size = 24, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 15, 5, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(2)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    axis.text.y = element_text(angle = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "viz", "gifs", paste0("day_16", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)

unlink("2024/recording/", recursive = TRUE)
