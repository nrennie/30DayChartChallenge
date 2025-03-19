library(tidyverse)
library(lubridate)
library(nrBrand)
library(glue)
library(ggtext)
library(camcorder)

# Download data -----------------------------------------------------------

# data from https://s3.amazonaws.com/capitalbikeshare-data/index.html
# Save in a directory called `Raw data`

# Load data ---------------------------------------------------------------

# 2017
d_2017_Q1 <- read.csv("Raw data/2017Q1-capitalbikeshare-tripdata.csv")
d_2017_Q2 <- read.csv("Raw data/2017Q2-capitalbikeshare-tripdata.csv")
d_2017_Q3 <- read.csv("Raw data/2017Q3-capitalbikeshare-tripdata.csv")
d_2017_Q4 <- read.csv("Raw data/2017Q4-capitalbikeshare-tripdata.csv")

# 2018
d_2018_01 <- read.csv("Raw data/201801-capitalbikeshare-tripdata.csv")
d_2018_02 <- read.csv("Raw data/201802-capitalbikeshare-tripdata.csv")
d_2018_03 <- read.csv("Raw data/201803-capitalbikeshare-tripdata.csv")
d_2018_04 <- read.csv("Raw data/201804-capitalbikeshare-tripdata.csv")
d_2018_05 <- read.csv("Raw data/201805-capitalbikeshare-tripdata.csv")
d_2018_06 <- read.csv("Raw data/201806-capitalbikeshare-tripdata.csv")
d_2018_07 <- read.csv("Raw data/201807-capitalbikeshare-tripdata.csv")
d_2018_08 <- read.csv("Raw data/201808-capitalbikeshare-tripdata.csv")
d_2018_09 <- read.csv("Raw data/201809-capitalbikeshare-tripdata.csv")
d_2018_10 <- read.csv("Raw data/201810-capitalbikeshare-tripdata.csv")
d_2018_11 <- read.csv("Raw data/201811-capitalbikeshare-tripdata.csv")
d_2018_12 <- read.csv("Raw data/201812-capitalbikeshare-tripdata.csv")

# 2019
d_2019_01 <- read.csv("Raw data/201901-capitalbikeshare-tripdata.csv")
d_2019_02 <- read.csv("Raw data/201902-capitalbikeshare-tripdata.csv")
d_2019_03 <- read.csv("Raw data/201903-capitalbikeshare-tripdata.csv")
d_2019_04 <- read.csv("Raw data/201904-capitalbikeshare-tripdata.csv")
d_2019_05 <- read.csv("Raw data/201905-capitalbikeshare-tripdata.csv")
d_2019_06 <- read.csv("Raw data/201906-capitalbikeshare-tripdata.csv")
d_2019_07 <- read.csv("Raw data/201907-capitalbikeshare-tripdata.csv")
d_2019_08 <- read.csv("Raw data/201908-capitalbikeshare-tripdata.csv")
d_2019_09 <- read.csv("Raw data/201909-capitalbikeshare-tripdata.csv")
d_2019_10 <- read.csv("Raw data/201910-capitalbikeshare-tripdata.csv")
d_2019_11 <- read.csv("Raw data/201911-capitalbikeshare-tripdata.csv")
d_2019_12 <- read.csv("Raw data/201912-capitalbikeshare-tripdata.csv")

# 2020
d_2020_01 <- read.csv("Raw data/202001-capitalbikeshare-tripdata.csv")
d_2020_02 <- read.csv("Raw data/202002-capitalbikeshare-tripdata.csv")
d_2020_03 <- read.csv("Raw data/202003-capitalbikeshare-tripdata.csv")
d_2020_04 <- read.csv("Raw data/202004-capitalbikeshare-tripdata.csv")
d_2020_05 <- read.csv("Raw data/202005-capitalbikeshare-tripdata.csv")
d_2020_06 <- read.csv("Raw data/202006-capitalbikeshare-tripdata.csv")
d_2020_07 <- read.csv("Raw data/202007-capitalbikeshare-tripdata.csv")
d_2020_08 <- read.csv("Raw data/202008-capitalbikeshare-tripdata.csv")
d_2020_09 <- read.csv("Raw data/202009-capitalbikeshare-tripdata.csv")
d_2020_10 <- read.csv("Raw data/202010-capitalbikeshare-tripdata.csv")
d_2020_11 <- read.csv("Raw data/202011-capitalbikeshare-tripdata.csv")
d_2020_12 <- read.csv("Raw data/202012-capitalbikeshare-tripdata.csv")

# 2021
d_2021_01 <- read.csv("Raw data/202101-capitalbikeshare-tripdata.csv")
d_2021_02 <- read.csv("Raw data/202102-capitalbikeshare-tripdata.csv")
d_2021_03 <- read.csv("Raw data/202103-capitalbikeshare-tripdata.csv")
d_2021_04 <- read.csv("Raw data/202104-capitalbikeshare-tripdata.csv")
d_2021_05 <- read.csv("Raw data/202105-capitalbikeshare-tripdata.csv")
d_2021_06 <- read.csv("Raw data/202106-capitalbikeshare-tripdata.csv")
d_2021_07 <- read.csv("Raw data/202107-capitalbikeshare-tripdata.csv")
d_2021_08 <- read.csv("Raw data/202108-capitalbikeshare-tripdata.csv")
d_2021_09 <- read.csv("Raw data/202109-capitalbikeshare-tripdata.csv")
d_2021_10 <- read.csv("Raw data/202110-capitalbikeshare-tripdata.csv")
d_2021_11 <- read.csv("Raw data/202111-capitalbikeshare-tripdata.csv")
d_2021_12 <- read.csv("Raw data/202112-capitalbikeshare-tripdata.csv")
d_2021_02$start_station_id <- as.numeric(d_2021_02$start_station_id)
d_2021_02$end_station_id <- as.numeric(d_2021_02$end_station_id)

# 2022
d_2022_01 <- read.csv("Raw data/202201-capitalbikeshare-tripdata.csv")
d_2022_02 <- read.csv("Raw data/202202-capitalbikeshare-tripdata.csv")
d_2022_03 <- read.csv("Raw data/202203-capitalbikeshare-tripdata.csv")
d_2022_04 <- read.csv("Raw data/202204-capitalbikeshare-tripdata.csv")
d_2022_05 <- read.csv("Raw data/202205-capitalbikeshare-tripdata.csv")
d_2022_06 <- read.csv("Raw data/202206-capitalbikeshare-tripdata.csv")
d_2022_07 <- read.csv("Raw data/202207-capitalbikeshare-tripdata.csv")
d_2022_08 <- read.csv("Raw data/202208-capitalbikeshare-tripdata.csv")
d_2022_09 <- read.csv("Raw data/202209-capitalbikeshare-tripdata.csv")
d_2022_10 <- read.csv("Raw data/202210-capitalbikeshare-tripdata.csv")
d_2022_11 <- read.csv("Raw data/202211-capitalbikeshare-tripdata.csv")
d_2022_12 <- read.csv("Raw data/202212-capitalbikeshare-tripdata.csv")

# Combine raw data sets ---------------------------------------------------

d_2017 <- bind_rows(list(d_2017_Q1, d_2017_Q2, d_2017_Q3, d_2017_Q4))
d_2018 <- bind_rows(list(
  d_2018_01, d_2018_02, d_2018_03, d_2018_04,
  d_2018_05, d_2018_06, d_2018_07, d_2018_08,
  d_2018_09, d_2018_10, d_2018_11, d_2018_12
))
d_2019 <- bind_rows(list(
  d_2019_01, d_2019_02, d_2019_03, d_2019_04,
  d_2019_05, d_2019_06, d_2019_07, d_2019_08,
  d_2019_09, d_2019_10, d_2019_11, d_2019_12
))
d_2020 <- bind_rows(list(
  d_2020_01, d_2020_02, d_2020_03, d_2020_04,
  d_2020_05, d_2020_06, d_2020_07, d_2020_08,
  d_2020_09, d_2020_10, d_2020_11, d_2020_12
))
d_2021 <- bind_rows(list(
  d_2021_01, d_2021_02, d_2021_03, d_2021_04,
  d_2021_05, d_2021_06, d_2021_07, d_2021_08,
  d_2021_09, d_2021_10, d_2021_11, d_2021_12
))
d_2022 <- bind_rows(list(
  d_2022_01, d_2022_02, d_2022_03, d_2022_04,
  d_2022_05, d_2022_06, d_2022_07, d_2022_08,
  d_2022_09, d_2022_10, d_2022_11, d_2022_12
))
saveRDS(d_2017, file = "data/d_2017.rds")
saveRDS(d_2018, file = "data/d_2018.rds")
saveRDS(d_2019, file = "data/d_2019.rds")
saveRDS(d_2020, file = "data/d_2020.rds")
saveRDS(d_2021, file = "data/d_2021.rds")
saveRDS(d_2022, file = "data/d_2022.rds")

# Read in data ------------------------------------------------------------

d_2017 <- readRDS("data/d_2017.rds")
d_2018 <- readRDS("data/d_2018.rds")
d_2019 <- readRDS("data/d_2019.rds")
d_2020 <- readRDS("data/d_2020.rds")
d_2021 <- readRDS("data/d_2021.rds")
d_2022 <- readRDS("data/d_2022.rds")
all_OD_data <- bind_rows(list(d_2017, d_2018, d_2019, d_2020, d_2021, d_2022))
saveRDS(all_OD_data, file = "data/all_OD_data.rds")

# Compute aggregates ------------------------------------------------------

all_OD_data <- readRDS("data/all_OD_data.rds")

plot_data <- all_OD_data |>
  mutate(duration = as.numeric(ymd_hms(ended_at) - ymd_hms(started_at))) |>
  select(Start.date, started_at, Duration, duration) |>
  mutate(
    Start.date = case_when(
      !is.na(Start.date) ~ Start.date,
      is.na(Start.date) ~ started_at
    )
  ) |>
  mutate(
    Duration = case_when(
      !is.na(Duration) ~ Duration,
      is.na(Duration) ~ duration
    )
  ) |>
  mutate(
    Date = ymd_hms(Start.date),
    Year = factor(year(Date)),
    Month = month(Date, label = TRUE),
    Hour = hour(Date)
  ) |>
  mutate(
    Duration =
      case_when(
        Duration < 120 ~ "Under 2 minutes",
        (Duration < 600) & (Duration >= 120) ~ "Between 2 and 10 minutes",
        (Duration < 1800) & (Duration >= 600) ~ "Between 10 and 30 minutes",
        Duration >= 1800 ~ "Over 30 minutes",
        TRUE ~ NA_character_
      )
  ) |>
  mutate(Duration = factor(Duration,
                           levels = c(
                             "Under 2 minutes",
                             "Between 2 and 10 minutes",
                             "Between 10 and 30 minutes",
                             "Over 30 minutes"
                           )
  )) |>
  select(Year, Month, Hour, Duration) |>
  group_by(Year, Month, Hour, Duration) |>
  summarise(n = n())

saveRDS(plot_data, file = "data/facet_plot_data.rds")

# Make plot ---------------------------------------------------------------

facet_plot_data <- readRDS("data/facet_plot_data.rds")

facet_plot_data <- facet_plot_data |>
  ungroup() |>
  mutate(Month = factor(Month))
class(facet_plot_data$Month) <- "factor"

# legend labels function
str_pad_custom <- function(labels){
  new_labels <- stringr::str_pad(labels, 30, "right")
  return(new_labels)
}

# start recording
gg_record(
  dir = file.path("recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 8.27, # width of saved image
  height = 11.69, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

g <- ggplot(
  data = facet_plot_data,
  mapping = aes(x = Hour, y = n, fill = Duration)
) +
  geom_col() +
  facet_grid(Month ~ Year)
g

g <- g +
  scale_fill_manual(name = "Journey time: ",
                    values = rev(nrBrand::nr_colours$contrast_seq[[1]][seq(1, 7, 2)]),
                    labels  = str_pad_custom) +
  scale_x_continuous(
    name = "", breaks = c(6, 18), labels = c("06:00", "18:00")) +
  scale_y_continuous(
    name = "", breaks = c(0, 25000, 50000), limits = c(0, 50000),
    labels = c("", "25K", "50K")) +
  guides(fill = guide_legend(nrow = 1)) +
  coord_cartesian(expand = FALSE)
g

st <- "**Capital Bikeshare** is a bike-sharing system servicing Washington D.C. and the surrounding areas, averaging over three millions rides per year. Most journeys are between 10 and 30 minutes, though an increasing number of very short journeys lasting under two minutes have been recorded. Though demand has returned to pre-pandemic levels, the pattern of demand has changed. The morning and evening rush hours peaks are no longer as clear cut, and demand is more evenly spread throughout the day."
social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#E30B5C;'>&#xf099;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>@nrennie35</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#E30B5C;'>&#xf4f6;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>fosstodon.org/@nrennie</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#E30B5C;'>&#xf09b;</span><span style='color:white;'>.</span><span style='font-family:Commissioner;color:#2F4F4F;'>nrennie</span><span style='color:white;'>..</span>"

g <- g +
  labs(
    title = "Back in the Saddle",
    subtitle = glue("Demand for bike-sharing services has changed throughout the Covid-19 pandemic with fewer rush hour rides.<br><br>{social}"),
    tag = st,
    caption = "**Source**: ride.capitalbikeshare.com/system-data<br>**Graphic**: Nicola Rennie"
  )

g +
  theme_minimal() +
  theme(
    text = element_text(
      family = "Commissioner",
      colour = "#2F4F4F"
    ),
    plot.title.position = "plot",
    plot.title = element_text(
      family = "Fraunces",
      size = 80,
      hjust = 0.5,
      colour = "#2F4F4F",
      margin = unit(c(0.7, 0, 0.5, 0), "cm")
    ),
    plot.subtitle = element_markdown(
      size = 32,
      lineheight = 0.5,
      hjust = 0.5,
      colour = "#2F4F4F",
      margin = unit(c(0, 0, 4, 0), "cm")
    ),
    plot.tag = element_textbox_simple(
      size = 30,
      lineheight = 0.5,
      hjust = 0,
      margin = margin(10, 0, 5, 0),
      width = grid::unit(7.4, "in"),
      colour = "#2F4F4F"
    ),
    plot.caption = element_markdown(
      size = 26,
      lineheight = 0.4,
      hjust = 0,
      margin = margin(15, 0, 5, 0),
      colour = "#2F4F4F"
    ),
    plot.tag.position = c(0.05, 0.81),
    plot.margin = margin(5, 5, 5, 5),
    axis.text = element_text(size = 24, colour = "#2F4F4F"),
    axis.title.y = element_text(size = 24, colour = "#2F4F4F"),
    strip.text = element_text(size = 24, lineheight = 0.4, colour = "#2F4F4F"),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.key.size = unit(0.5, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.justification = "left",
    legend.text = element_text(size = 24, lineheight = 0.4, colour = "#2F4F4F"),
    legend.title = element_text(size = 24, lineheight = 0.4, colour = "#2F4F4F"),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#F0F5F5", colour = "#F0F5F5"),
    panel.background = element_rect(fill = "#d8dcdc", colour = "#d8dcdc")
  )

# save gif
gg_playback(
  name = file.path("2023", "viz", "day_28.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "#F0F5F5"
)

ggsave("2023/viz/day_28.png", width = 8.27, height = 11.69, unit = "in")
