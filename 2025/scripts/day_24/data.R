library(tidyverse)

# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")
who <- readxl::read_xlsx("2025/data/who.xlsx") #https://www.who.int/data/inequality-monitor/data


# Data wrangling ----------------------------------------------------------

who_data <- who |>
  filter(
    indicator_name == "Population with basic handwashing facilities at home (%)",
    setting == "Mexico",
    subgroup == "Rural"
  ) |>
  select(date, estimate) |>
  rename(Handwashing = estimate) |>
  mutate(Handwashing = 100 - Handwashing)

plot_data <- income |>
  filter(
    Country == "Mexico"
  ) |>
  rename(
    Income = `Income share of the richest 1% (before tax) (World Inequality Database)`) |>
  select(-Country) |>
  left_join(who_data, by = c("Year" = "date")) |>
  drop_na() |>
  filter(Year < 2022)


# Write CSV ---------------------------------------------------------------

readr::write_csv(plot_data, "2025/scripts/day_24/data.csv")
