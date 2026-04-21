library(tidyverse)
library(countrycode)
library(glue)

# https://ourworldindata.org/grapher/co-emissions-per-capita
emissions_raw <- read_csv("2026/data/co-emissions-per-capita.csv")

# https://blogs.worldbank.org/en/opendata/world-bank-country-classifications-by-income-level-for-2024-2025
income_raw <- read_csv("2026/data/income.csv") |>
  select(Code, Income) |>
  mutate(Income = str_remove(Income, " countries"),
         Income = str_replace_all(Income, "-", " "))

emissions <- emissions_raw |>
  filter(Year >= 2000,
         str_starts(Code, "OWID_", negate = TRUE)) |>
  drop_na(Code) |>
  left_join(income_raw, "Code") |>
  rename(Emissions = `CO₂ emissions per capita`,
         Country = Entity) |>
  select(Country, Code, Income, Emissions, Year) |>
  pivot_wider(names_from = Year, values_from = Emissions) |>
  mutate(`% change` = 100 * (`2024` - `2000`) / `2000`, .after = 1) |>
  mutate(Code = countrycode(Code, origin = "iso3c", destination = "iso2c"),
         Code = paste0(":", str_to_lower(Code), ":")) |>
  mutate(Country = if_else(is.na(Income), Country, glue("{Country} ^{Income}^"))) |>
  select(-Income) |>
  mutate(`CO₂ emissions per capita` = `2024`, .after = 2)

write_csv(emissions, "2026/data/day_22.csv")









