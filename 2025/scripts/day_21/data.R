
# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")
energy <- readr::read_csv("2025/data/energy-consumption-by-source-and-country.csv")[,1:12]
# https://ourworldindata.org/energy/country/india


# Data wrangling ----------------------------------------------------------

energy_data <- energy |>
  dplyr::select(-c(Entity, Code)) |>
  tidyr::pivot_longer(-Year) |>
  dplyr::mutate(
    name = dplyr::if_else(
      stringr::str_detect(name, "Oil|Gas|Coal"),
      "Fossil Fuels",
      "Other"
    )
  ) |>
  dplyr::group_by(Year, name) |>
  dplyr::summarise(value = sum(value, na.rm = T)) |>
  dplyr::ungroup() |>
  dplyr::group_by(Year) |>
  dplyr::mutate(year_value = sum(value)) |>
  dplyr::mutate(prop = 100 * value / year_value) |>
  dplyr::ungroup() |>
  dplyr::filter(name == "Fossil Fuels") |>
  dplyr::select(Year, Fossil = prop)

plot_data <- income |>
  dplyr::filter(Year >= 1965, Year <= 1995) |>
  dplyr::filter(
    Country == "India"
  ) |>
  dplyr::rename(
    Income = `Income share of the richest 1% (before tax) (World Inequality Database)`) |>
  dplyr::select(-Country) |>
  dplyr::left_join(energy_data, by = "Year")


# Write CSV ---------------------------------------------------------------

readr::write_csv(plot_data, "2025/scripts/day_21/data.csv")
