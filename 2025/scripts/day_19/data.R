
# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Data wrangling ----------------------------------------------------------

plot_data <- income |>
  dplyr::filter(Year >= 1980) |>
  dplyr::filter(
    stringr::str_detect(Country, "(WID)", negate = TRUE),
    Country != "World"
  ) |>
  dplyr::mutate(Country = stringr::str_remove(Country, " \\(country\\)")) |>
  dplyr::mutate(
    Continent = countrycode::countrycode(
      sourcevar = Country,
      origin = "country.name",
      destination = "continent"
    )
  ) |>
  dplyr::filter(Continent == "Europe") |>
  tidyr::drop_na() |>
  dplyr::rename(
    Income = `Income share of the richest 1% (before tax) (World Inequality Database)`)


# Write CSV ---------------------------------------------------------------

readr::write_csv(plot_data,
                 "2025/scripts/day_19/data.csv")
