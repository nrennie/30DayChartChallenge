
# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Data wrangling ----------------------------------------------------------

plot_data <- income |>
  dplyr::filter(
    stringr::str_detect(Country, "(WID)", negate = TRUE),
    Country != "World",
    Year %in% c(2015, 2022, 2023)
  ) |>
  dplyr::mutate(Country = stringr::str_remove(Country, " \\(country\\)")) |>
  dplyr::rename(
    Income = `Income share of the richest 1% (before tax) (World Inequality Database)`
  ) |>
  tidyr::pivot_wider(names_from = Year, values_from = Income) |>
  dplyr::mutate(
    ChangeA = `2023` - `2022`,
    ChangeB = `2023` - `2015`
  ) |>
  dplyr::rename(Income = `2023`) |>
  dplyr::select(-c(`2015`, `2022`))  |> 
  dplyr::mutate(
    Continent = countrycode::countrycode(
      sourcevar = Country,
      origin = "country.name",
      destination = "continent"
    )
  ) |>
  tidyr::drop_na() |> 
  dplyr::mutate(
    Continent = factor(Continent),
    Continent = as.numeric(Continent)
  )


# Write CSV ---------------------------------------------------------------

readr::write_csv(plot_data, "2025/scripts/day_26/data.csv")
