
# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Data wrangling ----------------------------------------------------------

plot_data <- income |>
  dplyr::filter(
    Country %in% c("Australia", "World"),
    Year >= 1980,
    Year <= 2015
  ) |>
  dplyr::rename(Income = `Income share of the richest 1% (before tax) (World Inequality Database)`) |>
  tidyr::pivot_wider(
    names_from = Country,
    values_from = Income
  )


# Write CSV ---------------------------------------------------------------

readr::write_csv(plot_data, "2025/scripts/day_18/data.csv")
