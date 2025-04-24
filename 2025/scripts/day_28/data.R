# Load data ---------------------------------------------------------------

income <- readr::read_csv("2025/data/income-share-of-the-richest-1.csv")


# Data wrangling ----------------------------------------------------------

income_world <- income |>
  dplyr::filter(Country == "World") |>
  dplyr::slice_max(Year, n = 1) |>
  dplyr::rename(Income = `Income share of the richest 1% (before tax) (World Inequality Database)`) |>
  dplyr::mutate(Income = round(Income))

plot_data <- data.frame(
  type = c("People", "Wealth"),
  Yes = c(1, income_world$Income)
) |>
  dplyr::mutate(No = 100 - Yes) |>
  tidyr::pivot_longer(
    -type,
    values_to = "n",
    names_to = "YN"
  ) |>
  tidyr::uncount(weights = n) |>
  dplyr::mutate(
    x = rep(rep(1:10, each = 10), 2) +
      c(rep(0, times = 100), rep(12, times = 100)),
    y = rep(rep(1:10, times = 10), 2)
  ) |>
  dplyr::mutate(
    colour = dplyr::case_when(
      YN == "Yes" ~ "#83347A",
      YN == "No" ~ "#8BA6A9"
    )
  ) |>
  dplyr::mutate(
      icon = dplyr::case_when(
      type == "People" ~ "person-solid.svg",
      type == "Wealth" ~ "sack-dollar-solid.svg"
    )
  )



# Write CSV ---------------------------------------------------------------

readr::write_csv(plot_data, "2025/scripts/day_28/data.csv")
