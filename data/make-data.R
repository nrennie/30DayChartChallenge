# utils function
str_extract_between <- function(x, start, end) {
  pattern <- paste0("(?<=", start, ")(.*?)(?=", end, ")")
  return(stringr::str_extract(x, pattern = pattern))
}

# Params
years <- c(2022, 2023, 2024, 2025)

# Function
get_readme <- function(year) {
  yr_readme <- readLines(glue::glue("{year}/README.md")) |>
    data.frame(raw_char = _) |>
    tibble::as_tibble()

  yr_output <- yr_readme |>
    dplyr::filter(stringr::str_detect(raw_char, "##")) |>
    dplyr::mutate(
      year = year,
      prompt = str_extract_between(raw_char, "## ", " made with "),
      number = stringr::str_pad(readr::parse_number(prompt), 2, pad = "0"),
      tool = stringr::str_match(raw_char, ".*made with\\s*(.*)$")[,2],
      tool = stringr::str_remove_all(tool, "\\[.*?\\]|\\(.*?\\)|\\{.*?\\}"),
      image_url = glue::glue("https://raw.githubusercontent.com/nrennie/30DayChartChallenge/refs/heads/main/{year}/viz/day_{number}"),
      file_ext = dplyr::if_else(
        file.exists(glue::glue("{year}/viz/day_{number}.jpg")), ".jpg", ".png"
      ),
      image_url = glue::glue("{image_url}{file_ext}")
    ) |>
    dplyr::select(-c(raw_char, file_ext))
  return(yr_output)
  }

# Get data
all_data <- purrr::map_df(
  .x = years,
  .f = ~get_readme(.x)
)

# Save file
readr::write_csv(all_data, "data/all_data.csv")
save(all_data, file = "data/all_data.RData")
