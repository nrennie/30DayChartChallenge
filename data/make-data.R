# utils function
str_extract_between <- function(x, start, end) {
  pattern <- paste0("(?<=", start, ")(.*?)(?=", end, ")")
  return(stringr::str_extract(x, pattern = pattern))
}

# Params
years <- c(2022, 2023, 2024)

# Function
get_readme <- function(year) {
  yr_readme <- readLines(glue::glue("{year}/README.md")) |>
    data.frame(raw_char = _) |>
    tibble::as_tibble()

  yr_output <- yr_readme |>
    dplyr::filter(stringr::str_detect(raw_char, "##"),
                  stringr::str_detect(raw_char, glue::glue("{year}"), negate = TRUE)) |>
    dplyr::mutate(
      year = year,
      prompt = str_extract_between(raw_char, "## ", " in "),
      number = stringr::str_pad(readr::parse_number(prompt), 2, pad = "0"),
      tool = stringr::str_match(raw_char, ".*in\\s*(.*)$")[,2],
      tool = stringr::str_remove_all(tool, "\\[.*?\\]|\\(.*?\\)|\\{.*?\\}"),
      file_ext = dplyr::if_else(year == 2022, ".jpg", ".png"),
      image_url = glue::glue("https://raw.githubusercontent.com/nrennie/30DayChartChallenge/refs/heads/main/{year}/viz/day_{number}{file_ext}"),
      code_url = ""
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
writexl::write_xlsx(all_data, "data/all_data.xlsx")
save(all_data, file = "data/all_data.RData")
