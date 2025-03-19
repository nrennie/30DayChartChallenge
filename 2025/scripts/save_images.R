
# Function to save a PNG --------------------------------------------------

save_ojs_png <- function(day) {
  quarto::quarto_render(glue::glue("2025/scripts/day_{day}/index.qmd"))
  httpuv::runStaticServer(
    dir = glue::glue("2025/scripts/day_{day}/"),
    port = 4000,
    background = TRUE,
    browse = FALSE
  )
  webshot2::webshot(
    url = "http://127.0.0.1:4000/",
    file = glue::glue("2025/viz/day_{day}.png"),
    selector = ".cell-output.cell-output-display"
  )
  httpuv::stopAllServers()
}


# Day 1 - Fractions -------------------------------------------------------

save_ojs_png("01")


# Day 2 - Slopes ----------------------------------------------------------

save_ojs_png("02")
