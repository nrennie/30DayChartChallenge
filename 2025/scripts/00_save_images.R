# Update data -------------------------------------------------------------

source("data/make-data.R")


# Camcorder ---------------------------------------------------------------

library(camcorder)
gg_record(
  dir = file.path("2025", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Function to save a PNG --------------------------------------------------

save_js_png <- function(day,
                        ojs = TRUE,
                        selector = ".cell-output.cell-output-display", ...) {
  if (ojs) {
    quarto::quarto_render(glue::glue("2025/scripts/day_{day}/index.qmd"))
  }
  httpuv::runStaticServer(
    dir = glue::glue("2025/scripts/day_{day}/"),
    port = 4000,
    background = TRUE,
    browse = FALSE
  )
  webshot2::webshot(
    url = "http://127.0.0.1:4000/",
    file = glue::glue("2025/viz/day_{day}.png"),
    selector = selector,
    ...
  )
  httpuv::stopAllServers()
}


# Day 1 - Fractions -------------------------------------------------------

save_js_png("01")


# Day 2 - Slope -----------------------------------------------------------

save_js_png("02", vwidth = 1000, vheight = 1600)


# Day 3 - Circular --------------------------------------------------------

save_js_png("03", vwidth = 1000, vheight = 1000)


# Day 7 - Outliers --------------------------------------------------------

save_js_png("07", vwidth = 1000, vheight = 1000)


# Day 8 - Histogram -------------------------------------------------------

save_js_png("08", vwidth = 1000, vheight = 1000, expand = c(10, 10, 0, 10))


# Day 9 - Diverging -------------------------------------------------------

save_js_png("09", vwidth = 1000, vheight = 1600, expand = c(10, 0, 10, 10))


# Day 18 - El Pais (Theme) ------------------------------------------------

save_js_png("18",
  ojs = FALSE,
  selector = "svg",
  vwidth = 1000, vheight = 1600,
  expand = c(5, 0, 10, 10)
)


# Day 19 - Smooth ---------------------------------------------------------

save_js_png("19",
  ojs = FALSE,
  selector = "svg",
  vwidth = 1000, vheight = 1600,
  expand = c(5, 0, 10, 10)
)


# Day 21 - Fossils --------------------------------------------------------

save_js_png("21",
  ojs = FALSE,
  selector = "svg",
  vwidth = 1000, vheight = 1600,
  expand = c(5, 0, 10, 10)
)


# Day 26 - Monochrome -----------------------------------------------------

save_js_png("26",
  ojs = FALSE,
  selector = "svg",
  vwidth = 1000, vheight = 1600
)
