# Day 1 - Fractions -------------------------------------------------------

quarto::quarto_render("2025/code/01_fractions/index.qmd")
httpuv::runStaticServer(
  dir = "2025/code/01_fractions/",
  port = 4000,
  background = TRUE,
  browse = FALSE
)
webshot2::webshot(
  url = "http://127.0.0.1:4000/",
  file = "2025/viz/01_fractions.png",
  selector = ".cell-output.cell-output-display"
)
httpuv::stopAllServers()
