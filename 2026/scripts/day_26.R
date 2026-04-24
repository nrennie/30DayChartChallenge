library(tidyverse)
library(gapminder)
library(ggauto)
gapminder |>
  filter(continent == "Americas") |>
  mutate(year = ymd(paste0(year, "/01/01"))) |>
  ggauto(year, lifeExp, country,
    title = "Life expectancy increased in the Americas",
    subtitle = "Life expectancy at birth. 1952 to 2007.",
    caption = "**Source**: Gapminder | **Graphic**: N. Rennie",
    ylab = "Years"
  )

ggsave("2026/viz/day_26.png", width = 7, height = 8)
