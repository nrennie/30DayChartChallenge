library(tidyverse)
library(usefunc)
library(showtext)
library(emojifont)

# load fonts
font_add_google(name = "Knewave", family = "knewave")
font_add_google(name = "Mada", family = "mada")
showtext_auto()

# write data
# https://www.cancerresearchuk.org/health-professional/cancer-statistics/statistics-by-cancer-type/breast-cancer
total <- 700 - 100
all <- 100 - 23
prev <- 23

# prep data
st <- str_wrap_break("Breast cancer is the most common cancer in the UK, accounting for 15% of all new cancer cases (2016-2018).\n\n1 in 7 UK females will be diagnosed with breast cancer in their lifetime. 23% of cases in the UK are preventable.\n\nN. Rennie | Data: Cancer Research UK", 50)

plot_data <- tibble(expand.grid(x = seq(from = 1, by = 0.5, length.out = 20),
                                y = rep(1:35))) %>%
  mutate(type = c(rep("#ff405d", prev),
                  rep("#ff8fa0", all),
                  rep("#ffcad2", total)))

# plot
p <- ggplot() +
  geom_text(data = plot_data,
            mapping = aes(x = x,
                          y = y,
                          label = fontawesome('fa-female'),
                          colour = I(type)),
            family='fontawesome-webfont', size = 12) +
  annotate("text", x = 15.5, y = 13, label = "Breast Cancer\n\nin the UK",
           hjust = 0.5, colour = "#ff405d", size = 22,
           family = "knewave", lineheight = 0.2) +
  annotate("text", x = 15.5, y = 19, label = st,
           hjust = 0.5, colour = "#ff405d", size = 8,
           family = "mada", lineheight = 0.5) +
  coord_fixed(expand = FALSE) +
  xlim(0.5, 20) +
  scale_y_reverse(limits = c(36, 0)) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0, 0, -0.2), "cm"),
        panel.background = element_rect(fill = "#fff1f3", colour = "#fff1f3"),
        plot.background = element_rect(fill = "#fff1f3", colour = "#fff1f3"))

ggsave(p, filename = "2022/viz/day_02.jpg", unit = "in", height = 8.6, width = 5)
