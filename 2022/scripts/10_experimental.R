library(tidyverse)
library(rcartocolor)

# https://www.angio.net/pi/digits/1000.txt
df <- readr::read_csv("2022/data/pi.txt", col_names = F, col_types = "c")
pi_char <- df %>%
  pull(X1) %>%
  str_split(pattern =  "") %>%
  unlist()
pi_char <- pi_char[-2]
theta <- seq(11, 40 * pi, length.out = 1000)
r <- 0.5 + 0.5 * theta
plot_data <- tibble(x = r * cos(theta),
                    y = r * sin(theta),
                    colour = pi_char)

# plot
p <- ggplot(data = plot_data,
       mapping = aes(x = x, y = y, colour = colour, size = theta)) +
  geom_point() +
  annotate("text", x = 0, y = 0, label = "\U03C0",
           colour = "white", size = 8, fontface = "bold.italic") +
  scale_color_carto_d(palette = "Bold") +
  scale_size(range = c(0.05, 3)) +
  coord_fixed() +
  xlim(-(max(abs(c(plot_data$x, plot_data$y))) + 2), (max(abs(c(plot_data$x, plot_data$y))) + 2)) +
  ylim(-(max(abs(c(plot_data$x, plot_data$y))) + 2), (max(abs(c(plot_data$x, plot_data$y))) + 2)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = "black", colour = "black"))
p

ggsave(p, filename = "2022/viz/day_10.jpg", height = 7, width = 7, unit = "in")
