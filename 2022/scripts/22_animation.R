library(gganimate)
library(ggplot2)
library(dplyr)
library(showtext)

# add fonts
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# make data
plot_data <- expand.grid(x = 0:255,
                         y = 0:255,
                         z = 0:255) %>%
  mutate(colour = rgb(x, y, z, maxColorValue = 255))

# plot
p <- ggplot(data = plot_data,
       mapping = aes(x = x, y = y, colour = I(colour))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 255), breaks = c(0, 255)) +
  scale_y_continuous(limits = c(0, 255), breaks = c(0, 255)) +
  labs(x = "Red",
       y = "Green",
       subtitle = "Blue: {frame_time}",
       title = "RGB Colours",
       caption = "N. Rennie | #30DayChartChallenge") +
  coord_fixed() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        plot.title = element_text(family = "ubuntu", hjust = 0.5, face = "bold", size = 22, color = "black",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0.5,
                                     size = 14, color = "black"),
        plot.caption = element_text(family = "ubuntu", hjust = 0.5,
                                    size = 10, color = "black",
                                    margin = margin(t = 10)),
        axis.text = element_text(family = "ubuntu", hjust = 0,
                                 size = 10, color = "black"),
        axis.title.x = element_text(family = "ubuntu", hjust = 0.5,
                                    size = 14, color = "black",
                                    margin = margin(t = 10)),
        axis.title.y = element_text(family = "ubuntu", hjust = 0.5,
                                    size = 14, color = "black",
                                    margin = margin(r = 10)))

# animate
anim <- animate(p, width=715, height=715, duration = 10, nframes = 255)
anim_save(filename="2022/viz/day_22.gif", anim)
### too large :(

### create a function instead and map
plot_function <- function(z_i) {
  p_data <- plot_data %>%
    filter(z == z_i)
  p <- ggplot(data = p_data,
              mapping = aes(x = x, y = y, colour = I(colour))) +
    geom_point() +
    scale_x_continuous(limits = c(0, 255), breaks = c(0, 255)) +
    scale_y_continuous(limits = c(0, 255), breaks = c(0, 255)) +
    labs(x = "Red",
         y = "Green",
         subtitle = paste0("Blue: ", z_i),
         title = "RGB Colours",
         caption = "N. Rennie | #30DayChartChallenge") +
    coord_fixed(expand = F) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
          plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
          plot.margin = unit(c(0, 0.3, 0.3, 0), "cm"),
          plot.title = element_text(family = "ubuntu", hjust = 0.5, face = "bold", size = 22, color = "black",
                                    margin = margin(t = 10, r = 0, b = 10, l = 0)),
          plot.subtitle = element_text(family = "ubuntu", hjust = 0.5,
                                       size = 14, color = "black"),
          plot.caption = element_text(family = "ubuntu", hjust = 0.5,
                                      size = 10, color = "black",
                                      margin = margin(t = 5)),
          axis.text = element_text(family = "ubuntu", hjust = 0,
                                   size = 10, color = "black"),
          axis.title.x = element_text(family = "ubuntu", hjust = 0.5,
                                      size = 14, color = "black"),
          axis.title.y = element_text(family = "ubuntu", hjust = 0.5,
                                      size = 14, color = "black"))
  ggsave(p, filename = paste0("2022/viz/day_22/", z_i, ".jpg"), height = 715, width = 600, unit = "px")
  print(z_i)
}

#map
purrr::map(.x = 1:255, .f = ~plot_function(.x))

#use ezgif to stitch together
