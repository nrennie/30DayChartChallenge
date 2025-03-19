library(tidyverse)
library(gapminder)
library(showtext)

# add fonts
font_add_google(name = "Bungee", family = "bungee")
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# get and prep data
plot_data <- gapminder %>%
  select(country, year, lifeExp)

min(plot_data$year)

# plot
p <- ggplot(data = plot_data,
       mapping = aes(x = year, y = lifeExp)) +
  geom_area(fill = "#710193") +
  coord_cartesian(expand = F) +
  facet_wrap(~country, ncol = 24, labeller = labeller(country = label_wrap_gen(20))) +
  scale_y_continuous(breaks = c(0, 85),
                     limits = c(0, 85)) +
  scale_x_continuous(breaks = c(1960, 2000)) +
  labs(title = "Gapminder: Life Expectancy",
       subtitle = "N. Rennie | Data: {gapminder} | #30DayChartChallenge",
       x = "",
       y = "") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#019371", colour = "#019371"),
        plot.background = element_rect(fill = "black", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(colour = "white"),
        panel.spacing = unit(0.2, "lines"),
        strip.background = element_rect(fill = "black", colour = "black"),
        strip.text = element_text(family = "ubuntu", colour = "white", size = 11, lineheight = 0.5),
        axis.text = element_text(family = "ubuntu", colour = "white", size = 18),
        plot.title = element_text(family = "ubuntu", colour = "white",
                                  size = 40, hjust = 0.5, face = "bold",
                                  margin = margin(t = 20, b = 20)),
        plot.subtitle = element_text(family = "ubuntu", colour = "white",
                                     size = 24, hjust = 0.5,
                                     margin = margin(b = 20)),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
p

ggsave(p, filename = "2022/viz/day_23.jpg", height = 7, width = 14, unit = "in")
