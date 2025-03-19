library(readxl)
library(tidyverse)
library(gt)
library(showtext)
library(usefunc)
library(forcats)
library(lubridate)

# data
df <- tibble(read_xlsx("2022/data/london.xlsx"))

# load fonts
font_add_google(name = "Monoton", family = "monoton")
font_add_google(name = "Poiret One", family = "poiret")
showtext_auto()

# prep data
plot_data <- df %>%
  mutate(Labelx = Time + minutes(1),
         Col = (plot_data$Nationality == plot_data$Nationality[1]),
         Year = fct_rev(factor(Year)))

# set subtitle
tag = str_wrap_break("The London Marathon began in 1981, and 27 different women have won. The race has been won by a British runner 7 times. In 2003, Paula Radcliffe won in a time of 2:15:25, a world record which would stand for 16 years.\n\nN. Rennie | Data: Virgin Money London Marathon", 100)

# plot
p <- ggplot(data = plot_data) +
  geom_segment(aes(x = ymd_hms("1899-12-31 02:05:00"),
                   xend = Time,
                   y = Year, yend = Year),
               colour = alpha("#012169", 0.3)) +
  geom_point(aes(x = Time, y = Year, colour = as.factor(Col)),
             size = 4) +
  geom_text(aes(x = Labelx, y = Year, label = Athlete),
            hjust = 0,
            colour = "#012169",
            family = "poiret",
            size = 10) +
  scale_colour_manual(values = c("#012169", "#C8102E")) +
  scale_x_datetime(limits = c(ymd_hms("1899-12-31 02:05:00"), ymd_hms("1899-12-31 02:40:00")),
                   breaks = c(ymd_hms("1899-12-31 02:15:00"),
                              ymd_hms("1899-12-31 02:30:00")),
                   labels = c("02:15:00", "02:30:00")) +
  labs(x = "", y = "",
       title = "LONDON    MARATHON",
       subtitle = "Womens' Winning Times 1981 - 2021\n",
       tag = tag) +
  theme(plot.title = element_text(colour = "#C8102E", family = "monoton", size = 100, hjust = -0.2),
        plot.subtitle = element_text(colour = "#C8102E", family = "poiret", size = 60, hjust = -0.11,
                                     margin = margin(0,0, 20, 0)),
        plot.tag = element_text(colour = "#C8102E", family = "poiret", size = 36, hjust = 0, lineheight = 0.35),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.y = element_text(colour = "#C8102E", family = "poiret", size = 30, hjust = 1),
        axis.text.x = element_text(colour = "#C8102E", family = "poiret", size = 30),
        plot.tag.position = c(0.04, 0.88),
        plot.margin = unit(c(0.8, 0.8, 0.5, 0.5), "cm"))

# save as a4 image
ggsave(p, filename = "2022/viz/day_07.jpg", height = 11.75, width = 8.25, unit = "in")
