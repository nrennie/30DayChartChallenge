library(tidyverse)
library(ggridges)
library(usefunc)
library(grid)
library(cowplot)
library(showtext)

# load fonts
font_add_google(name = "Roboto", family = "roboto")
showtext_auto()

# get data
df <- read_csv("2022/data/working_hours.csv")

# prep data
plot_data <- df %>%
  mutate(decade = (Year - Year %% 10 ))

label_data <- plot_data %>%
  group_by(decade) %>%
  summarise(avg = round(mean(`Average annual working hours per worker`)))

# plot
p <- ggplot(data = plot_data,
       mapping = aes(x = `Average annual working hours per worker`,
                     y = factor(decade))) +
  geom_density_ridges(fill = "#fea9a3", colour = "#990f3d", size = 0.9) +
  scale_y_discrete(limits=rev) +
  labs(caption = "N. Rennie | Data: Our World in Data | #30DayChartChallenge",
       title = "Average working hours have decreased since 1870",
       subtitle = str_wrap_break("However, in recent years the range of hours i.e. the gap between those who work the most and those who work the least seems to be widening.", 80),
       y = "") +
  geom_text(data = filter(label_data, decade <= 1910),
            mapping = aes(x = 1200, y = factor(decade), label = paste0("Avg: ", avg, " hrs")),
            vjust = -1, colour = "#990f3d", family="roboto", hjust = 0) +
  geom_text(data = filter(label_data, decade > 1910),
            mapping = aes(x = 3400, y = factor(decade), label = paste0("Avg: ", avg, " hrs")),
            vjust = -1, colour = "#990f3d", family="roboto") +
  scale_x_continuous(expand = c(0, 0)) +
  theme(plot.background = element_rect(fill = "#fff1e5", colour="#fff1e5"),
        panel.background = element_rect(fill = "#fff1e5", colour="#fff1e5"),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.2, 0.7, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "black", size=12,
                                 hjust = 0.5, family="roboto"),
        axis.text.y = element_text(size=12, margin = margin(r = 5), family="roboto"),
        axis.text.x = element_text(hjust = 0, margin = margin(t = 5), family="roboto"),
        plot.title = element_text(colour = "black", size=16, face = "bold",
                                  hjust = 0, family="roboto",
                                  margin = margin(10, 0, 10, 0)),
        plot.subtitle = element_text(colour = "black", size=12, hjust = 0,
                                     family="roboto",
                                     margin = margin(0, 0, 10, 0)),
        plot.caption = element_text(colour = "black", size=10, hjust = 0,
                                     family="roboto",
                                     margin = margin(5, 0, 0, 0)),
        axis.title.x = element_text(colour = "black", size=12, hjust = 0.5,
                                    family="roboto",
                                    margin = margin(10, 0, 5, 0))
  )
p

# black box
rect <- rectGrob(
  x = unit(0.75, "in"),
  y = unit(1, "npc"),
  width = unit(1, "in"),
  height = unit(0.05, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(col = "black", fill = "black", alpha = 1)
)

ggdraw(p) +
  draw_grob(rect)

