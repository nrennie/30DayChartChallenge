library(tidyverse)
library(lubridate)
library(showtext)
library(usefunc)
library(patchwork)

# add fonts
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# read and prep data
df <- as_tibble(read.table("2022/data/hadcet.txt", header = F))
colnames(df) <- c("year",
                  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                  "avg")
df <- df %>%
  select(year, avg) %>%
  filter(year >= 1880,
         avg > -50)

df_min <- as_tibble(read.table("2022/data/hadcet_min.txt", header = F))
colnames(df_min) <- c("year",
                  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                  "avg_min")
df_min <- df_min %>%
  select(year, avg_min) %>%
  filter(year >= 1880,
         avg_min > -50)

df_max <- as_tibble(read.table("2022/data/hadcet_max.txt", header = F))
colnames(df_max) <- c("year",
                      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                      "avg_max")
df_max <- df_max %>%
  select(year, avg_max) %>%
  filter(year >= 1880,
         avg_max > -50)


# prep data
plot_data <- df %>%
  left_join(df_max, by = "year") %>%
  left_join(df_min, by = "year")

# subtitle
st <- str_wrap_break("Hadley Centre Central England Temperature (HadCET) dataset is the longest instrumental record of temperature in the world. The temperatures are representative of a roughly triangular area of the United Kingdom enclosed by Lancashire, London and Bristol. Temperatures are shown here from 1880 - 2021. The outer and inner rings represents the average maximum and minimum temperatures.", 80)

# plot
ggplot() +
  geom_ribbon(data = plot_data,
              mapping = aes(x = year, ymin = avg_min, ymax = avg_max),
              alpha = 0.3,
              colour = "lightgrey") +
  geom_line(data = plot_data,
            mapping = aes(x = year, y = avg, colour = avg),
            size = 2) +
  geom_line(data = plot_data,
            mapping = aes(x = year, y = avg_min)) +
  geom_line(data = plot_data,
            mapping = aes(x = year, y = avg_max)) +
  labs(x = "",
       y = "Temperature (°C)",
       subtitle = st,
       title = "Annual Temperature",
       caption = "N. Rennie | Data: Met Office (HadCET) | #30DayChartChallenge") +
  coord_polar() +
  scale_colour_distiller(name = "Temperature (°C)",
                         type = "seq",
                         palette = "OrRd",
                         direction = 1) +
  guides(colour = guide_colorbar(title.position = "top",
                               ticks.colour = "gray90")) +
  scale_x_continuous(breaks = c(1882, 1915, 1950, 1985, 2018),
                     labels = c(1880, 1915, 1950, 1985, 2020)) +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray90", colour="gray90"),
        panel.background = element_rect(fill = "gray90", colour="gray90"),
        plot.margin = unit(c(0.5, 2, 0.5, 2), "cm"),
        plot.title = element_text(family = "ubuntu", hjust = 0.5, face = "bold", size = 22, color = "black",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0.5,
                                     size = 12, color = "black"),
        plot.caption = element_text(family = "ubuntu", hjust = 0.5,
                                    size = 10, color = "black",
                                    margin = margin(t = 10)),
        legend.position = "bottom",
        legend.background = element_rect(fill = "gray90", colour="gray90"),
        legend.text = element_text(family = "ubuntu", hjust = 0.5,
                                    size = 10, color = "black"),
        legend.title = element_text(family = "ubuntu", hjust = 0.5,
                                    size = 10, color = "black"),
        axis.text.x = element_text(family = "ubuntu", hjust = 0.5,
                                    size = 10, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

