library(tidyverse)
library(showtext)
library(readxl)
library(lubridate)

# load fonts
font_add_google(name = "Red Rose", family = "rose")
showtext_auto()

# read data
df <- read_xlsx("2022/data/morecambe.xlsx")

plot_data <- df %>%
  mutate(time = ymd_hms(`Time (GMT)`)) %>%
  select(time, `Time (GMT)`, `Max Wave Height (m)`, `Wave Height (m)`)

# plot
ggplot(data = plot_data) +
  geom_area(mapping = aes(x = time,
                          y = `Max Wave Height (m)`),
            fill = "#006994") +
  geom_area(mapping = aes(x = time,
                          y = `Wave Height (m)`),
            fill = "#44A7C4") +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(limits = c(0, 2),
                     breaks = c(0, 0.5, 1, 1.5),
                     position = "right") +
  scale_x_datetime(breaks = c(ymd_hms("2022-04-18 06:00:00"),
                              ymd_hms("2022-04-18 12:00:00"),
                              ymd_hms("2022-04-18 18:00:00")),
                   labels = c("06:00", "12:00", "18:00")) +
  labs(x = "",
       y = "",
       title = "  24 Hours at Morecambe Bay",
       subtitle = "      Wave height and maximal wave height are shown in\n      metres for a 24 hour period at Morecambe Bay.\n      Data for 18/04/2022.\n\n      N. Rennie | Data: coastalmonitoring.org\n      #30DayChartChallenge") +
  theme(plot.background = element_rect(fill = "gray90", colour="gray90"),
        panel.background = element_rect(fill = "gray90", colour="gray90"),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-3, 0, 0, 0), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "black", size=12, hjust = 0.5, family="rose"),
        axis.text.y.right = element_text(vjust = -0.5, size=12,
                                         margin = margin(l = -42), face = "italic"),
        axis.text.x = element_text(hjust = 0, margin = margin(t = -26)),
        plot.title = element_text(colour = "black", size=27, face = "bold", hjust = 0, vjust = -15, family="rose", margin = margin(10, 0, 20, 20)),
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0, vjust = -35, family="rose", margin = margin(10, 0, 20, 20))
  )


