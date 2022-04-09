library(readxl)
library(tidyverse)
library(showtext)
library(usefunc)
library(patchwork)

# data https://www.gov.uk/government/statistics/fuel-poverty-trends-2022
df <- tibble(read_xlsx("2022/data/fuel.xlsx"))

# load fonts
font_add_google(name = "Bowlby One SC", family = "bowlby")
font_add_google(name = "Poiret One", family = "poiret")
showtext_auto()

# plot
p1 <- ggplot(data = df,
       mapping = aes(x = Year, y = `Proportion of total fuel poor households (%)`,
                     fill = Tenure)) +
  geom_area() +
  scale_fill_manual("", values = c("#ffb997", "#f67e7d", "#843b62", "#621940")) +
  scale_x_continuous(breaks = c(2010, 2015, 2020),
                     labels = c(2010, 2015, 2020)) +
  theme_minimal() +
  labs(x = "", y = str_wrap("Proportion of total fuel poor households (%)", 30)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#0b032d", colour = "#0b032d"),
        plot.background = element_rect(fill = "#0b032d", colour = "#0b032d"),
        axis.text = element_text(colour = '#ffb997', family="poiret", size=9),
        axis.title.y = element_text(colour = '#ffb997', family="poiret", size=12, margin = margin(r = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
p1

p2 <- ggplot(data = df,
       mapping = aes(x = Year, y = `Proportion of fuel poor households within group (%)`,
                     fill = Tenure)) +
  geom_area() +
  facet_wrap(~Tenure, nrow = 1) +
  scale_fill_manual("", values = c("#ffb997", "#f67e7d", "#843b62", "#621940")) +
  scale_x_continuous(breaks = c(2010, 2015, 2020),
                     labels = c(2010, 2015, 2020)) +
  labs(x = "", y = str_wrap("Proportion of fuel poor households within group (%)", 30)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#0b032d", colour = "#0b032d"),
        plot.background = element_rect(fill = "#0b032d", colour = "#0b032d"),
        strip.background =element_rect(fill="#0b032d", colour ="#0b032d"),
        strip.text = element_text(colour = '#ffb997', family="poiret", size=14),
        axis.text = element_text(colour = '#ffb997', family="poiret", size=9),
        axis.title.y = element_text(colour = '#ffb997', family="poiret", size=12, margin = margin(r = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
p2

# subtitle
st <- str_wrap_break("Although the percentage of UK households experiencing fuel poverty decreased between 2010 and 2020, not all types of properties are equal. Those in private rented accommodation are more than 3 times as likely to experience fuel poverty. Of those households who do experience fuel poverty, an increasing number of them are living in privately rented homes.\n\nN. Rennie | Data: www.gov.uk | #30DayChartChallenge", 54)

p3 <- ggplot() +
  annotate("text", x = 0, y = 0, label = st,
           colour = '#ffb997', family="poiret", size=4, hjust = 0) +
  xlim(0, 4) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#0b032d", colour = "#0b032d"),
        plot.background = element_rect(fill = "#0b032d", colour = "#0b032d"),
        axis.text = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p3

# join plots
p <- ((p1 + p3) / p2) +
  plot_annotation(title = "Fuel Poverty in the United Kingdom") &
  theme(plot.title = element_text(colour = '#ffb997', family="bowlby", size=24,
                                  margin = margin(b = 10)),
        panel.background = element_rect(fill = "#0b032d", colour = "#0b032d"),
        plot.background = element_rect(fill = "#0b032d", colour = "#0b032d"),
        plot.margin = unit(c(0.8, 0.5, 0.2, 0.5), "cm"))
p








