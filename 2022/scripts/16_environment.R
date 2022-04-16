library(tidyverse)
library(magrittr)
library(usefunc)
library(showtext)
library(rcartocolor)

df <- tibble(read_csv("2022/data/pollution.csv")[15:23,])

# add fonts
font_add_google(name = "Bungee", family = "bungee")
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# subtitle
st <- str_wrap_break("The number of serious pollution incidents affecting water, air or land decreased between 2002 and 2010. Most serious pollution incidents in the United Kingdom after water, with air pollution being the least common. The total number of pollution incidents per year is shown in grey, and the number of air, land, and water incidents are shown in purple, green, and blue, respectively.\n\nN. Rennie | Data: Environment Agency", 100)

# prep data
plot_data <- df %>%
  set_colnames(c("year", "Air", "Land", "Water")) %>%
  pivot_longer(cols = 2:4, names_to = "Type", values_to = "n") %>%
  mutate(n = as.numeric(n)) %>%
  group_by(year) %>%
  mutate(year_tot = sum(n))

# plot
ggplot() +
  geom_col(data = filter(plot_data, Type == "Air"),
           mapping = aes(x = year, y = year_tot),
           alpha = 0.5) +
  geom_col(data = plot_data,
           mapping = aes(x = year, y = n, fill = Type),
           position = "dodge") +
  scale_fill_manual("", values = c("#884c94", "#26aa83", "#4a75b0")) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = "Pollution Incidents",
       subtitle = st,
       x = "",
       y = "Number of serious pollution incidents") +
  theme_minimal() +
  theme(legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),
        axis.title.y = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black",
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black",
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black"),
        plot.title = element_text(family = "bungee", hjust = 1, size = 22, color = "black",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(family = "ubuntu", hjust = 1, size = 12, color = "black"),
        legend.text = element_text(family = "ubuntu", hjust = 1, size = 12, color = "black"),
        plot.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.ticks = element_blank())
