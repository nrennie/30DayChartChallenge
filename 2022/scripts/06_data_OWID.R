library(emojifont)
library(tidyverse)
library(usefunc)
library(showtext)

# data from https://ourworldindata.org/child-mortality#child-mortality-around-the-world-since-1800
df <- tibble(year = c(1800, 1900, 2000),
             percent_die = c(43.3, 36.2, 7.7))

# load fonts
font_add_google(name = "Raleway", family = "raleway")
showtext_auto()

# prep data
df_data <- df %>%
  mutate(per_hundred = round(percent_die),
         survive = (100 - per_hundred),
         facet_label = paste0(year, " (", survive, "%)")) %>%
  pivot_longer(cols = 3:4, values_to = "perc", names_to = "type") %>%
  select(-percent_die)

plot_data <- rep_df(expand.grid(x = rep(1:10), y = rep(1:10)), length(unique(df_data$facet_label))) %>%
  mutate(year = rep(unique(df_data$facet_label), each = 100),
         label = fontawesome('fa-user'),
         type = rep(df_data$type, times = df_data$perc))

# plot
ggplot() +
  geom_text(data = plot_data,
            mapping = aes(x = x,
                          y = y,
                          label = label,
                          colour = type),
            family='fontawesome-webfont', size = 6) +
  facet_wrap(~year) +
  scale_colour_manual("", values = c("#d3d3d3", "#673eb7")) +
  labs(title = "Two Centuries of Progress: Child Mortality",
       subtitle = str_wrap_break("\n\nThe global percentage of children dying before their fifth birthday has decreased more than five-fold in the last two centuries: from 43% in 1800, to 8% in 2000. Child mortality rates continue to fall.\n\nWhat percentage of children live beyond their fifth birthday?\n\n", 70),
       caption = "N. Rennie | Data: ourworldindata.org",
       x = "",
       y = "") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = "#e7e7e7", colour = "#e7e7e7"),
        plot.background = element_rect(fill = "#e7e7e7", colour = "#e7e7e7"),
        legend.position="none",
        strip.background =element_rect(fill="#e7e7e7", colour ="#e7e7e7"),
        strip.text = element_text(colour = '#404040', family="raleway", size=24),
        plot.title = element_text(colour = "#404040", size=26, hjust = 0.5, family="raleway"),
        plot.subtitle = element_text(colour = "#404040", size=14, hjust = 0.5, family="raleway"),
        plot.caption = element_text(colour = "#404040", size=12, hjust = 0.5, family="raleway"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank())




