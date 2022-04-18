library(OECD)
library(tidyverse)
library(usefunc)
library(ggrepel)
library(showtext)

# add fonts
font_add_google(name = "Cinzel Decorative", family = "cinzel")
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# get data
dsets <- get_datasets()
search_dataset("poverty", dsets)
df <- get_dataset("HOURSPOV")

# prep data
plot_df <- df %>%
  filter(LOCATION %notin% c("OECD", "EU28"),
         Time %in% c("2001", "2021")) %>%
  mutate(ObsValue = as.numeric(ObsValue),
         Time = as.numeric(Time)) %>%
  group_by(Time, LOCATION) %>%
  summarise(mean_val = mean(ObsValue))

# find biggest decrease
changes <- plot_df %>%
  pivot_wider(names_from = Time, values_from = c(mean_val)) %>%
  drop_na() %>%
  mutate(change = `2021` - `2001`)
slice_min(changes, n = 1, change)
slice_max(changes, n = 1, change)
countries <- changes %>% pull(LOCATION)

# prep data again...
plot_data <- plot_df %>%
  filter(LOCATION %in% countries) %>%
  mutate(num = as.numeric(LOCATION %in% c("LUX", "SVK")),
         num = factor(num, levels = c(0, 1))) %>%
  arrange(num) %>%
  mutate(LOCATION = factor(LOCATION, levels = LOCATION))

colours <- character(length = nrow(plot_data))
for (i in 1:nrow(plot_data)){
  if (plot_data$LOCATION[i] == "LUX"){
    colours[i] <- "#008080"
  } else if (plot_data$LOCATION[i] == "SVK") {
    colours[i] <- "#4b0082"
  } else {
    colours[i] <- "lightgrey"
  }
}
plot_data$colours <- colours
plot_data

# subtitle
st <- str_wrap_break("On average, the number of hours of work needed to escape poverty for workless families has decreased from 23.7 to 20.6 hours between 2001 and 2021. The USA and the Czech Republic have consistently had the highest number of hours, while Denmark and Japan have the lowest. Luxembourg has seen the biggest decrease, whilst Slovakia has seen the biggest increase.\n\n N. Rennie | Data: OECD | #30DayChartChallenge", 110)

# plot
p <- ggplot() +
  geom_line(data = plot_data,
            mapping = aes(x = Time, y = mean_val, group = LOCATION, colour = I(colours)),
            size = 0.5) +
  geom_text_repel(data = filter(plot_data, Time == 2021),
            mapping = aes(x = 2021.5, y = mean_val, label = LOCATION),
            hjust = 0, size = 5,
            family = "ubuntu",
            direction = "y",
            segment.color = 'white') +
  scale_x_continuous(breaks = c(2001, 2021),
                     limits = c(2001, 2023)) +
  coord_cartesian(expand = F) +
  labs(title = "Hours to escape poverty",
       subtitle = st,
       x = "",
       y = "Number of hours") +
  plot_theme(main_font = "ubuntu",
             title_font = "cinzel",
             bg_col = "#fafafa") +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 24))
p

ggsave(p, filename = "2022/viz/day_18.jpg", height = 6, width = 4, unit = "in")
