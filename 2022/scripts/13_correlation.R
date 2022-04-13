library(tidyverse)
library(usefunc)
library(showtext)
library(patchwork)
library(biscale)
library(maps)
library(cowplot)

# load fonts
font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# read data: https://worldpopulationreview.com/state-rankings/life-expectancy-by-state
df_life <- tibble(read.csv("2022/data/usa_life.csv")) %>%
  rename(state = "ï..State") %>%
  select(state, overall) %>%
  mutate(state = str_to_lower(state))

# read data: https://worldpopulationreview.com/state-rankings/average-rent-by-state
df_rent <- tibble(read.csv("2022/data/usa_rent.csv")) %>%
  rename(state = "ï..State") %>%
  select(state, MedianRent) %>%
  mutate(state = str_to_lower(state))

# prep data
plot_data <- left_join(df_rent, df_life, by = "state")
plot_data

# map data
main_states <- map_data("state")

map_data <- main_states %>%
  rename(state = "region") %>%
  inner_join(plot_data, by = "state") %>%
  filter(state != "") %>%
  select(-subregion)

us_classes <- bi_class(map_data, x=MedianRent, y=overall, style="quantile", dim=3) %>%
  mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class))

# plot map
p2 <- ggplot() +
  geom_polygon(data=us_classes,
               mapping=aes(x=long, y=lat, group=group, fill=bi_class),
               color="#fafafa", size=0.1, show.legend=FALSE) +
  bi_scale_fill(pal="DkBlue", dim=3, na.value="grey50") +
  theme(plot.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
p2

# plot correlation
p1 <- ggplot(data = plot_data,
       mapping = aes(x = MedianRent, y = overall)) +
  geom_point(size = 3, pch = 21, alpha = 0.5, fill = "#57a1bb", colour = "#3b4895") +
  geom_smooth(method = "lm", se = FALSE, size = 2, linetype="dashed", colour = "#3b4895") +
  labs(x = "Median Rent per Month ($)",
       y = "Average Life Expectancy") +
  theme(plot.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "#3b4895"),
        axis.text = element_text(family = "space", hjust = 0.5, size = 12, colour = "#3b4895"),
        axis.title.y = element_text(family = "space", colour = "#3b4895", hjust = 0.5, size = 12, margin = margin(r = 10)),
        axis.title.x = element_text(family = "space", colour = "#3b4895", hjust = 0.5, size = 12, margin = margin(t = 10)),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
p1

# subtitle
st <- str_wrap_break("Does paying higher rent make you live longer? No. Although the median rent per month is positively correlated with average life expectancy in each US state, one is unlikely to cause the other. Instead, both are caused by at least one other variable.\n\nN. Rennie | Data: worldpopulationreview.com | #30DayChartChallenge", 80)

# combine plots
p <- p1 + p2 +
  plot_annotation(title = "Correlation ≠ Causation",
                  subtitle = st) &
  theme(plot.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        plot.title = element_text(family = "space", face = "bold",
                                  hjust = 0.5, size = 24, colour = "#3b4895",
                                  margin = margin(b = 10)),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.subtitle = element_text(family = "space", hjust = 0.5, size = 12, colour = "#3b4895"))
p

# add legend
p_legend <- bi_legend(pal="DkBlue",
                      dim=3,
                      xlab="Rent ($)",
                      ylab="Life Expectancy",
                      size=12) +
  theme(plot.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        axis.title = element_text(family = "space", hjust = 0, size = 12, colour = "#3b4895"))
p_legend
q <- ggdraw() +
  draw_plot(p, 0, 0, 1, 1) +
  draw_plot(p_legend, 0.45, 0.05, 0.2, 0.2, scale=1)
q

