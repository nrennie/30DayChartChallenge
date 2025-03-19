# data
# https://www.worldatlas.com/articles/the-top-sunflower-seed-producing-countries-in-the-world.html

df <- data.frame(country = c("Ukraine", "Russia", "Argentina", "China", "Romania", "Other"),
             prod = c(11.0, 10.6, 3.1, 2.4, 2.1, 15.3))
write.csv(df, "2022/data/sunflower.csv")

# link to tableau
# https://public.tableau.com/app/profile/nicola.rennie/viz/SunflowerSeedProduction/Sunflower_Treemap

#### R version

library(ggplot2)
library(dplyr)

polygons <- read.csv("2022/data/voronoi.csv")

text_df <- polygons %>%
  group_by(group) %>%
  summarise(x = mean(x),
            y = mean(y))

ggplot() +
  geom_polygon(data = polygons,
               mapping = aes(x = x,
                             y = y,
                             group = group,
                             fill = group),
               colour = "white",
               size = 2) +
  geom_text(data = text_df,
            mapping = aes(x = x, y = y, label = group),
            colour = "#46607c",
            size = 5) +
  coord_fixed() +
  scale_fill_manual(values = c("Ukraine"="#98d9e4", "Russia"="#3ca8bc", "Argentina"="#4e9f50",
                               "China"="#87d180", "Romania"="#fcc66d", "Other"="#e7e5ef")) +
  labs(title = "Sunflower Seed Production",
       subtitle = "Sunflower seed production per country in millions of tonnes.",
       caption = "Data: www.worldatlas.com/articles/the-top-sunflower-seed-producing-countries-in-the-world") +
  theme_void() +
  scale_y_reverse() +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.title = element_text(colour = "#648ab2", size=17, face = "bold", hjust = 0, family="sans"),
        plot.subtitle = element_text(colour = "darkgrey", size=14, hjust = 0, family="sans",
                                     face = "italic"),
        plot.caption = element_text(colour = "black", size=8, hjust = 0, family="sans",
                                    face = "italic"))

