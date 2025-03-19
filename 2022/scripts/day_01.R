library(tidyverse)
library(readxl)
library(usefunc)
library(geofacet)
library(countrycode)
library(showtext)

# read in data
df <- tibble(
  read_xlsx("./2022/data/Female Labor Participation Rate by Country and Year.xlsx"))

# load fonts
font_add_google(name = "Train One", family = "train")
font_add_google(name = "Mada", family = "mada")
showtext_auto()

# look at europe only
europe <- europe_countries_grid1$code
europe_iso3 <- countrycode(europe, origin = 'iso2c', destination = 'iso3c')
europe_countries_grid1$Code <- europe_iso3

# prep data
plot_data <- df %>%
  filter(Year == 2019,
         Code %in% europe_iso3) %>%
  rename("Yes" = "Labor force participation rate, female (% of female population ages 15+) (modeled ILO estimate)") %>%
  mutate("No" = 100 - Yes,
         "label" = paste0(round(Yes), "%")) %>%
  select(-Year) %>%
  pivot_longer(Yes:No, names_to = "Answer", values_to = "Perc")

# subtitle
st <- str_wrap_break("The female labour force participation rate (% of female population ages 15+ who are working) varies amongst European countries. In 2019, Iceland had the highest percentage with 70.2% of women in the workforce. At the other end of the scale, Turkey had the lowest at just 34.3%.\n\nN. Rennie | Data: Diversity in Data | #30DayChartChallenge", 100)

# plot
p <- ggplot(data = plot_data,
       mapping = aes(x = 4, y = Perc, fill = Answer)) +
  geom_col() +
  geom_text(data = plot_data,
            mapping = aes(x = 0.5, 0.5, label = label),
            size = 7, colour = "#E1AD01", fontface = "bold") +
  coord_polar(theta = "y") +
  xlim(c(0.2, 4.5)) +
  facet_geo(~ Code, grid = "europe_countries_grid1") +
  scale_fill_manual(values = c("#603268", "#E1AD01")) +
  labs(title = "Working Women",
       subtitle = st) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#301934", colour = "#301934"),
        plot.background = element_rect(fill = "#301934", colour = "#301934"),
        legend.position="none",
        strip.background =element_rect(fill="#301934", colour ="#301934"),
        strip.text = element_text(colour = '#E1AD01', size = 20, family="mada"),
        plot.title = element_text(colour = "#E1AD01", size=56, hjust = 0, family="train",
                                  margin = margin(b = 10)),
        plot.subtitle = element_text(colour = "#E1AD01", size=26, hjust = 0, family="mada",
                                     margin = margin(b = 10), lineheight = 0.5),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),)
p

ggsave(p, filename = "2022/viz/day_01.jpg", unit = "in", height = 7, width = 6)
