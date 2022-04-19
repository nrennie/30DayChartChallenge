library(tidyverse)
library(usefunc)
library(showtext)

# add font
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# get data
df <- read_csv("2022/data/maternal-mortality.csv") %>%
  rename(value = `Maternal Mortality Ratio (Gapminder (2010) and World Bank (2015))`) %>%
  group_by(Year) %>%
  summarise(mean_ratio = mean(value))

df2 <- read_csv("2022/data/child-mortality.csv") %>%
  rename(value = "Share dying in first 5 years (based on Gapminder and World Bank (2019))") %>%
  filter(Entity == "World") %>%
  select(Year, value) %>%
  mutate(value = (value/100)*100000)

# prep text
st <- str_wrap_break("The maternal mortality ratio is the number of women who die from pregnancy-related causes while pregnant or within 42 days of pregnancy termination per 100,000 live births.\n\nThere are, however, a few countries where a young women today is more likely to die in childbirth than her mother was a generation ago: the United States, Serbia, Georgia, Saint Lucia, the Bahamas, North Korea, Jamaica, Tonga, Venezuela, South Africa, Suriname, Guyana and Zimbabwe.\n\n - Our World in Data", 130)
cap <- str_wrap_break("*Data is not available for every country in every year. An average across all available countries is taken each year. Some spikes in maternal mortality may be due to an increasing amount of data collected in countries where maternal mortality is higher, rather than a global increase. Estimates of average global maternal mortality may only be reliable since the 1990s.\n\nN. Rennie | Data: Our World in Data | #30DayChartChallenge", 130)

# plot
ggplot() +
  geom_area(data = df,
            mapping = aes(x = Year, y = mean_ratio),
            fill = "#811453") +
  labs(title = "Global Maternal Mortality",
       subtitle = st,
       caption = cap,
       x = "",
       y = "Maternal mortality ratio (per 100, 000)") +
  coord_cartesian(expand = F) +
  theme(plot.margin = unit(c(0.5, 1.2, 0.5, 0.5), "cm"),
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"),
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"),
        plot.title = element_text(family = "ubuntu", hjust = 0, face = "bold", size = 22, color = "black",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0, size = 12, color = "black"),
        plot.caption = element_text(family = "ubuntu", hjust = 0, size = 10, color = "black"),
        axis.text = element_text(family = "ubuntu", hjust = 0, size = 10, color = "black"),
        axis.title.y = element_text(family = "ubuntu", hjust = 0.5,
                                    size = 10, color = "black",
                                    margin = margin(r = 10)),
        axis.ticks.y = element_blank())
