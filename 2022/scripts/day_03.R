library(tidyverse)
library(usefunc)
library(showtext)
library(ggforce)

# data sources
#https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/bulletins/annualsurveyofhoursandearnings/2020
#https://www.ons.gov.uk/economy/inflationandpriceindices/bulletins/housepriceindex/august2020
#https://landregistry.data.gov.uk/app/ukhpi/browse?from=1966-01-01&location=http%3A%2F%2Flandregistry.data.gov.uk%2Fid%2Fregion%2Funited-kingdom&to=2018-04-01&lang=en
#https://www.theguardian.com/news/datablog/2014/jul/03/seven-ways-uk-wages-changed-over-four-decades

# load fonts
font_add_google(name = "Libre Barcode 128 Text", family = "libre")
font_add_google(name = "Mada", family = "mada")
showtext_auto()

# data
d_1980 <- c(20857, 6000)
d_2020 <- c(239000, 31461)

# prep data
circles <- data.frame(
  x0 = c(0, -3, 0, -15),
  y0 = c(0, -3, 0, -15),
  r = 0.1*sqrt(c(d_1980, d_2020)),
  fill = c("brown", "green", "brown", "green"),
  year = c(1980, 1980, 2020, 2020)
)

# subtitle
st <- str_wrap_break("The area of the larger circles represents the average UK house price in 1980 and 2020, whilst the area of the smaller circle represents the median annual salary. In 1980, the average house price was 3.5 times larger than median annual salary. In 2020, it was more than 7.5 times the median annual salary.", 70)

# plot
ggplot() +
  geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = fill), data = circles,
              colour = NA) +
  facet_wrap(~year, nrow = 1) +
  scale_fill_manual(values = c("green" = "#944d20", "brown" = "#a4a239")) +
  coord_fixed() +
  theme_void() +
  labs(title = "Housing Costs in the UK",
       subtitle = st,
       caption = "N.Rennie | Data: Office for National Statistics / Land Registry") +
  theme(panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        legend.position="none",
        strip.background =element_rect(fill="#fafafa", colour ="#fafafa"),
        strip.text = element_text(colour = '#a4a239', size = 20, family="mada"),
        plot.title = element_text(colour = "#944d20", size=56, hjust = 0.5, family="libre",
                                  margin = margin(b = 20)),
        plot.subtitle = element_text(colour = "#944d20", size=14, hjust = 0.5, family="mada",
                                     margin = margin(b = 20), lineheight = 1),
        plot.caption = element_text(colour = "#944d20", size=12, hjust = 0.5, family="mada",
                                     margin = margin(t = 10), lineheight = 0.5),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))




