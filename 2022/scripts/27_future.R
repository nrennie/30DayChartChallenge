library(tidyverse)
library(lubridate)
library(showtext)
library(usefunc)
library(patchwork)
library(fable)
library(tsibble)
library(feasts)
library(ggforce)

# add fonts
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# read and prep data
df <- as_tibble(read.table("2022/data/hadcet.txt", header = F))
colnames(df) <- c("year",
                  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                  "avg")
df <- df %>%
  select(year, avg) %>%
  filter(year >= 1880,
         avg > -50)

temp_data <- df %>%
  as_tsibble(index = year)

# fit arima model
fit <- temp_data %>%
  model(
    arima = ARIMA(avg)
  )
fit

fc <- fit %>%
  forecast(h = 29) %>%
  hilo(level = c(95)) %>%
  unpack_hilo("95%")
fc

# join data together
plot_df <- tibble(type = c(rep("obs", nrow(df)), rep("pred", nrow(fc))),
                  date = c(df$year, fc$year),
                  value = c(df$avg, fc$.mean),
                  lower = c(rep(NA, nrow(df)), fc$`95%_lower`),
                  upper = c(rep(NA, nrow(df)), fc$`95%_upper`))

# subtitle
st <- str_wrap_break("The Hadley Centre Central England Temperature (HadCET) dataset is the longest instrumental record of temperature in the world. The temperatures are representative of a roughly triangular area of the United Kingdom enclosed by Lancashire, London and Bristol. An ARIMA(1,1,1) model with drift predicts a further increase of around 0.5°C in average temperature by 2050.", 90)


# plot
ggplot(data = plot_df) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper),
              alpha = 0.5,
              fill = "#6A359C") +
  geom_line(aes(x = date, y = value, linetype = type),
            show.legend = FALSE, size = 1) +
  scale_linetype_manual("", values = c("obs" = "solid", "pred" = "dashed")) +
  scale_y_continuous(limits = c(8, 12)) +
  labs(x = "",
       y = "Temperature (°C)",
       title = "Central England Temperatures in 2050",
       subtitle = st,
       caption = "N. Rennie | Data: Met Office (HadCET) | #30DayChartChallenge") +
  facet_zoom(xlim = c(2022, 2050),
             ylim = c(8, 12),
             horizontal = FALSE) +
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.grid.major = element_line(colour = "lightgrey"),
        plot.title = element_text(family = "ubuntu", hjust = 0, face = "bold", size = 16, color = "black",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0,
                                     size = 12, color = "black", margin = margin(b = 10)),
        plot.caption = element_text(family = "ubuntu", hjust = 0,
                                    size = 10, color = "black",
                                    margin = margin(t = 5)),
        axis.title.x = element_text(family = "ubuntu", hjust = 0.5,
                                    size = 10, color = "black"),
        axis.title.y = element_text(family = "ubuntu", hjust = 0.5,
                                    size = 10, color = "black", margin = margin(r = 10)))






