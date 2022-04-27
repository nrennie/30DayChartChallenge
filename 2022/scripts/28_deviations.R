library(tidyverse)
library(lubridate)

# read data
df <- as_tibble(read.table("2022/data/hadcet_daily.txt", header = F))
colnames(df) <- c("year", "day",
                  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# prep data
plot_data <- df %>%
  pivot_longer(cols = "Jan":"Dec", names_to = "month", values_to = "temp") %>%
  filter(year %in% c(2019, 2020, 2021),
         temp > -999) %>%
  unite(date, c(day, month, year), sep = "/", remove = FALSE) %>%
  mutate(date = dmy(date)) %>%
  group_by(year) %>%
  mutate(year_temp = mean(temp),
         dev = temp - year_temp) %>%
  ungroup() %>%
  select(-c(temp, day, month, year, year_temp)) %>%
  arrange(date)

# save as csv
write.csv(plot_data, "2022/data/deviation.csv", row.names = FALSE)

# chart created with rawgraphs
