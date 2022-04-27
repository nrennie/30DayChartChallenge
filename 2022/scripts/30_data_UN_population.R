library(tidyverse)
library(ggalluvial)
library(showtext)
library(usefunc)
library(readxl)
library(forcats)

# get data: https://population.un.org/wpp/Download/Standard/Population/
df <- read_xlsx("2022/data/un_pop.xlsx")

prep_data <- df %>%
  filter(Type  == "Region",
         `Reference date (as of 1 July)` == 2020) %>%
  select(`Region, subregion, country or area *`,
         `0-4`:`100+`) %>%
  pivot_longer(cols = -c(`Region, subregion, country or area *`),
               names_to = "age_group",
               values_to = "pop") %>%
  rename("Region" = `Region, subregion, country or area *`) %>%
  mutate(pop = as.numeric(pop),
         pop = round(pop*1000),
         Region = factor(Region, levels = c("Africa", "Oceania",
                                            "Asia", "Europe",
                                            "Latin America and the Caribbean",
                                            "Northern America")),
         age_group = factor(age_group, levels = unique(age_group)),
         age_group = fct_collapse(age_group,
                                  "75+" = c("75-79",
                                            "80-84",
                                            "85-89",
                                            "90-94",
                                            "95-99",
                                            "100+")))
prep_data

# continent vs age group plot
ggplot(as.data.frame(prep_data),
            aes(y = pop, axis1 = Region, axis2 = age_group)) +
  geom_alluvium(aes(fill = Region), width = 1/9) +
  geom_stratum(width = 1/9, fill = "black", color = NA, alpha = 0.3, lwd=0.1) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_text(stat = "stratum",
            aes(label = str_wrap(after_stat(stratum), 10)),
            colour = "#fafafa",
            lineheight = 0.6,
            size = 3,
            family = "ubuntu") +
  labs(x = "",
       y = "",
       title = "UN World Population 2020",
       subtitle = "N. Rennie | Data: United Nations, Department of Economic and Social Affairs, Population Division (2019) | #30DayChartChallenge") +
  theme_void() +
  theme(plot.title = element_text(family = "ubuntu", hjust = 0.5, face = "bold", size = 22, color = "black",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0.5,
                                     size = 10, color = "black"),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.background = element_rect(fill = "gray90", colour="gray90"),
        panel.background = element_rect(fill = "gray90", colour="gray90"),)
