rm(list = ls())

library(dplyr)
library(ggplot2)

df.CO2 <- read.table("/home/femeunier/Documents/projects/TrENDY.analyses/data/global_co2_ann_1700_2019.txt") %>%
  rename(year = V1,
         CO2 = V2)

CO2.all <- df.CO2 %>%
  mutate(CO2pi = df.CO2 %>%
           filter(year <= 1850) %>%
           pull(CO2) %>%
           mean()) %>%
  pivot_longer(cols = c(CO2,CO2pi)) %>%
  mutate(var = case_when(name == "CO2" ~ "Historical CO2",
                         TRUE ~ "PI CO2"))

ggplot(data = CO2.all) +
  geom_line(aes(x = year, y = value,
                linetype = var)) +
  scale_x_continuous(limits = c(1900,2020),expand = c(0,0),
                     breaks = seq(1900,2020,20)) +
  labs(x = "", y = "Atm. CO2 concentration [ppm]",
       linetype = "") +
  theme_bw() +
  theme(legend.position = c(0.1,0.9))
