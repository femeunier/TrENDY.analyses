rm(list = ls())

library(dplyr)
library(zoo)
library(ggplot2)
library(timeplyr)
library(lubridate)

system2("scp",
        c("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/cVeg.ts.congo.RDS",
          "./outputs/"))

raw <- readRDS("./outputs/cVeg.ts.congo.RDS")

df.sum <- raw %>%
  group_by(year,model) %>%
  summarise(cVeg.m = mean(cVeg.m,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(model) %>%
  mutate(delta = c(NA,diff(cVeg.m)))

ggplot(data = df.sum) +
  geom_line(aes(x = year,
                y = delta,
                color = model)) +
  theme_bw()
