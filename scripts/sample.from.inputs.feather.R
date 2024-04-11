rm(list = ls())

library(dplyr)
library(feather)

file <- "/data/gent/vo/000/gvo00074/felicien/TrENDYv11/AI/Global.Monthly.Climate.feather"
data <- read_feather(file) %>%
  mutate(lat_lon = paste(lat,lon,sep = "_"))

selected <- data %>%
  dplyr::filter(month == 1, year == min(year)) %>%
  sample_frac(size = 0.05)


data.filt <- data %>%
  dplyr::filter(lat_lon %in% selected[["lat_lon"]])

write_feather(data.filt,
              "/data/gent/vo/000/gvo00074/felicien/TrENDYv11/AI/Global.Monthly.Climate.sample.feather")
