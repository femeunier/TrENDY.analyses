rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(plotbiomes)
library(raster)
library(ggplot2)
library(sf)

existing.files <- list.files(path = "./outputs/",pattern = "monthly.climate.global.*")
existing.files <- existing.files[existing.files != "monthly.climate.global.RDS"]
final.years <- as.numeric(gsub(".*?([0-9]+).*", "\\1", existing.files))

final.years <- c(seq(1910,2020,10),2022)
# final.years <- c(2010,2020,2022)

df.all <- data.frame()
for (ifinal.year in seq(1,length(final.years))){

  print(final.years[ifinal.year])

  cfile <- file.path("./outputs/",paste0("monthly.climate.global.",final.years[ifinal.year],".RDS"))
  cdf <- readRDS(cfile)

  df.all <- bind_rows(df.all,
                      cdf)

}

# df.all <- bind_rows(df.all,
#                     readRDS("./outputs/monthly.climate.global.RDS")) %>%
#   distinct()
# saveRDS(df.all,
#         "./outputs/monthly.climate.global.predictions.RDS")

saveRDS(df.all,
        "./outputs/monthly.climate.global.all.RDS")

# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/combine.monthly.inputs.R hpc:/data/gent/vo/000/gvo00074/felicien/R/



