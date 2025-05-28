rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(raster)
library(RColorBrewer)
library(TrENDY.analyses)

model.names <- get.model.names.TRENDY(version = "v13")

df.all <- data.frame()
for (cmodel in model.names){

  print(cmodel)

  OPfile <- paste0("./outputs/Trendy.",cmodel,".S3.cAGB.pantropical.v13.RDS")

  if (file.exists(OPfile)){
    A <- readRDS(OPfile) %>% ungroup()
    A.filt <- A %>%
      filter(year >= 2000)
    A.filt.select <- A.filt %>%
      filter(lon >= 15, lon <= 30,
             lat >= -10, lat <= 10)

    A.sum <- A.filt.select %>%
      group_by(year,month) %>%
      summarise(cVeg.m = mean(cVeg,na.rm = TRUE),
                .groups = "keep")

    df.all <- bind_rows(df.all,
                        A.sum %>% mutate(model = cmodel))
  }

}

saveRDS(df.all,"./outputs/cVeg.ts.congo.RDS")

# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/combine.cVeg.Congo.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

