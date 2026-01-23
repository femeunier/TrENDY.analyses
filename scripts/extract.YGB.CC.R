rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(raster)
library(reshape2)
library(lattice)
library(rgdal)
library(YGB)
library(caret)
library(TrENDY.analyses)
library(stringr)
library(randomForest)
library(ggpointdensity)
library(lubridate)
library(xgboost)
library(zoo)
library(Congo.ED2)

models <- TrENDY.analyses::get.model.names.TRENDY("v13")

all.models <- data.frame()

for (cmodel in models){
  model.file <- paste0("./outputs/Trendy.",cmodel,".S2.CC.pantropical.v13.RDS")

  if (!all(file.exists(model.file))){
    next()
  }

  print(cmodel)

  all.models <- bind_rows(all.models,
                          readRDS(model.file) %>%
                            mutate(model = cmodel) %>%
                            filter(lat >= -10,lat <= 10,
                                   lon >= 20,lon <= 30) %>%
                            mutate(model.lat.lon = paste0(model,".",lat,".",lon)))

}

df.search <- all.models %>%
  group_by(model) %>%
  filter(month == 1,year == year[1]) %>%
  dplyr::select(lat,lon,model,model.lat.lon) %>%
  distinct()

clon = 24.82 ; clat = 0.82

model.lat.lons <- find.coord.Trendy(Trendy.grid = df.search,
                                    target = c(clon,clat),
                                    Ngridcells = 1) %>%
  unique()

selected <- all.models %>%
  filter(model.lat.lon %in% model.lat.lons)

saveRDS(selected,
        "./outputs/df.YGB.all.Trendy.RDS")

# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/extract.YGB.CC.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

