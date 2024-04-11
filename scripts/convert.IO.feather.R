rm(list = ls())

library(ggplot2)
library(dplyr)
library(viridis)
library(scales)
library(tidyr)
library(raster)
library(feather)
library(TrENDY.analyses)
library(zoo)

# Dataset4AI

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

biome <- readRDS("./outputs/biome1961.1990.RDS")
biome.rst <- rasterFromXYZ(biome[,c("lon","lat","tmp")])

###########################################################################################################
# Climate

CO2 <- read.table("./data/global_co2_ann_1700_2019.txt") %>%
  rename(year = V1,
         CO2 = V2)

z <- read.zoo(CO2)
zz <- na.approx(z, xout = seq(start(z), end(z), length.out=(end(z) - start(z))*12 + 1))

CO2 <- data.frame(time = as.numeric(rownames(as.data.frame(zz))),
                  CO2 = as.vector(zz)) %>%
  mutate(year = floor(time),
         month = c(rep(1:12,(end(z) - start(z))),1))

Climate <- readRDS( "./outputs/monthly.climate.global.RDS") %>%
  ungroup()
# %>%
#   filter(year == 2019)

all.climate <- Climate %>%
  left_join(CO2,
            by = c("year","month"))

write_feather(all.climate %>% dplyr::select(-c(time)),
              file.path("/data/gent/vo/000/gvo00074/felicien/TrENDYv11/AI/",
                        paste0("Global.Monthly.Climate.feather")))

###########################################################################################################


models <- model.names <- get.model.names.TRENDY(version = "v11")


for (cmodel in models){

  print(cmodel)

  file2read <- paste0("./outputs/Trendy.",cmodel,".S2.cVeg.Global.v11.RDS")

  if (!file.exists(file2read)){
    next()
  }

  cdf <- readRDS(file2read) %>%
    ungroup() %>%
    rename(cVeg = value) %>%
    filter(year <= 2019) %>%
    mutate(cVeg = as.numeric(cVeg),
           lat = as.numeric(as.vector(lat)),
           lon = as.numeric(as.vector(lon))) %>%
    dplyr::select(-c(abs.time,time,time.unit))


  cVeg.rspld <- TrENDY.analyses::resample.df.all.col(bigdf = cdf,
                                                     raster2resample = biome.rst,
                                                     var.names = "cVeg") %>%
    filter(!is.na(cVeg)) %>%
    group_by(lat,lon) %>%
    filter(!all(cVeg == 0))

  write_feather(cVeg.rspld,
                file.path("/data/gent/vo/000/gvo00074/felicien/TrENDYv11/AI/",
                          paste0("cVeg.S2.",cmodel,".Global.feather")))


}


# scp /home/femeunier/Documents/projects/TrENDY.analyses/data/global_co2_ann_1700_2019.txt hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/data/
# scp /home/femeunier/Documents/projects/TrENDY.analyses/outputs/biome1961.1990.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/convert.IO.feather.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/


