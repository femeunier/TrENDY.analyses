rm(list = ls())

library(ggplot2)
library(dplyr)
library(viridis)
library(scales)
library(tidyr)
library(raster)
library(feather)
library(TrENDY.analyses)

# Dataset4AI

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

biome <- readRDS("./outputs/biome1961.1990.RDS")
biome.rst <- rasterFromXYZ(biome[,c("lon","lat","tmp")])

###########################################################################################################
# Climate

CO2 <- read.table("./data/global_co2_ann_1700_2019.txt") %>%
  rename(year = V1,
         CO2 = V2)

# file <- "/home/femeunier/Documents/projects/Congo.ED2/outputs/monthly.climate.pantropical.RDS"
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/monthly.climate.pantropical.RDS",
#                       file))

Climate <- readRDS( "./outputs/monthly.climate.global.RDS") %>%
  ungroup()
# %>%
#   filter(year == 2019)


all.climate <- Climate %>%
  left_join(CO2,
            by = "year")

###########################################################################################################

# Veg <- readRDS("./outputs/Trendy.Global.cVeg.v11.RDS") %>%
#   ungroup() %>%
#   # pivot_wider(names_from = variable,
#   #             values_from = value) %>%
#   # dplyr::select(-c(scenario)) %>%
#   rename(cVeg = value) %>%
#   # filter(model == "CLM5.0",
#   #        year >= 2000) %>%
#   filter(year <= 2019)  # After, no climate

models <- model.names <- get.model.names.TRENDY(version = "v11")

# Veg.rspld <- data.frame()
Veg.climate.sum <- data.frame()
delta_pr <- 100
delta_t <- 1

for (cmodel in models){

  print(cmodel)

  file2read <- paste0("./outputs/Trendy.",cmodel,".S2.cVeg.Global.v11.RDS")

  if (!file.exists(file2read)){
    next()
  }

  cdf <- readRDS(file2read) %>%
    ungroup() %>%
    # pivot_wider(names_from = variable,
    #             values_from = value) %>%
    # dplyr::select(-c(scenario)) %>%
    rename(cVeg = value) %>%
    # filter(model == "CLM5.0",
    #        year >= 2000) %>%
    filter(year <= 2019) %>%
    mutate(cVeg = as.numeric(cVeg),
           lat = as.numeric(as.vector(lat)),
           lon = as.numeric(as.vector(lon))) %>%
    dplyr::select(-c(abs.time,time,time.unit))


  cVeg.rspld <- TrENDY.analyses::resample.df.all.col(bigdf = cdf,
                                                     raster2resample = biome.rst,
                                                     var.names = "cVeg") %>%
    filter(!is.na(cVeg),
           cVeg >= 0)

  Veg.select <- cVeg.rspld %>%
    ungroup() %>%
    dplyr::select(lon,lat,year,month,cVeg)

  sites2keep <- Veg.select %>%
    filter(year == year[1]) %>%
    mutate(lat.lon = paste0(lat,lon,sep = ".")) %>%
    pull(lat.lon) %>% unique()

  Climate <- Climate %>%
    mutate(lat.lon = paste0(lat,lon,sep = ".")) %>%
    dplyr::filter(lat.lon %in% sites2keep) %>%
    dplyr::select(- c(lat.lon))

  # Veg.rspld <- bind_rows(list(Veg.rspld,
  #                             Veg.select %>% mutate(model = cmodel)))

  Veg.climate <- Climate  %>%
    # mutate(month = 1) %>%
    left_join(Veg.select,
              by = c("lat","lon","year","month")) %>%
    left_join(CO2,
              by = "year") %>%
    mutate(model = cmodel)



  # write.csv(,
  #           file = file.path(getwd(),"./outputs",paste0("Climate.vs.AGB_",cmodel,".csv")))


  write_feather(Veg.climate %>%
                  ungroup() %>%
                  mutate(CO2 = case_when(month == 1 ~ CO2,
                                         TRUE ~ NA_real_)),
                file.path(getwd(),"./outputs",paste0("Climate.vs.AGB_",cmodel,".feather")))


  cdf.Veg.climate.sum <- Veg.climate %>%
    group_by(lat,lon,year) %>%
    mutate(MAP = sum(pre)*4*365/12) %>%
    group_by(lat,lon) %>%
    summarise(tmax = mean(tmax,na.rm = TRUE),
              tmp = mean(tmp,na.rm = TRUE),
              MAP = mean(MAP,na.rm = TRUE),
              tmin = mean(tmin,na.rm = TRUE),
              cVeg = mean(cVeg,na.rm = TRUE),
              .groups = "keep")

  cdf.summary <- cdf.Veg.climate.sum %>%
    mutate(tas.cat = round((1/delta_t)*tmp)/(1/delta_t),
           pr.cat = round(MAP/delta_pr)*delta_pr) %>%
    group_by(tas.cat,pr.cat) %>%
    summarise(cVeg.m = mean(cVeg),
              .groups = "keep")

  Veg.climate.sum <- bind_rows(list(Veg.climate.sum,
                                    cdf.summary %>% mutate(model = cmodel)))


  # ggplot(data = world) +
  #   geom_raster(data = cdf.Veg.climate.sum,
  #               aes(x = lon, y = lat,
  #                   fill = cVeg),na.rm = TRUE, alpha = 1) +
  #   geom_sf(fill = NA) +
  #   labs(x = "",y = "") +
  #   # scale_x_continuous(limits = c(-100,145)) +
  #   # scale_y_continuous(limits = c(-20,20)) +
  #   scale_fill_gradient (low = "white",
  #                        high = muted("green"),
  #                        limits = c(0,40),
  #                        oob = scales::squish) +
  #   theme_bw()


}

# Veg.sum <- Veg.rspld %>%
#   group_by(model,lat,lon) %>%
#   summarise(cVeg = mean(cVeg,na.rm = TRUE),
#             .groups = "keep")
#
#
#
# models2plot <- c(models2anal[sample(1:length(models2anal),
#                                     min(6,length(models2anal)), replace = FALSE)])
# df2plot <- Veg.sum %>%
#   ungroup() %>%
#   dplyr::filter(model %in% models2plot)

# ggplot(data = world) +
#   geom_raster(data = df2plot,
#             aes(x = lon, y = lat,
#                 fill = cVeg),na.rm = TRUE, alpha = 1) +
#   geom_sf(fill = NA) +
#   labs(x = "",y = "") +
#   scale_x_continuous(limits = c(-100,145)) +
#   scale_y_continuous(limits = c(-20,20)) +
#   scale_fill_gradient (low = "white",
#                        high = muted("green"),
#                        limits = c(0,40),
#                        oob = scales::squish) +
#   facet_wrap(~ model) +
#   theme_bw()


# ggplot() +
#   geom_tile(data = Veg.climate.sum %>% filter(model %in% models2plot),
#             aes(x = tas.cat - 273.15, y = pr.cat,
#                 fill = cVeg.m),na.rm = TRUE, alpha = 1) +
#   scale_fill_gradient(low ="white",
#                       high = scales::muted("darkgreen"),
#                       na.value = "transparent") +
#   labs(x = "MAT (°C)",y = "MAP (mm)", fill = "cVeg [kg/m²]") +
#   facet_wrap(~ model) +
#   theme_bw()

# scp /home/femeunier/Documents/projects/Congo.ED2/outputs/monthly.climate.pantropical.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp /home/femeunier/Documents/projects/TrENDY.analyses/data/global_co2_ann_1700_2019.txt hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/data/
# scp /home/femeunier/Documents/projects/TrENDY.analyses/outputs/biome1961.1990.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/climate.vs.agb_AI.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/


