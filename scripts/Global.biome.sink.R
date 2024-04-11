rm(list = ls())

library(ggplot2)
library(dplyr)
library(viridis)
library(scales)
library(tidyr)
library(raster)
library(feather)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

biome <- readRDS("./outputs/biome1961.1990.RDS")
biome.rst <- rasterFromXYZ(biome[,c("lon","lat","tmp")])
biome.rst.crop <- crop(biome.rst,extent(-10,45,-15,10))

###########################################################################################################

rerun = FALSE

if (rerun){

  Veg <- readRDS("./outputs/Trendy.Global.cVeg.v11.RDS") %>%
    dplyr::select(-c(scenario,variable)) %>%
    rename(cVeg = value) %>%
    filter(year <= 2019)  # After, no climate

  models <- unique(Veg$model)

  Veg.rspld <- Veg.climate.sum <- data.frame()

  for (cmodel in models){

    print(cmodel)
    cdf <- Veg %>% filter(model == cmodel)
    cVeg.rspld <- TrENDY.analyses::resample.df.all.col(bigdf = cdf,
                                                       raster2resample = biome.rst,
                                                       var.names = "cVeg") %>%
      filter(!is.na(cVeg),
             cVeg >= 0)

    Veg.select <- cVeg.rspld %>%
      ungroup() %>%
      dplyr::select(lon,lat,year,cVeg)

    Veg.rspld <- bind_rows(list(Veg.rspld,
                                Veg.select %>% mutate(model = cmodel)))

  }

  Veg.sum <- Veg.rspld %>%
    group_by(model,lat,lon) %>%
    summarise(cVeg = mean(cVeg,na.rm = TRUE),
              .groups = "keep")

  saveRDS(Veg.sum,
          "./outputs/Global.Veg.sum.RDS")

  Veg.biome <- Veg.rspld %>%
    left_join(biome,
              by = c("lat","lon")) %>%
    group_by(lat,lon,model,biome) %>%
    mutate(sink = c(NA_real_,diff(cVeg)))

  Veg.biome.sum <- Veg.biome %>%
    group_by(model,biome,year) %>%
    summarise(cVeg.m = mean(cVeg,na.rm = TRUE),
              sink.m = mean(sink,na.rm = TRUE),
              .groups = "keep")

  saveRDS(Veg.biome.sum,
          "./outputs/Global.Veg.biome.sum.RDS")


} else {

  system2("rsync",
          paste("-avz",
                "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Global.Veg.sum.RDS",
                "./outputs/Global.Veg.sum.RDS"))

  Veg.sum <- readRDS("./outputs/Global.Veg.sum.RDS")
  models <- unique(Veg.sum$model)

  system2("rsync",
          paste("-avz",
                "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Global.Veg.biome.sum.RDS",
                "./outputs/Global.Veg.biome.sum.RDS"))

  Veg.biome.sum <- readRDS("./outputs/Global.Veg.biome.sum.RDS")

}

models2plot <- c(models[sample(1:length(models),
                               min(6,length(models)), replace = FALSE)])


df2plot <- Veg.sum %>%
  ungroup() %>%
  dplyr::filter(model %in% models2plot)

ggplot(data = world) +
  geom_raster(data = df2plot,
            aes(x = lon, y = lat,
                fill = cVeg),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  labs(x = "",y = "") +
  scale_x_continuous(limits = c(-100,145)) +
  scale_y_continuous(limits = c(-20,20)) +
  scale_fill_gradient (low = "white",
                       high = muted("green"),
                       limits = c(0,40),
                       oob = scales::squish) +
  facet_wrap(~ model) +
  theme_bw()


ggplot(data = Veg.biome.sum %>%
         filter(!is.na(biome))) +
  geom_line(aes(x = year, y = cVeg.m, color = model)) +
  facet_wrap(~ biome) +
  theme_bw()

ggplot(data = Veg.biome.sum %>%
         filter(!is.na(biome))) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(aes(x = year, y = sink.m, color = model)) +
  facet_wrap(~ biome) +
  theme_bw()

# MEM
Veg.biome.sum.MEM <- Veg.biome.sum %>%
  group_by(year,biome) %>%
  summarise(cVeg.m.MEM = mean(cVeg.m,na.rm = TRUE),
            cVeg.sd.MEM = sd(cVeg.m,na.rm = TRUE),
            sink.m.MEM = mean(sink.m,na.rm = TRUE),
            sink.sd.MEM = sd(sink.m,na.rm = TRUE),
            .groups = "keep")

ggplot(data = Veg.biome.sum.MEM %>%
         filter(!is.na(biome)),
       aes(x = year, y = cVeg.m.MEM)) +
  geom_ribbon(aes(ymin = cVeg.m.MEM - cVeg.sd.MEM,
                  ymax = cVeg.m.MEM + cVeg.sd.MEM),
              color = NA, fill = "lightgrey", alpha = 0.5) +
  geom_line(color = "black") +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ biome) +
  theme_bw()

ggplot(data = Veg.biome.sum.MEM %>%
         filter(!is.na(biome)),
       aes(x = year, y = sink.m.MEM)) +
  geom_ribbon(aes(ymin = sink.m.MEM - sink.sd.MEM,
                  ymax = sink.m.MEM + sink.sd.MEM),
              color = NA, fill = "lightgrey", alpha = 0.5) +
  geom_line(color = "black") +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ biome) +
  theme_bw()

# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/Global.biome.sink.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
