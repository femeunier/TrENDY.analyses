rm(list = ls())

library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(YGB)
library(TrENDY.analyses)
library(scales)

############################################################################################
# Regridding

biome <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/biome1961.1990.RDS")
biome.rst <- rasterFromXYZ(biome[,c("lon","lat","tmp")])
e <- as(extent(-10, 45, -15, 10), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
biome.rst.crop <- crop(biome.rst, e)

############################################################################################
# Climate

Ndays.per.month <- c(31,28,31,30,31,30,31,31,30,31,30,31)

df.climate <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRU.JRA.1991.2020.RDS") %>%
  dplyr::filter(lat >= -15, lat <= 10,
                lon >= -15, lon <= 45) %>%
  mutate(Pmm = pre/6*24*Ndays.per.month[month])

hist(df.climate$Pmm)

df.climate.MCWD <- df.climate %>%
  filter(!is.na(Pmm)) %>%
  dplyr::select(lon,lat,Pmm,tmin,tmax,year,month) %>%
  group_by(month,lon,lat) %>%
  summarise(Pmm = mean(Pmm),
            tmin = mean(tmin),
            tmax = mean(tmax),
            .groups = "keep") %>%
  group_by(lon,lat) %>%
  mutate(MAP = sum(Pmm),
         tmean = mean(tmin+tmax)/2,
         Ndays = Ndays.per.month,
         E = 3.33,
         Etot = E*Ndays) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD))

df.climate.MCWD.uni <- df.climate.MCWD %>%
  group_by(lat,lon) %>%
  summarise(MAP = unique(MAP),
            MCWD = unique(MCWD),
            tmin = unique(tmin),
            tmean = unique(tmean),
            tmax = unique(tmax),
            .groups = "keep")

df.climate.MCWD.uni.rspd <- resample.df.all.col(bigdf = df.climate.MCWD.uni %>% mutate(time = 1),
                                                raster2resample = biome.rst.crop,
                                                var.names = c("MAP","MCWD","tmin","tmax","tmean"),
                                                res = 0.5) %>% dplyr::select(-time)

hist(df.climate.MCWD.uni.rspd$MAP)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_tile(data = df.climate.MCWD.uni.rspd,
            aes(x = lon, y = lat,
                fill = tmean),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "red",na.value = "transparent") +
  labs(x = "",y = "") +
  theme_bw()

hist(df.climate.MCWD.uni.rspd$tmean-273.15)


###################################################################################
# AGB

df.AGB <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.centralAfrica.S3.cAGB.rspld_v11.RDS") %>%
  distinct() %>%
  dplyr::filter(lat >= -15, lat <= 10,
                lon >= -15, lon <= 45) %>%
  pivot_wider(names_from = "variable",
              values_from = "value") %>%
  mutate(cAGB = case_when(is.na(cRoot) ~ cVeg,
                          TRUE ~ cVeg- cRoot)) %>%
  filter(!is.na(cAGB))

ggplot(data = world) +
  geom_raster(data = df.AGB,
            aes(x = lon, y = lat,
                fill = cAGB),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  facet_wrap(~ model) +
  labs(x = "",y = "") +
  theme_bw()

###################################################################################
# Merging

df.AGB.climate <- df.AGB %>%
  left_join(df.climate.MCWD.uni.rspd,
            by = c("lat","lon")) %>%
  mutate(cat = case_when(MCWD >= -250 ~ 2,
                         MAP < 1000 ~ 1,
                         TRUE ~ 3)) %>%
  filter(!(is.na(cAGB) | is.na(MAP)))


ggplot(data = df.AGB.climate) +
  geom_boxplot(aes(x = as.factor(cat), y = cAGB,
                   fill = as.factor(cat)),
               outlier.shape = NA) +
  facet_wrap(~ model) +
  # scale_y_log10() +
  theme_bw()

df.AGB.climate.sum <- df.AGB.climate %>%
  group_by(lat,lon) %>%
  summarise(cAGB = mean(cAGB),
            MAP = unique(MAP),
            MCWD = unique(MCWD),
            tmean = unique(tmean),
            tmin = unique(tmin),
            tmax = unique(tmax),
            .groups = "keep")

ggplot(data = df.AGB.climate.sum) +
  geom_point(aes(x = MCWD,
                 y = MAP,
                 color = cAGB)) +
  theme_bw()


climate_vs_agb.cat <- df.AGB.climate

delta.quant = 0.1
MAP.quant <- as.numeric(quantile(climate_vs_agb.cat$MAP,seq(delta.quant,1-delta.quant,delta.quant)))
MCWD.quant <- as.numeric(quantile(climate_vs_agb.cat$MCWD,seq(delta.quant,1-delta.quant,delta.quant)))
tmean.quant <- as.numeric(quantile(climate_vs_agb.cat$tmean,seq(delta.quant,1-delta.quant,delta.quant)))
# tmin.quant <- as.numeric(quantile(climate_vs_agb.cat$tmin,seq(delta.quant,1-delta.quant,delta.quant)))
# tmax.quant <- as.numeric(quantile(climate_vs_agb.cat$tmax,seq(delta.quant,1-delta.quant,delta.quant)))
# SW.quant <- as.numeric(quantile(climate_vs_agb.cat$SW,seq(delta.quant,1-delta.quant,delta.quant)))

MAP.quant.all <- c(climate_vs_agb.cat %>% filter(!is.na(MAP)) %>% pull(MAP) %>% min(),
                   MAP.quant) +
  diff(c(climate_vs_agb.cat %>% filter(!is.na(MAP)) %>% pull(MAP) %>% min(),
         MAP.quant,
         climate_vs_agb.cat %>% filter(!is.na(MAP)) %>% pull(MAP) %>% max()))/2

tmean.quant.all <- c(climate_vs_agb.cat %>% filter(!is.na(tmean)) %>% pull(tmean) %>% min(),
                   tmean.quant) +
  diff(c(climate_vs_agb.cat %>% filter(!is.na(tmean)) %>% pull(tmean) %>% min(),
         tmean.quant,
         climate_vs_agb.cat %>% filter(!is.na(tmean)) %>% pull(tmean) %>% max()))/2

# tmin.quant.all <- c(climate_vs_agb.cat %>% filter(!is.na(tmin)) %>% pull(tmin) %>% min(),
#                      tmin.quant) +
#   diff(c(climate_vs_agb.cat %>% filter(!is.na(tmin)) %>% pull(tmin) %>% min(),
#          tmin.quant,
#          climate_vs_agb.cat %>% filter(!is.na(tmin)) %>% pull(tmin) %>% max()))/2
#
# tmax.quant.all <- c(climate_vs_agb.cat %>% filter(!is.na(tmax)) %>% pull(tmax) %>% min(),
#                      tmax.quant) +
#   diff(c(climate_vs_agb.cat %>% filter(!is.na(tmax)) %>% pull(tmax) %>% min(),
#          tmax.quant,
#          climate_vs_agb.cat %>% filter(!is.na(tmax)) %>% pull(tmax) %>% max()))/2

MCWD.quant.all <- c(climate_vs_agb.cat %>% filter(!is.na(MCWD)) %>% pull(MCWD) %>% min(),
                    MCWD.quant) +
  diff(c(climate_vs_agb.cat %>% filter(!is.na(MCWD)) %>% pull(MCWD) %>% min(),
         MCWD.quant,
         climate_vs_agb.cat %>% filter(!is.na(MCWD)) %>% pull(MCWD) %>% max()))/2

# SW.quant.all <- c(climate_vs_agb.cat %>% filter(!is.na(SW)) %>% pull(SW) %>% min(),
#                   SW.quant) +
#   diff(c(climate_vs_agb.cat %>% filter(!is.na(SW)) %>% pull(SW) %>% min(),
#          SW.quant,
#          climate_vs_agb.cat %>% filter(!is.na(SW)) %>% pull(SW) %>% max()))/2

df.sum <- climate_vs_agb.cat %>%
  mutate(MAP.cat = classify.quant(MAP,MAP.quant),
         MCWD.cat = classify.quant(MCWD,MCWD.quant),
         tmean.cat = classify.quant(tmean,tmean.quant),
         # tmin.cat = classify.quant(tmin,tmin.quant),
         # tmax.cat = classify.quant(tmax,tmax.quant),
         # SW.cat = classify.quant(SW,SW.quant)
         ) %>%
  mutate(MAP.cat.abs = MAP.quant.all[MAP.cat],
         # SW.cat.abs = SW.quant.all[SW.cat],
         MCWD.cat.abs = MCWD.quant.all[MCWD.cat],
         # tmin.cat.abs = tmin.quant.all[tmin.cat],
         tmean.cat.abs = tmean.quant.all[tmean.cat],
         # tmax.cat.abs = tmax.quant.all[tmax.cat]
         )

df.long <- df.sum %>%
  dplyr::select(lon,lat,cAGB,model,
                # SW.cat,
                MAP.cat,MCWD.cat,
                # tmin.cat,tmax.cat,
                tmean.cat) %>%
  pivot_longer(cols = c(MAP.cat,MCWD.cat,
                        # tmin.cat,tmax.cat,
                        tmean.cat),
               names_to = "var",
               values_to = "value")


# Data
new.raster <- rasterFromXYZ(expand.grid(sort(unique(df.long$lon)),
                                        sort(unique(df.long$lat))) %>% rename(lon = Var1,
                                                                              lat = Var2) %>%
                              mutate(v = NA))

Avi.AFR <- stack("/home/femeunier/Documents/projects/SoilSensitivity/data/AGBavit_AFR.gri")

e <- as(extent(-10, 45, -15, 10), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
Avi.AFR.crop <- crop(Avi.AFR, e)
Avi.AFR.crop.rspld <- resample(Avi.AFR.crop/20,new.raster)

AGB.data.map <- as.data.frame(Avi.AFR.crop.rspld,
                              xy = TRUE) %>%
  rename(lon = x,
         lat = y)

df.long.data <- bind_rows(list(df.long,
                               df.long %>%
                                 filter(model == model[1]) %>%
                                 dplyr::select(-cAGB) %>%
                                 left_join(AGB.data.map %>% rename(cAGB = Avitabile_AGB_Map),
                                           by = c("lon","lat")) %>%
                                 mutate(model = "data")
                               )) %>%
  mutate(model = factor(model,
                        levels = c(unique(df.long$model),"data")))


Climate_AGB_sum <- df.long.data %>%
  group_by(model,var,value) %>%
  summarise(cAGB.med = median(cAGB,na.rm = TRUE),
            cAGB.mean = mean(cAGB,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.long.data) +
  geom_line(data = Climate_AGB_sum,
            aes(x = value,y = cAGB.mean,color = model),
            alpha = 1, size = 1) +
  facet_grid(~var) +
  scale_x_continuous(breaks = seq(1,1/delta.quant)) +
  scale_color_manual(values = c(hue_pal()(length(unique(df.long$model))),"black")) +
  scale_fill_manual(values = c(hue_pal()(length(unique(df.long$model))),"black")) +

  theme_bw()


# MEM vs data

df.MEM.data <- df.long.data %>% mutate(type = case_when(model == "data" ~ "data",
                                                        TRUE ~ "model")) %>%
  group_by(lon,lat,var,value,type) %>%
  summarise(cAGB = mean(cAGB,na.rm = TRUE),
            .groups = "keep")

delta = 0.

Climate_AGB_sum_MEM <- df.MEM.data %>%
  group_by(type,var,value) %>%
  summarise(cAGB.med = median(cAGB,na.rm = TRUE),
            cAGB.mean = mean(cAGB,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(value = case_when(type == "data" ~ (value - delta),
                           type == "model" ~ (value + delta)))

ggplot(data = df.MEM.data) +

  geom_boxplot(aes(x = (value),y = cAGB,
                   fill = type,
                   group = interaction(type,value)),
               # outlier.shape = NA,
               show.legend = FALSE,
               alpha = 0.25) +

  geom_line(data = Climate_AGB_sum_MEM,
            aes(x = value,y = cAGB.mean,color = type),
            alpha = 1, size = 1) +

  facet_grid(~var) +

  scale_x_continuous(breaks = seq(1,1/delta.quant)) +

  theme_bw()







