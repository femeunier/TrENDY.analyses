rm(list = ls())

library(ggplot2)
library(dplyr)
library(viridis)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

CO2 <- read.table("/home/femeunier/Documents/projects/TrENDY.analyses/data/global_co2_ann_1700_2019.txt") %>%
  rename(year = V1,
         CO2 = V2)

# Dataset4AI

file <- "/home/femeunier/Documents/projects/Congo.ED2/outputs/monthly.climate.pantropical.RDS"
system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/monthly.climate.pantropical.RDS",
                      file))

Climate <- readRDS(file)

NPP <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.pantropical.S2.npp.ORCHIDEE.v11.RDS")

NPP.top <- NPP %>%
  ungroup() %>%
  filter(year == 1955,
         month == 1)

all(sort(unique(NPP.top$lat)) %in% sort(unique(Climate$lat)))
all(sort(unique(NPP.top$lon)) %in% sort(unique(Climate$lon)))

NPP.select <- NPP %>%
  dplyr::select(lon,lat,year,month,model,value) %>%
  rename(npp = value) %>%
  filter(year <= 2019)  # After, no climate

NPP.climate <- NPP.select %>% left_join(Climate,
                                        by = c("lat","lon","year","month")) %>%
  left_join(CO2,
            by = "year")



NPP.climate.sum <- NPP.climate %>%
  group_by(lat,lon) %>%
  summarise(tmax = mean(tmax,na.rm = TRUE),
            tmin = mean(tmin,na.rm = TRUE),
            npp = mean(npp,na.rm = TRUE),
            .groups = "keep")

ggplot(data = NPP.climate.sum,
       aes(x = tmax, y = npp)) +
  # geom_point() +
  geom_bin2d() +
  # geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  stat_smooth(method = "lm", se = FALSE) +
  theme_bw()

write.csv(NPP.climate,file = "./outputs/Climate.vs.NPP.csv")
