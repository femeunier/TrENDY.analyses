rm(list = ls())

library(dismo)
library(tie)
library(corrplot)

days2months <- c(31,28,31,30,31,30,
                 31,31,30,31,30,31)

CRUJRA <- readRDS("./outputs/CRU.JRA.RDS") %>%
  mutate(pr = pre/6*24*days2months[month])


CRUJRA %>% group_by(lat,lon) %>%
  summarise(MAP = sum(pr)) %>%
  pull(MAP) %>% hist()

top <- CRUJRA %>%
  filter(lon == lon[1],
         lat == lat[1],
         year == year[1])

biovars(top$pre,top$tmin -273.15,top$tmax - 273.15)

CRUJRA.bio <- CRUJRA %>%
  # filter(lon == lon[1],
  #        lat == lat[1],
  #        year == year[1]) %>%
  group_by(lon,lat) %>%
  bow(tie(bio1, bio2, bio3, bio4,
          bio5, bio6, bio7, bio8,
          bio9, bio10, bio11, bio12,
          bio13, bio14, bio15, bio16,
          bio17, bio18, bio19) := c(biovars(pr,
                                            (tmin - 273.15),
                                            (tmax - 273.15))[c(1:19)]))

biovar.names <-
  c(bio1 = "Mean annual temperature",
    bio2 = "Mean diurnal range (mean of max temp - min temp)",
    bio3 = "Isothermality (bio2/bio7) (* 100)",
    bio4 = "Temperature seasonality (standard deviation *100)",
    bio5 = "Max temperature of warmest month",
    bio6 = "Min temperature of coldest month",
    bio7 = "Temperature annual range (bio5-bio6)",
    bio8 = "Mean temperature of the wettest quarter",
    bio9 = "Mean temperature of driest quarter",
    bio10 = "Mean temperature of warmest quarter",
    bio11 = "Mean temperature of coldest quarter",
    bio12 = "Total (annual) precipitation",
    bio13 = "Precipitation of wettest month",
    bio14 = "Precipitation of driest month",
    bio15 = "Precipitation seasonality (coefficient of variation)",
    bio16 = "Precipitation of wettest quarter",
    bio17 = "Precipitation of driest quarter",
    bio18 = "Precipitation of warmest quarter",
    bio19 = "Precipitation of Coldest Quarter")

CRUJRA.bio.long <- CRUJRA.bio %>%
  pivot_longer(cols = -c(lon,lat),
               names_to = "biovar") %>%
  group_by(biovar) %>%
  mutate(value.rel = (value - min(value))/(max(value) - min(value)),
         biovar = factor(biovar,
                         levels = names(biovar.names)))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_tile(data = CRUJRA.bio.long,
            aes(x = lon, y = lat,
                fill = value.rel),na.rm = TRUE, alpha = 1) +
  geom_sf(data = world,
          fill = NA) +
  labs(x = "",y = "") +
  facet_wrap(~ biovar) +
  scale_fill_gradient(low = "white",high = "darkgreen") +
  theme_bw()


ggplot() +
  geom_density(data = CRUJRA.bio.long,
            aes(x = value.rel, color = biovar), na.rm = TRUE, fill = NA) +
  theme_bw()

M <- cor((CRUJRA.bio %>% ungroup() %>%
                dplyr::select(-c(lon,lat))))

corrplot(M, method = 'ellipse', col = COL2(n=200), type = "upper", diag = FALSE)

saveRDS(CRUJRA.bio,file = "./outputs/CRUJRA.bio.RDS")

