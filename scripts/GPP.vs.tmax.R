rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

climate <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/monthly.climate.pantropical.RDS")
GPP.test <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.CABLE-POP.S3.gpp_centralAfrica_v11.RDS")

GPP.YGB <- GPP.test %>%
  mutate(month = round(1/2+(time - year)*12)) %>%
  group_by(year,month,lat,lon) %>%
  summarise(GPP.m = mean(value),
            .groups = "keep")

df.dist <- climate %>% filter(year == year[1],
                              month == month[1]) %>%
  mutate(dist = sqrt((lon - 24.8)**2 + (lat - 0.5)**2)) %>%
  arrange(dist) %>%
  slice_head(n = 9) %>%
  mutate(lon_lat = paste0(lon,lat,sep = "_"))

climate.select <- climate %>%
  mutate(lon_lat = paste0(lon,lat,sep = "_")) %>%
  dplyr::filter(lon_lat %in% df.dist[["lon_lat"]],
                year %in% (GPP.YGB %>% pull(year) %>%
                             unique())) %>%
  dplyr::select(-lon_lat) %>%
  mutate(tmp = tmp - 273.15,
         tmin = tmin - 273.15,
         tmax = tmax - 273.15)


GPP.vs.climate <- GPP.YGB %>%
  left_join(climate.select,
            by = c("lat","lon","year","month")) %>%
  filter(!is.na(tmp))

summary(lm(data = GPP.vs.climate %>%
     ungroup() %>%
     select(-c(year,month,lat,lon)),
   formula =  GPP.m ~ .))

ggplot(data = GPP.vs.climate,
       aes(x = tmp, y = GPP.m)) +
  geom_point() +
  stat_smooth(method = "lm",color = "red", fill = "red") +
  theme_bw()

GPP.vs.climate.long <- GPP.vs.climate %>%
  pivot_longer(names_to = "var",
               values_to = "val",
               cols = -c(year,month,lat,lon,GPP.m))

ggplot(data = GPP.vs.climate.long,
       aes(x = val, y = GPP.m)) +
  geom_point() +
  facet_wrap(~ var, scales = "free_x") +
  stat_smooth(method = "lm",color = "red", fill = "red",
              formula = y ~ x + I(x^2)) +
  theme_bw()
