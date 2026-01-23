rm(list = ls())

# Compare CRUJRA and Bioclim for inventory plots (same drivers?)

library(dplyr)
library(ggplot2)
library(raster)
library(geodata)
library(tidyr)

rerun = FALSE

#################################################################
# Sites
networks <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Afritron+Rainfor+Asia+Sullivan.RDS")

networks2plot <- networks %>%
  group_by(ClusterCode) %>%
  slice_head(n = 1) %>%
  group_by(Continent) %>%
  rename(lat = Lat,lon = Lon) %>%
  dplyr::select(ClusterCode,lat,lon,int_ini,int_fin,Continent) %>%
  mutate(yr_ini = floor(int_ini),
         yr_fin = floor(int_fin)) %>%
  dplyr::select(-c(int_ini,int_fin))


#################################################################
# Climate
# CRU-JRA

file <- "./outputs/monthly.climate.pantropical.RDS"
system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/monthly.climate.pantropical.RDS",
                      file))

Climate <- readRDS(file)
df4dist <- Climate %>%
  filter(year == min(year),
         month == min(month))

Ngridcells = 1

# WorldClime

if (rerun){

  df4comp <- data.frame()
  for (isite in seq(1,nrow(networks2plot))){

    print(isite/nrow(networks2plot))

    clat <- networks2plot$lat[isite] ; clon <- networks2plot$lon[isite] ; cContinent = networks2plot$Continent[isite]

    # cyears <- seq(networks2plot$yr_ini[isite],
    #               networks2plot$yr_fin[isite],
    #               1)
    cyears <- 1961:1990


    # JRA
    cdist <- df4dist %>%
      mutate(dist = sqrt((lat - clat)**2 + (lon - clon)**2)) %>%
      arrange(dist) %>%
      slice_head(n = Ngridcells) %>%
      mutate(lat_lon = paste0(lat,lon,"_"))

    cClimate <- Climate %>%
      mutate(lat_lon = paste0(lat,lon,"_")) %>%
      dplyr::filter(lat_lon %in% c(cdist[["lat_lon"]]),
                    year %in% cyears) %>%
      dplyr::select(-c(lat_lon))


    # WC

    wc.tavg <- geodata::worldclim_tile("tavg",
                                       lon=networks2plot$lon[isite],
                                       lat=networks2plot$lat[isite],
                                       "./wc0.5/",
                                       res = 0.5)

    wc.tmin <- geodata::worldclim_tile("tmin",
                                       lon=networks2plot$lon[isite],
                                       lat=networks2plot$lat[isite],
                                       "./wc0.5/",
                                       res = 0.5)


    wc.tmax <- geodata::worldclim_tile("tmax",
                                       lon=networks2plot$lon[isite],
                                       lat=networks2plot$lat[isite],
                                       "./wc0.5/",
                                       res = 0.5)

    wc.prec <- geodata::worldclim_tile("prec",
                                       lon=networks2plot$lon[isite],
                                       lat=networks2plot$lat[isite],
                                       "./wc0.5/",
                                       res = prec)

    xy <- rbind(c(clon,clat))
    p <- vect(xy, crs="+proj=longlat +datum=WGS84")

    tavg.WC <- mean(as.numeric(terra::extract(wc.tavg, p)))
    tmin.WC <- mean(as.numeric(terra::extract(wc.tmin, p)))
    tmax.WC <- mean(as.numeric(terra::extract(wc.tmax, p)))
    precip.WC <- sum(as.numeric(terra::extract(wc.prec, p)))

    df4comp <- bind_rows(list(df4comp,
                              data.frame(MAT_JRA = mean(cClimate$tmp) - 273.15,
                                         tmin_JRA = mean(cClimate$tmin) - 273.15,
                                         tmax_JRA = mean(cClimate$tmax) - 273.15,
                                         MAP_JRA = cClimate %>%
                                           group_by(year) %>%
                                           summarise(Pmm = sum(pre*4*365/12),
                                                     .groups = "keep") %>%
                                           ungroup() %>%
                                           summarise(MAP = mean(Pmm)) %>% pull(MAP),


                                         MAT_WC = tavg.WC,
                                         tmin_WC = tmin.WC,
                                         tmax_WC = tmax.WC,
                                         MAP_WC = precip.WC,


                                         Continent = cContinent)
    ))


  }



  saveRDS(df4comp,
          "./outputs/CRUJRA.vs.WC.RDS")
} else {
  df4comp <- readRDS("./outputs/CRUJRA.vs.WC.RDS")
}





df4comp.long <- df4comp %>%
  ungroup() %>%
  mutate(id = 1:n()) %>%
  pivot_longer(cols = -c(id,Continent),
               names_to = "var",
               values_to = "value") %>%
  mutate(source = sub(".*\\_", "", var),
         var = sub("\\_.*", "", var)) %>%
  pivot_wider(names_from = source,
              values_from = value)

ggplot(data = df4comp.long,
       aes(x = JRA,y = WC, color = Continent)) +
  geom_point() +
  stat_smooth(method = "lm") +
  stat_smooth(method = "lm", color = "darkgrey") +
  geom_abline(slope = 1, intercept = 1,linetype = 2, color = "darkgrey") +
   facet_wrap(~ var, scales = "free") +
  theme_bw()

