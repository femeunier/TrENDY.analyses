rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(plotbiomes)
library(raster)
library(ggplot2)

years <- 1961:1990
vars <- c("pre","tmp","tmin","tmax")
# dir <- "/home/femeunier/Documents/projects/TrENDY.analyses/data"
dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDY/inputs"

days2months <- c(31,28,31,30,31,30,
                 31,31,30,31,30,31)
all.days <- rep(1:12,days2months)
all.days.hours <- sort(rep(all.days,4))

df.all <- df.all.monthly <- data.frame()

for (cyear in years){

  print(paste0(cyear))
  cyr.df <- data.frame()
  for (cvar in vars){

    print(paste0("- ",cvar))

    zip.file <- file.path(dir,
                          paste0("crujra.v2.1.5d.",cvar,".",cyear,".365d.noc.nc.gz"))

    system2("gunzip",
            paste("-k",zip.file))

    nc.file <- file.path(dir,
                         paste0("crujra.v2.1.5d.",cvar,".",cyear,".365d.noc.nc"))

    nc <- nc_open(nc.file)
    lats <- ncvar_get(nc,"lat")
    lons <- ncvar_get(nc,"lon")

    data <- ncvar_get(nc,cvar)

    values <- apply(data,c(1,2),mean,na.rm = TRUE)
    montly <- montly.min <- montly.max <- array(NA,dim = c(dim(values),12))

    for (imonth in (1:12)){
      montly[,,imonth] <- apply(data[,,which(all.days.hours == imonth)],c(1,2),mean,na.rm = TRUE)

      # if (cvar == "tmp"){
      #   montly.min[,,imonth] <- apply(data[,,which(all.days.hours == imonth)],c(1,2),min,na.rm = TRUE)
      #   montly.max[,,imonth] <- apply(data[,,which(all.days.hours == imonth)],c(1,2),max,na.rm = TRUE)
      #
      #   montly.min[!is.finite(montly.min)] <- NA
      #   montly.max[!is.finite(montly.max)] <- NA
      #
      # }

    }


    cdf <- reshape2::melt(values) %>%
      rename(lon = Var1,
             lat = Var2) %>%
      mutate(lon = lons[lon],
             lat = lats[lat]) %>% filter(!is.na(value)) %>%
      rename(!!cvar := value)

    cdf.monthly <- reshape2::melt(montly) %>%
      rename(lon = Var1,
             lat = Var2,
             month = Var3) %>%
      mutate(lon = lons[lon],
             lat = lats[lat]) %>% filter(!is.na(value)) %>%
      rename(!!cvar := value)

    # if (cvar == "tmp"){
    #   cdf.monthly.min <- reshape2::melt(montly.min) %>%
    #     rename(lon = Var1,
    #            lat = Var2,
    #            month = Var3) %>%
    #     mutate(lon = lons[lon],
    #            lat = lats[lat]) %>% filter(!is.na(value)) %>%
    #     rename(tmp = value)
    #
    #   cdf.monthly.max <- reshape2::melt(montly.max) %>%
    #     rename(lon = Var1,
    #            lat = Var2,
    #            month = Var3) %>%
    #     mutate(lon = lons[lon],
    #            lat = lats[lat]) %>% filter(!is.na(value)) %>%
    #     rename(tmp := value)
    #
    #   cdf.monthly[["tmp.min"]] <- cdf.monthly.min[["tmp"]]
    #   cdf.monthly[["tmp.max"]] <- cdf.monthly.max[["tmp"]]
    # }

    if (cvar == vars[1]){
      cyr.df <- cdf
      cmonth.df <- cdf.monthly
    } else {
      cyr.df <- cyr.df %>%
        left_join(cdf,
                  by = c("lat","lon"))

      cmonth.df <- cmonth.df %>%
        left_join(cdf.monthly,
                  by = c("lat","lon","month"))
    }



    # system2("rm",nc.file)

  }

  df.all <- bind_rows(list(df.all,
                           cyr.df %>% mutate(year = cyear)))

  df.all.monthly <- bind_rows(list(df.all.monthly,
                                   cmonth.df %>% mutate(year = cyear)))

}

saveRDS(df.all.monthly,
        "./outputs/CRU.JRA.RDS")

stop()

system2("scp",paste("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/CRU.JRA.RDS",
                    "/home/femeunier/Documents/projects/TrENDY.analyses/outputs/"))

df.all <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRU.JRA.RDS")

df.all.units <- df.all %>%
  mutate(pre = pre*4*365/10,
         tmp = tmp-273.15)

df.all.units.sum <- df.all.units %>%
  group_by(lat,lon) %>%
  summarise(tmp = mean(tmp,na.rm = TRUE),
            pre = mean(pre,na.rm = TRUE))


coords <- cbind(id = 1:nrow(df.all.units.sum),
                df.all.units.sum$tmp,
                df.all.units.sum$pre)

biome.id <- (plotbiomes::Whittaker_biomes) %>% dplyr::select(biome_id,biome) %>% distinct() %>% mutate(id = 1:9)
polys <- geometry(plotbiomes::Whittaker_biomes_poly)

sp <- SpatialPoints(coords[,c(2,3)])
e <- as.data.frame(raster::extract(polys,sp)) %>%
  rename(point.id = id.y,
         id = id.x) %>% left_join(biome.id %>% dplyr::select(-biome_id),
                                  by = "id")

df.all.units.sum["biome"] <- e$biome

saveRDS(df.all.units.sum,
        "./outputs/biome.RDS")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_tile(data = df.all.units.sum %>% filter(!is.na(biome)),
            aes(x = lon, y = lat,
                fill = as.factor(biome)),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  labs(x = "",y = "", fill = "Biome") +
  theme_bw()


# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/define.biomes.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
