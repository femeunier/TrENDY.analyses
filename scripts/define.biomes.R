rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(plotbiomes)
library(raster)
library(ggplot2)
library(sf)

# years <- 1961:1990
# vars <- c("pre","tmp","tmin","tmax")
# # dir <- "/home/femeunier/Documents/projects/TrENDY.analyses/data"
# dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDY/inputs"
#
# days2months <- c(31,28,31,30,31,30,
#                  31,31,30,31,30,31)
#
# all.days <- rep(1:12,days2months)
# all.days.hours <- sort(rep(all.days,4))
#
# df.all <- df.all.monthly <- data.frame()
#
# for (cyear in years){
#
#   print(paste0(cyear))
#   cyr.df <- data.frame()
#   for (cvar in vars){
#
#     print(paste0("- ",cvar))
#
#     zip.file <- file.path(dir,
#                           paste0("crujra.v2.1.5d.",cvar,".",cyear,".365d.noc.nc.gz"))
#
#     system2("gunzip",
#             paste("-k",zip.file))
#
#     nc.file <- file.path(dir,
#                          paste0("crujra.v2.1.5d.",cvar,".",cyear,".365d.noc.nc"))
#
#     nc <- nc_open(nc.file)
#     lats <- ncvar_get(nc,"lat")
#     lons <- ncvar_get(nc,"lon")
#
#     data <- ncvar_get(nc,cvar)
#
#     values <- apply(data,c(1,2),mean,na.rm = TRUE)
#     montly <- montly.min <- montly.max <- array(NA,dim = c(dim(values),12))
#
#     for (imonth in (1:12)){
#       montly[,,imonth] <- apply(data[,,which(all.days.hours == imonth)],c(1,2),mean,na.rm = TRUE)
#
#       # if (cvar == "tmp"){
#       #   montly.min[,,imonth] <- apply(data[,,which(all.days.hours == imonth)],c(1,2),min,na.rm = TRUE)
#       #   montly.max[,,imonth] <- apply(data[,,which(all.days.hours == imonth)],c(1,2),max,na.rm = TRUE)
#       #
#       #   montly.min[!is.finite(montly.min)] <- NA
#       #   montly.max[!is.finite(montly.max)] <- NA
#       #
#       # }
#
#     }
#
#
#     cdf <- reshape2::melt(values) %>%
#       rename(lon = Var1,
#              lat = Var2) %>%
#       mutate(lon = lons[lon],
#              lat = lats[lat]) %>% filter(!is.na(value)) %>%
#       rename(!!cvar := value)
#
#     cdf.monthly <- reshape2::melt(montly) %>%
#       rename(lon = Var1,
#              lat = Var2,
#              month = Var3) %>%
#       mutate(lon = lons[lon],
#              lat = lats[lat]) %>% filter(!is.na(value)) %>%
#       rename(!!cvar := value)
#
#     # if (cvar == "tmp"){
#     #   cdf.monthly.min <- reshape2::melt(montly.min) %>%
#     #     rename(lon = Var1,
#     #            lat = Var2,
#     #            month = Var3) %>%
#     #     mutate(lon = lons[lon],
#     #            lat = lats[lat]) %>% filter(!is.na(value)) %>%
#     #     rename(tmp = value)
#     #
#     #   cdf.monthly.max <- reshape2::melt(montly.max) %>%
#     #     rename(lon = Var1,
#     #            lat = Var2,
#     #            month = Var3) %>%
#     #     mutate(lon = lons[lon],
#     #            lat = lats[lat]) %>% filter(!is.na(value)) %>%
#     #     rename(tmp := value)
#     #
#     #   cdf.monthly[["tmp.min"]] <- cdf.monthly.min[["tmp"]]
#     #   cdf.monthly[["tmp.max"]] <- cdf.monthly.max[["tmp"]]
#     # }
#
#     if (cvar == vars[1]){
#       cyr.df <- cdf
#       cmonth.df <- cdf.monthly
#     } else {
#       cyr.df <- cyr.df %>%
#         left_join(cdf,
#                   by = c("lat","lon"))
#
#       cmonth.df <- cmonth.df %>%
#         left_join(cdf.monthly,
#                   by = c("lat","lon","month"))
#     }
#
#
#
#     # system2("rm",nc.file)
#
#   }
#
#   df.all <- bind_rows(list(df.all,
#                            cyr.df %>% mutate(year = cyear)))
#
#   df.all.monthly <- bind_rows(list(df.all.monthly,
#                                    cmonth.df %>% mutate(year = cyear)))
#
# }
#
# saveRDS(df.all.monthly,
#         "./outputs/CRU.JRA.RDS")
# stop()

# system2("scp",paste("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/CRU.JRA.1991.2020.RDS",
#                     "/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRU.JRA.1991.2020.RDS"))

df.all <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRU.JRA.1901.1910.RDS")

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


# transform to sf objects
psf   <- sf::st_as_sf(sp) %>%
  dplyr::mutate(ID_point = 1:dim(.)[1])
polsf <- sf::st_as_sf(plotbiomes::Whittaker_biomes_poly)

# remove points inside polygons
in_points  <- lengths(sf::st_within(psf,polsf))
out_points <- psf[in_points == 0, ]

# find nearest poly
nearest <- polsf[sf::st_nearest_feature(out_points, polsf) ,]  %>%
  dplyr::mutate(id_point = out_points$ID)

df.all.units.sum["biome"] <- e$biome
df.all.units.sum["type"] <- "Interpolation"

df.all.units.sum.extrap <- df.all.units.sum
df.all.units.sum.extrap[["biome"]][nearest$id_point] <- nearest$biome
df.all.units.sum.extrap[["type"]][nearest$id_point] <- "Extrapolation"

ggplot(data = df.all.units.sum.extrap) +
  geom_point(aes(x = tmp, y = pre, color = biome, shape = type)) +
  scale_shape_manual(values=c(1,16)) +
  theme_bw()

df.all.units.sum.extrap %>%
  group_by(type,biome) %>%
  summarise(N = length(pre))

saveRDS(df.all.units.sum,
        "./outputs/biome.1901.1910.RDS")

delta_pr <- 5
delta_t <- 0.5


Whittaker_biomes.text <- Whittaker_biomes %>% group_by(biome) %>%
  summarise(temp_c = mean(temp_c),
            precp_cm = mean(precp_cm),
            .groups = "keep") %>%
  mutate(temp_c = case_when(biome == "Woodland/shrubland" ~ 13,
                            biome == "Temperate seasonal forest" ~ 10,
                            biome == "Temperate rain forest" ~ 15,
                            biome == "Subtropical desert" ~ 25,
                            biome == "Tropical rain forest" ~ 25,
                            biome == "Temperate grassland/desert" ~ 8,
                            biome == "Tropical seasonal forest/savanna" ~ 25,
                            TRUE ~ temp_c),
         precp_cm = case_when(biome == "Tundra" ~ 20,
                              biome == "Woodland/shrubland" ~ 70,
                              biome == "Temperate seasonal forest" ~ 150,
                              biome == "Temperate rain forest" ~ 250,
                              biome == "Subtropical desert" ~ 35,
                              biome == "Temperate grassland/desert" ~ 15,
                              biome == "Tropical rain forest" ~ 325,
                              biome == "Tropical seasonal forest/savanna" ~ 150,
                              TRUE ~ precp_cm),
         biome = case_when(biome == "Tropical seasonal forest/savanna" ~ "Tropical seasonal \r\n forest/savanna",
                           TRUE ~ biome))


df2plot <- df.all.units.sum.extrap %>%
  # filter(type == "Interpolation") %>%
  mutate(tas.cat = round((1/delta_t)*tmp)/(1/delta_t),
         pr.cat = round(pre/delta_pr)*delta_pr)


ggplot(df2plot,
       aes(x=tmp, y=pre) ) +
  geom_bin2d(bins = 100,
             alpha = 0.7) +
  # scale_fill_continuous(type = "viridis") +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, linewidth = 0.5) +
  geom_text(data = Whittaker_biomes.text,
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  labs(x = "Temperature (°C)",
       y = "Precipitation (cm)") +
  facet_wrap(~ type) +
  theme_bw() +
  theme(legend.position = c(0.08,0.75))


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_tile(data = df.all.units.sum %>% filter(!is.na(biome)),
            aes(x = lon, y = lat,
                fill = as.factor(biome)),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  labs(x = "",y = "", fill = "Biome") +
  theme_bw()


ncfile <- "/home/femeunier/Documents/projects/TrENDY.analyses/data/CABLE-POP_S0_oceanCoverFrac.nc"
nc <- nc_open(ncfile)
lons <- ncvar_get(nc,"longitude")
lats <- ncvar_get(nc,"latitude")
oceanCoverFrac <- ncvar_get(nc,"oceanCoverFrac")
nc_close(nc)

df.area <-melt(oceanCoverFrac)  %>%
  rename(lon = Var1,
         lat = Var2,
         OceanCoverFrac = value) %>%
  mutate(lon = lons[lon],
         lat = lats[lat],
         landCoverFrac = 1 - OceanCoverFrac) %>%
  mutate(lon = case_when(lon > 180 ~ (lon - 360),
                         TRUE ~ lon))

ggplot(data = world) +
  geom_tile(data = df.area,
            aes(x = lon, y = lat,
                fill = landCoverFrac),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  labs(x = "",y = "") +
  theme_bw()

old.df <- expand.grid(lon = seq(-179.75,180,0.5),
                      lat = seq(-89.75,90,0.5))
biome.rst <- rasterFromXYZ(old.df %>% mutate(tmp = 1))
plot(biome.rst)

# old.df %>%
#   filter(lat == -55.25,
#          lon == 70.25)

dfr.area <- raster(SpatialPixelsDataFrame(points = df.area[c("lon","lat")],
                                          data = df.area["landCoverFrac"],
                                          tolerance = 1e-5))

dfr.area.rspld <- resample(dfr.area,biome.rst)
# dfr.area.rspld[is.na(biome.rst)] <- NA

new.df <- as.data.frame(dfr.area.rspld,
                        xy = TRUE) %>%
  rename(lon = x,
         lat = y) %>%
  filter(!is.na(landCoverFrac))


Gridarea <- RCMIP5:::calcGridArea(lon = as.vector(unique(new.df$lon)),
                                  lat = as.vector(unique(new.df$lat))) %>%
  melt() %>% mutate(Var1 = (as.vector(unique(new.df$lon)))[Var1],
                    Var2 = (as.vector(unique(new.df$lat)))[Var2]) %>%
  rename(lon = Var1,
         lat = Var2,
         GridArea = value) %>%
  mutate(GridArea = GridArea/1e6)

new.df <- new.df %>%
  left_join(Gridarea,
            by = c("lat","lon")) %>%
  mutate(fracArea = GridArea*landCoverFrac)


# new.df  %>%
#   filter(lat == -55.25)

df.bar <- df.all.units.sum[,c("lon","lat","biome","type")] %>%
  left_join(new.df,
            by = c("lat","lon"))

df.bar.sum <- df.bar %>%
  filter(!is.na(biome),
         type == "Interpolation") %>%
  group_by(biome) %>%
  summarise(totArea = sum(fracArea/1e6,na.rm = TRUE),
            .groups = "keep")


ggplot(df.bar.sum) +
  geom_bar(aes(x = biome, fill = biome,
               y = totArea), stat = "identity", show.legend = TRUE) +
  labs(x = "", y = "Area (millions of km²)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 20)) +
  guides(fill = "none")

sum(df.bar.sum$totArea)

# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/define.biomes.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
