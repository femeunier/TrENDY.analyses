rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(lubridate)
library(plotbiomes)
library(ggplot2)
library(tidyr)
library(YGB)
library(raster)
library(RColorBrewer)

maindir <- "/data/gent/vo/000/gvo00074/felicien/TrENDY/data"
maindir <- "/home/femeunier/Documents/projects/TrENDY.analyses/data/"
model.names <- c("ORCHIDEE-CNP","JULES-ES-1p0","JSBACH")
model.dir <- c("","","")
scenarios <- c("S0","S1")

########################################################################
# For reading
npp.names <- c("npp","npp_nlim")
lat.names <- c("latitude","lat","lat_FULL")
lon.names <- c("longitude","lon","lon_FULL")
time.names <- c("time","time_counter")

#######################################################################
# For regridding
biome <- readRDS("./outputs/biome.RDS")
biome.rst <- rasterFromXYZ(biome[,c("lon","lat","tmp")])

#######################################################################

all.df <- data.frame()

for (imodel in seq(1,length(model.dir))){

  print(paste0("",model.names[imodel]))
  for (iscenario in seq(1,length(scenarios))){

    print(paste0("- ",scenarios[iscenario]))

    cfile <- file.path(maindir,
                       file_name <- paste(cmodel <- model.names[imodel],
                                          cscenario <- scenarios[iscenario],
                                          "npp.nc",
                                          sep = "_"))

    nc <- nc_open(cfile)

    lats <- NULL ; i = 1
    while(is.null(lats) & i <= length(lat.names)){
      lats <- tryCatch(ncvar_get(nc,lat.names[i]),
                       error = function(e) NULL)
      i = i +1
    }

    lons <- NULL ; i = 1
    while(is.null(lons) & i <= length(lon.names)){
      lons <- tryCatch(ncvar_get(nc,lon.names[i]),
                       error = function(e) NULL)
      i = i +1
    }

    times <- NULL ; i = 1
    while(is.null(times) & i <= length(time.names)){
      times <- tryCatch(ncvar_get(nc,time.names[i]),
                       error = function(e) NULL)

      if (!is.null(times)) unit.time <- strsplit(ncatt_get(nc,time.names[i],"units")[["value"]], "\\s+")[[1]]

      i = i + 1
    }

    npp <- NULL ; i = 1
    while(is.null(npp) & i <= length(npp.names)){
      npp <- tryCatch(ncvar_get(nc,npp.names[i]),
                      error = function(e) NULL)
      i = i +1
    }

    nc_close(nc)


    years <- year(unit.time[3]) + (yday(unit.time[3]))/365 +
      hour(paste(unit.time[3],unit.time[4]))/24/365  + times * udunits2::ud.convert(1,unit.time[1],"days")/365  # approximate years

    actual.years <- min(floor(years)) + ((1:length(years)) - 0.5)/12

    accurate.years <- floor(actual.years)

    # all.years <- floor(years)
    select <- which(accurate.years>1988)
    min.select <- min(select)
    max.select <- ifelse(length(select)%%12,max(select),max(select)-length(select)%%12)

    times.selected <- times[min.select:max.select]

    # accurate.years[min.select:max.select]
    # select <- which(years>1988)
    #
    # if (length(select) == 0) next()
    #

    # years.select <- floor(years[select])
    # table(years.select)

    cdf <- melt(npp[,,min.select:max.select]) %>%
      rename(lon = Var1,
             lat = Var2,
             time = Var3,
             npp = value) %>%
      mutate(lon = lons[lon],
             lat = lats[lat],
             time = times.selected[time]) %>%
      group_by(lat,lon) %>%
      mutate(year =  accurate.years[min.select:max.select]) %>%
      group_by(lat,lon,year) %>%
      mutate(month = 1:12) %>%
      filter(!is.na(npp))

    cdf.sum <- cdf %>%
      group_by(lon,lat,year) %>%
      summarise(npp.m = mean(npp,na.rm = TRUE),
                .groups = "keep") %>%
      filter(!is.na(npp.m))

    cdf.sum.sum <- cdf.sum %>%
      filter(year > (max(year) - 30)) %>%
      group_by(lon,lat) %>%
      summarise(npp.m = mean(npp.m,na.rm = TRUE),
                .groups = "keep")

    cdf.rspld <- resample.df(cdf.sum.sum,"npp.m",biome.rst)

    all.df <- bind_rows(list(all.df,
                             cdf.rspld %>% mutate(model = model.names[imodel],
                                                    scenario = scenarios[iscenario])
                             ))

  }
}

cdf.biome <- all.df %>% left_join(biome,
                                  by = c("lat","lon"))


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_tile(data = cdf.biome,
            aes(x = lon, y = lat,
                fill = npp.m),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "") +
  facet_grid(model ~ scenario) +
  theme_bw()

cdf.biome.wide <- cdf.biome %>%
  dplyr::filter(!is.na(npp.m)) %>%
  pivot_wider(names_from = scenario,
              values_from = npp.m) %>%
  filter(S0 > 0) %>%
  mutate(diff = S1 - S0,
         RR = S1/S0,
         lnRR = log(S1/S0))


cdf.biome.wide.filter <- cdf.biome.wide %>%
  left_join(cdf.biome.wide %>%
              group_by(model,biome) %>%
              summarise(mS0 = mean(S0),
                        .groups = "keep"),
            by = c("model","biome")) %>%
  dplyr::filter(S0 > (1e-3*mS0))



ggplot(data = cdf.biome.wide.filter %>%
         dplyr::filter(!is.na(biome))) +
  geom_boxplot(aes(x = as.factor(biome),
                   y = diff,
                   fill = model),outlier.shape = NA) +
  theme_bw() +
  labs(x = "",
       y = "NPP difference \r\n (hist - cCO2)",
       fill = "Model") +
  scale_y_continuous(limits = c(0,1.25e-8)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

polys <- geometry(plotbiomes::Whittaker_biomes_poly)

delta_pr <- 50
delta_t <- 1

Trendy.wide.sum <-
  cdf.biome.wide.filter %>%
  filter(!is.na(biome)) %>%
  mutate(tas.cat = round((1/delta_t)*tmp)/(1/delta_t),
         pr.cat = round(pre/delta_pr)*delta_pr) %>%
  group_by(model,tas.cat,pr.cat) %>%
  summarise(diff_npp.m = mean(diff,na.rm = TRUE),
            diff_npp.rel.m = 100*mean(diff/S0,na.rm = TRUE),
            lnRR_npp.m = mean(lnRR,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(diff_npp.rel.m.bnd = case_when(diff_npp.rel.m > 100 ~ 100,
                                        diff_npp.rel.m < -100 ~ -100,
                                        TRUE ~ diff_npp.rel.m))

# ggplot() +
#   geom_tile(data = Trendy.wide.sum,
#             aes(x = tas.cat, y = pr.cat,
#                 fill = diff_npp.m),na.rm = TRUE, alpha = 1) +
#   labs(x = "",y = "") +
#   facet_wrap(~ model, scales = "free") +
#   geom_polygon(data = Whittaker_biomes,
#                aes(x = temp_c, y = precp_cm, group = biome),
#                color = "black",
#                fill = NA, size = 0.5) +
#   scale_fill_gradient2(low = "red",mid = "white",high = "darkgreen",na.value = "transparent") +
#   geom_text(data =Whittaker_biomes %>% group_by(biome) %>%
#               summarise(temp_c = mean(temp_c),
#                         precp_cm = mean(precp_cm),
#                         .groups = "keep"),
#             aes(x = temp_c, y = precp_cm, label = biome),
#             size = 3.5)  +
#   labs(x = "MAT (°C)",y = "MAP (cm)", fill = "NPP difference \r\n (cCO2 - hist) \r\n [kg/m²/yr]") +
#   theme_bw()

tas.cat <- seq(min(Trendy.wide.sum$tas.cat),
               max(Trendy.wide.sum$tas.cat),delta_t)

pr.cat <- seq(min(Trendy.wide.sum$pr.cat),
              max(Trendy.wide.sum$pr.cat),delta_pr)

models <- unique(Trendy.wide.sum$model)

df.grid <- expand.grid(tas.cat, pr.cat, models) %>%
  rename(tas.cat = Var1,
         pr.cat = Var2,
         model = Var3) %>%
  left_join(
    Trendy.wide.sum %>% dplyr::select(tas.cat, pr.cat, model, lnRR_npp.m,diff_npp.m),
    by = c("tas.cat", "pr.cat", "model"))

# ggplot() +
#   geom_contour_filled(data = df.grid,
#                       aes(x = tas.cat, y = pr.cat, z= diff_npp.m)) +
#   # scale_fill_distiller(palette = "Spectral", direction = -1) +
#   facet_wrap(~ model, scales = "free") +
#   geom_polygon(data = Whittaker_biomes,
#                aes(x = temp_c, y = precp_cm, group = biome),
#                color = "black",
#                fill = NA, size = 0.5) +
#   theme_bw()


#############################################################################
# Polisher

tas.cat.small <- seq(min(floor(Whittaker_biomes$temp_c))-1,
                     max(ceiling(Whittaker_biomes$temp_c))+1,0.01)

pr.cat.small <- seq(min(floor(Whittaker_biomes$precp_cm))-1,
                    max(ceiling(Whittaker_biomes$precp_cm))+1,0.1)

df.grid.small <- expand.grid(tas.cat.small, pr.cat.small) %>%
  rename(temp_c = Var1,
         precp_cm = Var2) %>%
  mutate(point.id = 1:(length(tas.cat.small)*length(pr.cat.small)))

sp = SpatialPoints(df.grid.small[,c(1,2)])
e <- as.data.frame((raster::extract(polys,sp)))%>%
  rename(point.id = id.y,
         biome.id = id.x) %>%
  left_join(df.grid.small,
            by = "point.id")

rst.small <- rasterFromXYZ(e[,c("temp_c","precp_cm","biome.id")])

df.perfect <- data.frame()

for (cmodel in models){

  print(cmodel)

  rst.large <- rasterFromXYZ(df.grid %>% filter(model == cmodel,
                                                !is.na(lnRR_npp.m)) %>%
                               dplyr::select(tas.cat,pr.cat,lnRR_npp.m))

  rst.large2 <- rasterFromXYZ(df.grid %>% filter(model == cmodel,
                                                !is.na(diff_npp.m)) %>%
                               dplyr::select(tas.cat,pr.cat,diff_npp.m))

  rst.rspld <- resample(rst.large,rst.small,method = "bilinear")
  rst.rspld2 <- resample(rst.large2,rst.small,method = "bilinear")

  # my_window <- extent( -15, 30,0, 400)
  # # my_window <- extent( 18, 20,300, 320)
  # plot(my_window, col=NA)
  # plot(rst.large,add = TRUE)

  # now we cut
  test <- as.data.frame(rst.rspld,xy = TRUE) %>%
    rename(temp_c = x,
           precp_cm = y) %>%
    mutate(temp_c = round(100*temp_c)/100,
           precp_cm = round(10*precp_cm)/10) %>%
    left_join(e ,
              by = c("temp_c","precp_cm")) %>%
    filter(!is.na(biome.id)) %>%
    left_join(as.data.frame(rst.rspld2,xy = TRUE) %>%
                rename(temp_c = x,
                       precp_cm = y) %>%
                mutate(temp_c = round(100*temp_c)/100,
                       precp_cm = round(10*precp_cm)/10),
              by = c("temp_c","precp_cm"))


  test2plot <- test %>% mutate_all(imputeTS::na_interpolation)

  df.perfect  <- bind_rows(list(df.perfect,
                                test2plot %>% mutate(model = cmodel)))

}

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


ggplot() +
  geom_contour_filled(data = df.perfect,
                      aes(x = temp_c, y = precp_cm, z= lnRR_npp.m),
                      alpha = 0.4,
                      bins = 9) +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, size = 0.5) +
  geom_text(data = Whittaker_biomes.text,
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  scale_fill_manual(values = brewer.pal(9, "Greens")) +
  labs(x = "Temperature (°C)",
       y = "Precipitation (cm)",
       fill = "lnRR") +
  facet_wrap(~ model) +
  theme_bw() +
  theme(legend.position = c(0.08,0.75))


df.perfect.sum <- df.perfect %>%
  group_by(temp_c,precp_cm,biome.id) %>%
  summarise(lnRR_npp.m.m = mean(lnRR_npp.m,na.rm = TRUE),
            lnRR_npp.m.CV = 100*sd(lnRR_npp.m,na.rm = TRUE)/mean(lnRR_npp.m,na.rm = TRUE),
            diff_npp.m.m = mean(diff_npp.m,na.rm = TRUE),
            .groups = "keep")

# df.perfect %>% filter(temp_c == temp_c[1],
#                       precp_cm == precp_cm[1])


ggplot() +
  geom_contour_filled(data = df.perfect.sum,
                      aes(x = temp_c, y = precp_cm, z= lnRR_npp.m.m),
                      alpha = 0.4,
                      bins = 9) +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, size = 0.5) +
  geom_text(data = Whittaker_biomes.text,
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  scale_fill_manual(values = brewer.pal(9, "Greens")) +
  labs(x = "Temperature (°C)",
       y = "Precipitation (cm)",
       fill = "lnRR") +
  theme_bw() +
  theme(legend.position = c(0.08,0.75),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

ggsave(plot = last_plot(),filename = "./Figures/Mean.png",
       dpi = 300,unit = "cm",width = 25,height = 14)

ggplot() +
  geom_contour_filled(data = df.perfect.sum,
                      aes(x = temp_c, y = precp_cm, z= lnRR_npp.m.CV),
                      alpha = 0.4,
                      bins = 9) +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, size = 0.5) +
  geom_text(data = Whittaker_biomes.text,
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  scale_fill_manual(values = brewer.pal(9, "Purples")) +
  labs(x = "Temperature (°C)",
       y = "Precipitation (cm)",
       fill = "CV lnRR (%)") +
  theme_bw() +
  theme(legend.position = c(0.08,0.75),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(fill = "none")

ggsave(plot = last_plot(),filename = "./Figures/CV_nokegend.png",
       dpi = 300,unit = "cm",width = 25,height = 14)
