# rm(list = ls())
# dev.off()
# require(rgdal)
#
shape <- readOGR(dsn = "~/Downloads/Ibof/", layer = "rwdb_riv")
#
# options(sf_max.plot=1)
# CB <- readOGR(dsn = "/home/femeunier/Desktop/FWO/",layer = "congo_basin_polyline")
# plot(CB)
# plot(shape, col = "blue",add = TRUE)


rm(list = ls())

library(rgdal)
library(sf)
library(ggplot2)
library(raster)

# my_spdf <- readOGR(dsn= paste0(getwd(),"/home/femeunier/Desktop/FWO/"),
#                    layer="CongoBasin",
#                    verbose=FALSE)

CongoBasin <- st_read("/home/femeunier/Desktop/FWO/congo_basin_polyline.shp")

ggplot() +
  geom_sf(
    data = CongoBasin,
    size = 0.5,
    color = "black",
    fill = NA
  )

world <- map_data("world")

ggplot() +
  geom_map(
    data = world,map = world,aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1) +
  geom_sf(data = CongoBasin,size = 0.5,color = "black",fill = NA) +
  labs(x = "",y = "") +
  coord_sf() +
  theme_void() +
  scale_x_continuous(limits = c(10,35)) +
  scale_y_continuous(limits = c(-15,10)) +
  theme(legend.position = "none")


str_name<-'/home/femeunier/Desktop/FWO/ESA_tropical_africa_LC.tif'
imported_raster=raster(str_name)


getmode <- function(v,...) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# r_up_0.5 <- aggregate(imported_raster, fact = 1/res(imported_raster),fun = getmode)

mod_raster <- reclassify(imported_raster,t(matrix(c(10,1,
                                                    11,1,
                                                    12,1,
                                                    20,1,
                                                    30,1,
                                                    40,1,
                                                    50,2,
                                                    60,3,
                                                    61,3,
                                                    62,3,
                                                    70,2,
                                                    80,3,
                                                    90,4,
                                                    100,5,
                                                    110,5,
                                                    120,5,
                                                    122,5,
                                                    130,6,
                                                    140,7,
                                                    150,6,
                                                    151,6,
                                                    152,6,
                                                    153,6,
                                                    160,10,
                                                    170,10,
                                                    180,10,
                                                    190,8,
                                                    200,6,
                                                    201,6,
                                                    202,6,
                                                    210,9),
                                                  nrow = 2)))
# 1 = Grass/Cropland, 2 = Evergreen, 3 = Dry forest, 4 = Needleleaves, 5 = bare-ground,6 = water
# Unique:10  11  12  20  30  40  50  60  61  62  70  80  90 100 110 120 122 130 140 150 151 152 153 160 170 180 190 200 201 202 210
# Marijn: 1 = grey, dark green = 2, green = 3, brown-green = 4, brown = 5, yellow = 6, rose = 7, red = 8, white = 9


mod_raster_upscaled <- aggregate(mod_raster, fact = 0.25/res(mod_raster), fun = modal, na.rm = TRUE)

temp <- as.data.frame(mod_raster_upscaled, xy = T)



ggplot()+
  geom_tile(data = temp %>%
              dplyr::filter(ESA_tropical_africa_LC != 9),
            aes(x = x, y = y,
                fill = as.factor(ESA_tropical_africa_LC)),
            colour = "black") +
  geom_map(data = world,
           map = world,
           aes(long, lat, map_id = region),
           color = "black", fill = NA, size = 0.5) +
  scale_fill_manual(values = c("#b9bab5","#005401","#448704","#798003",
                               "#c49402","#fff1c0","white","brown")) +
  # geom_sf(data = CongoBasin,size = 0.5,color = "red",fill = NA) +
  theme_void() +
  theme(panel.grid.major = element_blank()) +
  scale_x_continuous(limits = c(-21,46),expand = c(0,0)) +
  scale_y_continuous(limits = c(-15,20),expand = c(0,0)) +
  guides(fill = "none")

r <- mod_raster_upscaled
A <- st_read("/home/femeunier/Desktop/FWO/CongoBasin.shp")
r2 <- crop(r, extent(A))
r3 <- mask(r2, A)
temp2 <- as.data.frame(r3, xy = T)


sites <- data.frame(lat = c(0+17/60,0.8071,((1+51.5/60)+ (0+26/60))/2,0.4862, 7.135716,0.74621),
                    lon = c(25+18/60,24.4530,((19+41/60) + (23+32/60))/2,30.3897,-1.887314,26.22101),
                    names = c("Yoko","Yangambi","Djolu","Kibale","Abafour","Bafwamogo"),
                    type = c("Chronosequence","Fluxtower","Chronosequence","Chronosequence","Chronosequence","Chronosequence")) %>%
  dplyr::filter(names != "Kibale")



ggplot() +
  geom_tile(data = temp2 %>%
              dplyr::filter(ESA_tropical_africa_LC != 9),
            aes(x = x, y = y,
                fill = as.factor(ESA_tropical_africa_LC)), alpha = 0.7) +
  geom_path(data = shape,
            aes(x=long, y=lat, group=group), color = "darkblue") +

  scale_fill_manual(values = c("#b9bab5","#005401","#448704","#798003",
                               "#c49402","black","white")) +
  geom_point(data = sites %>% dplyr::filter(names != "Abafour"),
             aes(x = lon, y = lat), size = 2,
             color = "black", fill = "black") +

  geom_sf(data = CongoBasin,size = 0.5,color = "black",fill = NA) +
  theme_void() +
  theme(panel.grid.major = element_blank()) +
  scale_x_continuous(limits = c(11,34.1),expand = c(0,0)) +
  scale_y_continuous(limits = c(-13.5,9.5),expand = c(0,0)) +
  guides(fill = "none")


