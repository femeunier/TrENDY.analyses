rm(list = ls())

library(plotbiomes)

models <- TrENDY.analyses::get.model.names.TRENDY()
# models <- "ISAM"

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

biome <- readRDS("./outputs/biome1961.1990.RDS")
biome.rst <- rasterFromXYZ(biome[,c("lon","lat","tmp")])

tas.cat.small <- seq(min(floor(Whittaker_biomes$temp_c))-1,
                     max(ceiling(Whittaker_biomes$temp_c))+1,0.05)   # 0.01

pr.cat.small <- seq(min(floor(Whittaker_biomes$precp_cm))-1,
                    max(ceiling(Whittaker_biomes$precp_cm))+1,0.5)    # 0.1

df.grid.small <- expand.grid(tas.cat.small, pr.cat.small) %>%
  rename(temp_c = Var1,
         precp_cm = Var2) %>%
  mutate(point.id = 1:(length(tas.cat.small)*length(pr.cat.small)))

polys <- geometry(plotbiomes::Whittaker_biomes_poly)
sp <- SpatialPoints(df.grid.small[,c(1,2)])
e <- as.data.frame((raster::extract(polys,sp)))%>%
  rename(point.id = id.y,
         biome.id = id.x) %>%
  left_join(df.grid.small,
            by = "point.id")

rst.small <- rasterFromXYZ(e[,c("temp_c","precp_cm","biome.id")])
df.perfect <- data.frame()

for (cmodel in models){

  print(cmodel)

  OPfile <- paste0("./outputs/Trendy.Global.",cmodel,".cAGB.v11.feather")

  if (!file.exists(OPfile)) next()

  raw.data <- read_feather(OPfile)
  df.model.wide <- raw.data  %>%
    filter(year >= 1990) %>%
    filter(!is.na(cVeg)) %>%
    rowwise() %>%
    mutate(cAGB = sum(c(cVeg,-cRoot),na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::select(-c(cVeg,cRoot)) %>%
    pivot_wider(names_from = "scenario",
                values_from = "cAGB")


  if(!all(c("S0","S1") %in% colnames(df.model.wide))){
    next()
  } else if (all(is.na(df.model.wide$S0)) | all(is.na(df.model.wide$S1))){
    next()
  }

  df.model.wide.rspld <- resample.df.all.col(bigdf = df.model.wide,
                                             raster2resample = biome.rst,
                                             var.names = c("S0","S1","S2"))

  ggplot(data = world) +
    geom_raster(data = df.model.wide %>%
                filter(year == 1905),
              aes(x = lon, y = lat,
                  fill = S1),na.rm = TRUE, alpha = 1) +
    geom_sf(fill = NA) +
    labs(x = "",y = "", fill = "Biome") +
    # facet_wrap(~ scenario) +
    theme_bw()

  # ggplot(data = world) +
  #   geom_tile(data = df.model.wide %>%
  #               filter(year == 1905,
  #                      scenario == "S0"),
  #             aes(x = lon, y = lat,
  #                 fill = cRoot),na.rm = TRUE, alpha = 1) +
  #   geom_sf(fill = NA) +
  #   labs(x = "",y = "", fill = "Biome") +
  #   facet_wrap(~ scenario) +
  #   theme_bw()

  cdf.biome <- df.model.wide.rspld %>% left_join(biome,
                                                 by = c("lat","lon")) %>%
    filter(!is.na(biome)) %>%
    filter(!(is.na(S0) & is.na(S1) & is.na(S2)),
           !((S0 == 0) & (S1 == 0) & (S2 == 0)))

  ggplot(data = cdf.biome) +
    geom_boxplot(aes(x = biome, fill = biome,
                     y = 100*(S1 - S0)/S0),outlier.shape = NA) +
    scale_y_continuous(limits = c(-100,100)) +
    theme_bw()

  delta_pr <- 50
  delta_t <- 1

  Trendy.wide.sum <-
    cdf.biome %>%
    mutate(diff = S1 - S0,
           lnRR = log(S1/S0)) %>%
    filter(!is.na(biome)) %>%
    mutate(tas.cat = round((1/delta_t)*tmp)/(1/delta_t),
           pr.cat = round(pre/delta_pr)*delta_pr) %>%
    group_by(model,tas.cat,pr.cat) %>%
    summarise(diff.m = mean(diff,na.rm = TRUE),
              diff.rel.m = 100*mean(diff/S0,na.rm = TRUE),
              lnRR.m = mean(lnRR,na.rm = TRUE),
              .groups = "keep")

  tas.cat <- seq(min(Trendy.wide.sum$tas.cat),
                 max(Trendy.wide.sum$tas.cat),delta_t)

  pr.cat <- seq(min(Trendy.wide.sum$pr.cat),
                max(Trendy.wide.sum$pr.cat),delta_pr)

  df.grid <- expand.grid(tas.cat, pr.cat, models) %>%
    rename(tas.cat = Var1,
           pr.cat = Var2,
           model = Var3) %>%
    left_join(
      Trendy.wide.sum %>% dplyr::select(tas.cat, pr.cat, model,diff.m,diff.rel.m),
      by = c("tas.cat", "pr.cat", "model"))

  rst.large <- rasterFromXYZ(df.grid %>% filter(model == cmodel,
                                                !is.na(diff.m)) %>%
                               dplyr::select(tas.cat,pr.cat,diff.m))

  rst.large2 <- rasterFromXYZ(df.grid %>% filter(model == cmodel,
                                                 !is.na(diff.rel.m)) %>%
                                dplyr::select(tas.cat,pr.cat,diff.rel.m))

  rst.rspld <- resample(rst.large,rst.small,method = "bilinear")
  rst.rspld2 <- resample(rst.large2,rst.small,method = "bilinear")

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


  test2plot <- test %>%
    mutate_all(imputeTS::na_interpolation)

  df.perfect  <- bind_rows(list(df.perfect,
                                test2plot %>%
                                  mutate(model = cmodel)))

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


df.perfect.m <- df.perfect %>%
  group_by(temp_c,precp_cm) %>%
  summarise(diff.m.m = mean(diff.m,na.rm = TRUE),
            diff.m.sd = sd(diff.m,na.rm = TRUE),
            .groups = "keep")


ggplot() +
  geom_contour_filled(data = df.perfect %>%
                        mutate(diff.m.bnd = case_when(diff.m > 5 ~ 5,
                                                      diff.m < -2 ~ -2,
                                                      TRUE ~ diff.m)),
                      aes(x = temp_c, y = precp_cm, z= diff.m.bnd),
                      alpha = 0.4,
                      bins = 9) +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, size = 0.5) +
  geom_text(data = Whittaker_biomes.text,
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  scale_color_brewer(pal = "Spectral") +
  labs(x = "Temperature (°C)",
       y = "Precipitation (cm)",
       fill = "AGB difference (kgC/m²)") +
  facet_wrap(~ model) +
  theme_bw() +
  theme(legend.position = c(0.9,0.1))


ggplot() +
  geom_contour_filled(data = df.perfect.m,
                      aes(x = temp_c, y = precp_cm, z= diff.m.m),
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
       fill = "AGB difference (kgC/m²)") +
  # facet_wrap(~ model) +
  theme_bw() +
  theme(legend.position = c(0.08,0.75))


ggplot() +
  geom_contour_filled(data = df.perfect.m,
                      aes(x = temp_c, y = precp_cm, z= diff.m.sd),
                      alpha = 0.4,
                      bins = 9) +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, size = 0.5) +
  geom_text(data = Whittaker_biomes.text,
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  scale_fill_manual(values = brewer.pal(9, "Reds")) +
  labs(x = "Temperature (°C)",
       y = "Precipitation (cm)",
       fill = "AGB difference (kgC/m²)") +
  # facet_wrap(~ model) +
  theme_bw() +
  theme(legend.position = c(0.08,0.75))
