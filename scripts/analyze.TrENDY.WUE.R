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
library(TrENDY.analyses)


#######################################################################
# For re-gridding

biome <- readRDS("./outputs/biome1961.1990.RDS")
biome.rst <- rasterFromXYZ(biome[,c("lon","lat","tmp")])

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_tile(data = biome %>% filter(!is.na(biome)),
            aes(x = lon, y = lat,
                fill = biome),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  labs(x = "",y = "", fill = "Biome") +
  theme_bw()

#######################################################################

# system2("rsync",
#         paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.global.evapotrans.v11.RDS",
#               "./outputs/"))

# Test 2 with ET
all.df.ET <- readRDS("./outputs/Trendy.global.evapotrans.v11.RDS") %>%
  dplyr::filter((model %in% c("CABLE-POP","CLASSIC","CLM5.0",
                              "DLEM","IBIS","JSBACH",
                              "JULES","LPJ-GUESS","ORCHIDEE",
                              "SDGVM","VISIT-NIES"))) # Model with everything

all.df.NPP <- readRDS("./outputs/Trendy.global.npp.v11.RDS") %>%
  dplyr::filter(model %in% all.df.ET[["model"]])

all.df <- all.df.NPP %>%
  dplyr::select(-variable) %>%
  rename(NPP = value) %>%
  left_join(all.df.ET %>%
              dplyr::select(-variable) %>%
              rename(ET = value),
            by = c("lat","lon","model","scenario")) %>%
  mutate(value = NPP/ET) %>%
  dplyr::select(-c(NPP,ET))

models <- unique(all.df$model)

# Resampling needed! (not all grids)
all.df.rspld <- data.frame()
for (cmodel in models){
  print(cmodel)
  all.df.rspld <- bind_rows(list(all.df.rspld,
                                 resample.df.all.col(bigdf = all.df %>% filter(model == cmodel),
                                                     raster2resample = biome.rst,
                                                     var.names = "value") %>% mutate(model = cmodel)
  ))
}


# check resampling

cdf.biome <- all.df.rspld %>% left_join(biome,
                                        by = c("lat","lon")) %>%
  filter(!is.na(biome))

ggplot() +
  geom_density(data = all.df %>% filter(!is.na(value),
                                        value != 0.),
               aes(x = value, fill = scenario), alpha = 0.2, color = NA) +
  geom_density(data = all.df.rspld %>% filter(!is.na(value)),
               aes(x = value, color = scenario), alpha = 0.5, fill = NA) +
  facet_wrap(~ model, scales = "free") +
  theme_bw()



cdf.biome.wide <- cdf.biome %>%
  dplyr::filter(!is.na(value)) %>%
  pivot_wider(names_from = scenario,
              values_from = value) %>%
  filter(S0 > 0) %>%
  mutate(diff = S1 - S0,
         RR = S1/S0,
         lnRR = log(S1/S0))

cdf.biome.wide.filter <- cdf.biome.wide %>%
  left_join(cdf.biome.wide %>%
              group_by(model,biome) %>%
              summarise(mS0 = mean(S0),
                        .groups = "keep"),
            by = c("model","biome"))

ggplot(data = cdf.biome.wide.filter %>%
         dplyr::filter(!is.na(biome))) +
  geom_boxplot(aes(x = as.factor(biome),
                   y = S1,
                   fill = model),outlier.shape = NA) +
  theme_bw() +
  labs(x = "",
       y = "WUE",
       fill = "Model") +
  scale_y_continuous(limits = c(0,0.005)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = c(0.1,0.78)) +
  guides(fill=guide_legend(ncol=2))


ggplot(data = cdf.biome.wide.filter %>%
         dplyr::filter(!is.na(biome))) +
  geom_boxplot(aes(x = as.factor(biome),
                   y = diff,
                   fill = model),outlier.shape = NA) +
  theme_bw() +
  labs(x = "",
       y = "WUE change (S1 - S0)",
       fill = "Model") +
  scale_y_continuous(limits = c(-0.00025,0.001)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = c(0.3,0.8)) +
  geom_hline(yintercept = 0, linetype = 2) +
  guides(fill=guide_legend(ncol=2))

polys <- geometry(plotbiomes::Whittaker_biomes_poly)

delta_pr <- 50
delta_t <- 1

Trendy.wide.sum <-
  cdf.biome.wide.filter %>%
  filter(!is.na(biome)) %>%
  mutate(tas.cat = round((1/delta_t)*tmp)/(1/delta_t),
         pr.cat = round(pre/delta_pr)*delta_pr) %>%
  group_by(model,tas.cat,pr.cat) %>%
  summarise(diff_ET.m = mean(diff,na.rm = TRUE),
            diff_ET.rel.m = 100*mean(diff/S0,na.rm = TRUE),
            lnRR_ET.m = mean(lnRR,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(diff_ET.rel.m.bnd = case_when(diff_ET.rel.m > 100 ~ 100,
                                       diff_ET.rel.m < -100 ~ -100,
                                       TRUE ~ diff_ET.rel.m))

ggplot() +
  geom_tile(data = Trendy.wide.sum,
            aes(x = tas.cat, y = pr.cat,
                fill = diff_ET.m),na.rm = TRUE, alpha = 1) +
  labs(x = "",y = "") +
  facet_wrap(~ model, scales = "free") +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, linewidth = 0.5) +
  scale_fill_gradient2(low = "red",mid = "white",high = "darkgreen",na.value = "transparent") +
  geom_text(data =Whittaker_biomes %>% group_by(biome) %>%
              summarise(temp_c = mean(temp_c),
                        precp_cm = mean(precp_cm),
                        .groups = "keep"),
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  labs(x = "MAT (°C)",y = "MAP (cm)", fill = "WUE difference \r\n (hist - cCO2) ") +
  theme_bw()

tas.cat <- seq(min(Trendy.wide.sum$tas.cat),
               max(Trendy.wide.sum$tas.cat),delta_t)

pr.cat <- seq(min(Trendy.wide.sum$pr.cat),
              max(Trendy.wide.sum$pr.cat),delta_pr)

df.grid <- expand.grid(tas.cat, pr.cat, models) %>%
  rename(tas.cat = Var1,
         pr.cat = Var2,
         model = Var3) %>%
  left_join(
    Trendy.wide.sum %>% dplyr::select(tas.cat, pr.cat, model, lnRR_ET.m,diff_ET.m),
    by = c("tas.cat", "pr.cat", "model"))

# ggplot() +
#   geom_contour_filled(data = df.grid,
#                       aes(x = tas.cat, y = pr.cat, z= diff_ET.m)) +
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
                     max(ceiling(Whittaker_biomes$temp_c))+1,0.05)   # 0.01

pr.cat.small <- seq(min(floor(Whittaker_biomes$precp_cm))-1,
                    max(ceiling(Whittaker_biomes$precp_cm))+1,0.5)    # 0.1

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
                                                !is.na(lnRR_ET.m)) %>%
                               dplyr::select(tas.cat,pr.cat,lnRR_ET.m))

  rst.large2 <- rasterFromXYZ(df.grid %>% filter(model == cmodel,
                                                 !is.na(diff_ET.m)) %>%
                                dplyr::select(tas.cat,pr.cat,diff_ET.m))

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
                      aes(x = temp_c, y = precp_cm, z= diff_ET.m),
                      alpha = 0.4,
                      bins = 9) +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, size = 0.5) +
  geom_text(data = Whittaker_biomes.text,
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  scale_fill_manual(values = brewer.pal(9, "RdBu")) +
  labs(x = "Temperature (°C)",
       y = "Precipitation (cm)",
       fill = "Difference WUE") +
  facet_wrap(~ model) +
  theme_bw() +
  theme(legend.position = c(0.9,0.15))


df.perfect.sum <- df.perfect %>%
  group_by(temp_c,precp_cm,biome.id) %>%
  summarise(lnRR_ET.m.m = mean(lnRR_ET.m,na.rm = TRUE),
            lnRR_ET.m.CV = 100*sd(lnRR_ET.m,na.rm = TRUE)/abs(mean(lnRR_ET.m,na.rm = TRUE)),
            diff_ET.m.CV = 100*sd(diff_ET.m,na.rm = TRUE)/abs(mean(diff_ET.m,na.rm = TRUE)),
            diff_ET.m.sd = sd(diff_ET.m,na.rm = TRUE),
            diff_ET.m.m = mean(diff_ET.m,na.rm = TRUE),
            .groups = "keep")

# df.perfect %>% filter(temp_c == temp_c[1],
#                       precp_cm == precp_cm[1])


ggplot() +
  geom_contour_filled(data = df.perfect.sum,
                      aes(x = temp_c, y = precp_cm, z= diff_ET.m.m),
                      alpha = 0.4,
                      bins = 9) +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, size = 0.5) +
  geom_text(data = Whittaker_biomes.text,
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  scale_fill_manual(values = brewer.pal(10, "RdBu")) +
  # scale_fill_gradient2()
  labs(x = "Temperature (°C)",
       y = "Precipitation (cm)",
       fill = "Mean ET difference") +
  theme_bw() +
  theme(legend.position = c(0.08,0.75),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

ggplot() +
  geom_contour_filled(data = df.perfect.sum,
                      aes(x = temp_c, y = precp_cm, z= diff_ET.m.sd),
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
       fill = "SD of the difference") +
  theme_bw() +
  theme(legend.position = c(0.1,0.7),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

