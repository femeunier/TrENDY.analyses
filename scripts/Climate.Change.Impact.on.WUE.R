rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(TrENDY.analyses)
library(raster)

# system2("rsync",
#         paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.global.npp.rspld_v11.RDS",
#               "./outputs/"))

Mask.CC <- readRDS("./outputs/Map.change.climate.RDS") %>%
  mutate(Delta_x = case_when(type.of.change == "temp" ~ sign(tmp),
                             type.of.change == "precip" ~ sign(MAP),
                             type.of.change == "both" ~ sign(MAP*tmp)),
         Delta_x2 = case_when(type.of.change == "temp" ~ tmp,
                              type.of.change == "precip" ~ MAP,
                              type.of.change == "both" ~ MAP*tmp)) %>%
  mutate(lat = as.numeric(as.vector(lat)),
         lon = as.numeric(as.vector(lon)))

ggplot(data = Mask.CC) +
  geom_bar(aes(x = Delta_x)) +
  facet_wrap(~ type.of.change) +
  theme_bw()

biome <- readRDS("./outputs/biome1961.1990.RDS")
biome.rst <- rasterFromXYZ(biome[,c("lon","lat","tmp")])

###########################################################################


TrendyET <- readRDS("./outputs/Trendy.global.evapotrans.v11.RDS") %>%
  dplyr::filter((model %in% c("CABLE-POP","CLASSIC","CLM5.0",
                              "DLEM","IBIS","JSBACH",
                              "JULES","LPJ-GUESS","ORCHIDEE",
                              "SDGVM","VISIT-NIES"))) # Model with everything

TrendyNPP <- readRDS("./outputs/Trendy.global.npp.v11.RDS") %>%
  dplyr::filter(model %in% TrendyET[["model"]])

Trendy <- TrendyNPP %>%
  dplyr::select(-variable) %>%
  rename(NPP = value) %>%
  left_join(TrendyET %>%
              dplyr::select(-variable) %>%
              rename(ET = value),
            by = c("lat","lon","model","scenario")) %>%
  mutate(value = NPP/ET) %>%
  dplyr::select(-c(NPP,ET))

Trendy.rspld <- resample.df.all.col(bigdf = Trendy,
                                      raster2resample = biome.rst,
                                      var.names = "value")


Trendy.wide <- Trendy.rspld %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  filter(!(is.na(S0) & is.na(S1) & is.na(S2))) %>%
  left_join(Mask.CC %>% dplyr::select(lat,lon,biome_norm,keep,type.of.change,MAP,tmp,
                                      Delta_x,Delta_x2) %>%
              rename(biome = biome_norm),
            by = c("lat","lon")) %>%
  mutate(lnRR = log(S2/S1/Delta_x),
         diff = (S2 - S1)/Delta_x)    # Or Delta_x2

ggplot(data = Trendy.wide %>%
         dplyr::filter(keep)) +
  geom_boxplot(aes(x = as.factor(biome),y = diff, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  scale_y_continuous(limits = c(-1,1)*0.0005) +
  labs(x = "", y = "WUE change") +
  facet_wrap(~ model,nrow = 3,ncol = 4) +
  geom_hline(yintercept = 0,linetype = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggplot(data = Trendy.wide %>%
         dplyr::filter(keep) %>%
         # dplyr::filter(is.finite(lnRR)) %>%
         dplyr::filter(type.of.change == "temp")) +
  geom_boxplot(aes(x = as.factor(model),y = diff, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  ggtitle("Impact of temp change") +
  scale_y_continuous(limits = c(-1,1)*0.0005) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "WUE change") +
  facet_wrap(~ biome) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(data = Trendy.wide %>%
         dplyr::filter(keep) %>%
         # dplyr::filter(is.finite(lnRR)) %>%
         dplyr::filter(type.of.change == "precip")) +
  geom_boxplot(aes(x = as.factor(model),y = diff, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  ggtitle("Impact of precip change") +
  scale_y_continuous(limits = c(-1,1)*0.0005) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "WUE change") +
  facet_wrap(~ biome) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(data = Trendy.wide %>%
         dplyr::filter(keep) %>%
         # dplyr::filter(is.finite(lnRR)) %>%
         dplyr::filter(type.of.change == "both")) +
  ggtitle("Impact of precip + temp change") +
  geom_boxplot(aes(x = as.factor(model),y = diff, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  scale_y_continuous(limits = c(-1,1)*0.0005) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "WUE change") +
  facet_wrap(~ biome) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


Trendy.wide.av <- Trendy.wide %>%
  dplyr::filter(keep) %>%
  group_by(biome,type.of.change,lon,lat) %>%
  summarise(lnRR = mean(lnRR,na.rm = TRUE),
            diff.m = mean(diff,na.rm = TRUE),
            .groups = "keep")

ggplot(data = Trendy.wide.av) +
  geom_boxplot(aes(x = as.factor(biome),y = diff.m*1000, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  scale_y_continuous(limits = c(-1,1)*0.5) +
  labs(x = "", y = "WUE change [gC/kgW") +
  facet_wrap(~type.of.change) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

