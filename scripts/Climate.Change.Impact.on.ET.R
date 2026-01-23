rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(TrENDY.analyses)

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

TrendyET <- readRDS("./outputs/Trendy.global.evapotrans.v11.RDS") %>%
  dplyr::filter((model %in% c("CABLE-POP","CLASSIC","CLM5.0",
                              "DLEM","IBIS","JSBACH",
                              "JULES","LPJ-GUESS","ORCHIDEE",
                              "SDGVM","VISIT-NIES"))) # Model with everything

TrendyET.rspld <- resample.df.all.col(bigdf = TrendyET,
                                       raster2resample = biome.rst,
                                       var.names = "value")


TrendyET.wide <- TrendyET.rspld %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  filter(!(is.na(S0) & is.na(S1) & is.na(S2))) %>%
  left_join(Mask.CC %>% dplyr::select(lat,lon,biome_norm,keep,type.of.change,MAP,tmp,
                                      Delta_x,Delta_x2) %>%
              rename(biome = biome_norm),
            by = c("lat","lon")) %>%
  mutate(lnRR = log(S2/S1/Delta_x),
         diff = (S2 - S1)/Delta_x)    # Or Delta_x2

ggplot(data = TrendyET.wide %>%
         dplyr::filter(keep)) +
  geom_boxplot(aes(x = as.factor(biome),y = diff*86400, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  scale_y_continuous(limits = c(-1,1)*0.4) +
  labs(x = "", y = "ET change (kg/m²/d)") +
  facet_wrap(~ model,nrow = 3,ncol = 4) +
  geom_hline(yintercept = 0,linetype = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggplot(data = TrendyET.wide %>%
         dplyr::filter(keep) %>%
         # dplyr::filter(is.finite(lnRR)) %>%
         dplyr::filter(type.of.change == "temp")) +
  geom_boxplot(aes(x = as.factor(model),y = diff*86400, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  ggtitle("Impact of temp change") +
  scale_y_continuous(limits = c(-1,1)*0.4) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "ET change (kg/m²/d)") +
  facet_wrap(~ biome) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(data = TrendyET.wide %>%
         dplyr::filter(keep) %>%
         # dplyr::filter(is.finite(lnRR)) %>%
         dplyr::filter(type.of.change == "precip")) +
  geom_boxplot(aes(x = as.factor(model),y = diff*86400, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  ggtitle("Impact of precip change") +
  scale_y_continuous(limits = c(-1,1)*0.4) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "ET change (kg/m²/d)") +
  facet_wrap(~ biome) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(data = TrendyET.wide %>%
         dplyr::filter(keep) %>%
         # dplyr::filter(is.finite(lnRR)) %>%
         dplyr::filter(type.of.change == "both")) +
  ggtitle("Impact of precip + temp change") +
  geom_boxplot(aes(x = as.factor(model),y = diff*86400, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  scale_y_continuous(limits = c(-1,1)*0.4) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "ET change (kg/m²/d)") +
  facet_wrap(~ biome) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


TrendyET.wide.av <- TrendyET.wide %>%
  dplyr::filter(keep) %>%
  group_by(biome,type.of.change,lon,lat) %>%
  summarise(lnRR = mean(lnRR,na.rm = TRUE),
            diff.m = mean(diff,na.rm = TRUE),
            .groups = "keep")

ggplot(data = TrendyET.wide.av) +
  geom_boxplot(aes(x = as.factor(biome),y = diff.m*86400, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  scale_y_continuous(limits = c(-1,1)*0.152) +
  labs(x = "", y = "ET change [kgC/m²/d]") +
  facet_wrap(~type.of.change) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

