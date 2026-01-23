rm(list = ls())

library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

df.spinup <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRU.JRA.1901.1910.RDS")
df.climate <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRU.JRA.1961.1990.RDS")
df.present <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRU.JRA.1991.2020.RDS")


df.all <- bind_rows(list(
  df.spinup %>% mutate(type = "init"),
  df.climate %>% mutate(type = "norm"),
  df.present %>% mutate(type = "present")))

df.all.av <- df.all %>%
  group_by(lat,lon,type) %>%
  summarise(MAP = mean(pre*4*365/10,na.rm = TRUE),
            tmp = mean(tmp,na.rm = TRUE) - 273.15,
            .groups = "keep") %>%
  distinct()

# Add biomes
coords <- cbind(id = 1:nrow(df.all.av),
                df.all.av$tmp,
                df.all.av$MAP)
biome.id <- (plotbiomes::Whittaker_biomes) %>% dplyr::select(biome_id,biome) %>% distinct() %>% mutate(id = 1:9)
polys <- geometry(plotbiomes::Whittaker_biomes_poly)
sp <- SpatialPoints(coords[,c(2,3)])
e <- as.data.frame(raster::extract(polys,sp)) %>%
  rename(point.id = id.y,
         id = id.x) %>%
  left_join(biome.id %>% dplyr::select(-biome_id),
            by = "id")

df.all.av["biome"] <- e$biome

# Compute change
df.all.av.wide <-
  df.all.av %>%
  pivot_wider(names_from = type,
              values_from = c(MAP,tmp,biome)) %>%
  mutate(diff_MAP_norm = MAP_norm - MAP_init,
         diff_tmp_norm = tmp_norm - tmp_init,
         diff_MAP_present = MAP_present - MAP_init,
         diff_tmp_present = tmp_present - tmp_init)

df.all.av.wide.long <- df.all.av.wide %>%
  pivot_longer(cols = c(diff_MAP_norm,diff_tmp_norm,
                        diff_MAP_present,diff_tmp_present),
               values_to = "value",
               names_to = "var") %>%
  group_by(var) %>%
  mutate(value.rel = -1 + 2*(value-min(value,na.rm = TRUE))/
           (max(value,na.rm = TRUE) - min(value,na.rm = TRUE))) %>%
  mutate(variable = case_when(grepl("MAP",var) ~ "MAP",
                              grepl("tmp",var) ~ "tmp",
                              TRUE ~ "other"),
         timing = case_when(grepl("norm",var) ~ "norm",
                            grepl("present",var) ~ "present",
                            TRUE ~ "other")) %>%
  mutate(biome_change_norm = case_when(biome_norm == biome_init ~ FALSE,
                                       TRUE ~ TRUE),
         biome_change_present = case_when(biome_present == biome_init ~ FALSE,
                                          TRUE ~ TRUE))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_tile(data = df.all.av.wide.long %>% filter(timing == "present",
                                                  variable == "tmp"),
  aes(x = lon, y = lat,
  fill = value),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  labs(x = "",y = "", fill = "Temp. change (°C)") +
  # facet_wrap( ~ timing, scales = "free") +
  scale_fill_gradient2(low = muted("red"),
                       mid = "white",
                       high = muted("green")) +
  theme_bw()

ggplot(data = world) +
  geom_tile(data = df.all.av.wide.long %>% filter(timing == "present",
                                                  variable == "MAP"),
            aes(x = lon, y = lat,
                fill = value*10),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  labs(x = "",y = "", fill = "Precip. change (mm)") +
  # facet_wrap( ~ timing, scales = "free") +
  scale_fill_gradient2(low = muted("red"),
                       mid = "white",
                       high = muted("green"),
                       limits = c(-250,250),
                       oob = scales::squish) +
  theme_bw()

df2plot2D <- df.all.av.wide.long %>%
  ungroup() %>%
  dplyr::select(-c(var,value.rel)) %>%
  pivot_wider(names_from = variable,
              values_from = value)
# ggplot(df2plot2D %>% dplyr::filter(!is.na(biome_norm)),
#        aes(x = MAP, y = tmp) ) +
#   geom_bin2d(bins = 70, show.legend = FALSE) +
#   scale_fill_continuous(type = "viridis") +
#   facet_grid(timing ~ biome_norm) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   geom_vline(xintercept = 0, linetype = 2) +
#   theme_bw()

df2ref <- df2plot2D %>%
  dplyr::filter(!is.na(biome_norm)) %>%
  group_by(biome_norm) %>%
  summarise(MAP_ref = mean(MAP_init),
            tmp_ref = mean(abs(tmp_init)),
            .groups = "keep")

df2plot2D.and.ref <- df2plot2D %>%
  left_join(df2ref,
            by = "biome_norm") %>%
  mutate(MAP.change.rel = MAP/MAP_ref,
         tmp.change.rel = tmp/tmp_ref)

ggplot(df2plot2D.and.ref %>% dplyr::filter(!is.na(biome_norm),
                                           timing == "present"),
       aes(x = 100*MAP.change.rel, y = tmp) ) +
  geom_bin2d(bins = 70, show.legend = TRUE) +
  labs(x = "Change in precip relative to pi period (%)",
         y = "Change in temp relative to pi period (°C)",
       fill = "Gridcell #") +
  scale_fill_continuous(type = "viridis") +
  facet_grid( ~ biome_norm) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  theme(legend.position = c(0.03,0.7))


df.select <- df2plot2D.and.ref %>%
  dplyr::filter(!is.na(biome_norm),
                timing == "present") %>%
  group_by(biome_norm) %>%
  mutate(Ntot = length(MAP_init)) %>%
  filter(abs(MAP.change.rel) < 10/100,
         abs(tmp) > 1)    # changes in temperature, no (low) changes in precip


df.select2 <- df2plot2D.and.ref %>%
  dplyr::filter(!is.na(biome_norm),
                timing == "present") %>%
  group_by(biome_norm) %>%
  mutate(Ntot = length(MAP_init)) %>%
  filter(abs(MAP.change.rel) > 10/100,
         abs(tmp) < 0.25)   # low changes in temperature, changes in precip

df.select3 <- df2plot2D.and.ref %>%
  dplyr::filter(!is.na(biome_norm),
                timing == "present") %>%
  group_by(biome_norm) %>%
  mutate(Ntot = length(MAP_init)) %>%
  filter(abs(MAP.change.rel) > 10/100,
         abs(tmp) > 1)   # changes in temperature and in precip

df.select.all <- bind_rows(list(df.select %>% mutate(type.of.change = "temp"),
                                df.select2 %>% mutate(type.of.change = "precip"),
                                df.select3 %>% mutate(type.of.change = "both")))

ggplot(data = world) +
  geom_tile(data = df.select.all,
            aes(x = lon, y = lat,
                fill = type.of.change),na.rm = TRUE, alpha = 1) +
  # geom_point(data = df.select.all,
  #           aes(x = lon, y = lat, shape = as.factor(type.of.change)), size = 0.1) +
  geom_sf(fill = NA) +
  # scale_shape_manual(values = c("",".","x")) +
  labs(x = "",y = "", fill = "Type of CC") +
  theme_bw()

df.bar <- df.select.all %>%
  group_by(biome_norm,type.of.change) %>%
  summarise(N = length(MAP_init),
            Ntot = unique(Ntot),
            .groups = "keep") %>%
  mutate(r = N/Ntot)


ggplot(df.bar) +
  geom_bar(aes(x = biome_norm, fill = type.of.change,
               y = r), stat = "identity", show.legend = TRUE) +
  labs(x = "", fill = "Type of CC") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

df.bar.total <- df.bar %>%
  group_by(biome_norm) %>%
  summarise(Ntot = sum(N),
            rtot = sum(r),
            .groups = "keep") %>%
  mutate(lab = paste0(sprintf("%.1f",100*rtot),"%"))

ggplot(df.bar) +
  geom_bar(aes(x = biome_norm, fill = type.of.change,
               y = N), stat = "identity", show.legend = TRUE) +
  labs(x = "", fill = "Type of CC") +
  theme_bw() +
  geom_text(data = df.bar.total,
            aes(x = biome_norm, y = Ntot+250, label = lab)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


saveRDS(df.select.all %>%
          dplyr::select(lat,lon,biome_norm,MAP_ref,tmp_ref,MAP,tmp,type.of.change) %>%
          mutate(keep = TRUE),
        "./outputs/Map.change.climate.RDS")

ggplot(data = df.select.all %>%
         filter(type.of.change %in% c("temp","both"))) +
  geom_violin(aes(x = biome_norm,
                   fill = biome_norm,
                   y = (tmp_present - tmp_init))) +
  theme_bw() +
  guides(fill = "none") +
  labs(x = "",y = "Temp change (°C)") +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggplot(data = df.select.all %>%
         filter(type.of.change %in% c("precip","both"))) +
  geom_violin(aes(x = biome_norm,
                   fill = biome_norm,
                   y = 10*(MAP_present - MAP_init))) +
  theme_bw() +
  guides(fill = "none") +
  labs(x = "",y = "Precip change (mm)") +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
