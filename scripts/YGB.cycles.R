rm(list = ls())

library(dplyr)
library(ggplot2)
# library(Congo.ED2)
library(tidyr)

years <- 2023:2023

################################################################################
# CCycle

system2("rsync",c("-avz",
                  "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/df.YGB.all.Trendy.RDS",
                  "./outputs/"))

df.model.wide <- readRDS("./outputs/df.YGB.all.Trendy.RDS") %>%
  dplyr::select(-any_of(c("time","model.lat.lon"))) %>%
  filter(year %in% years) %>%
  mutate(npp = case_when(is.na(npp) & !is.na(gpp) & !is.na(ra) ~ gpp - ra,
                         TRUE ~ npp),
         nep = case_when(is.na(nep) & !is.na(nep) ~ -nee,
                         is.na(nep) & !is.na(gpp) & !is.na(ra) & !is.na(rh) ~ gpp - ra - rh,
                         is.na(nep) & !is.na(npp) & !is.na(rh) ~ npp - rh,
                         TRUE ~ nep)) %>%
  dplyr::select(-nee)

df.model <- df.model.wide %>%
  pivot_longer(cols = -c("lon","lat","year","month","model"),
               names_to = "variable",
               values_to = "value") %>%
  filter(!is.na(value))

df.model %>%
  group_by(variable) %>%
  summarise(N = length(unique(model)),
            .groups = "keep")

df.ts <- df.model %>%
  group_by(model,variable,year,month) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

df.ts.m <- df.ts %>%
  group_by(variable,year,month) %>%
  summarise(value.se = sd(value.m,na.rm = TRUE)/sqrt(length(value.m)),
            value.m = mean(value.m,na.rm = TRUE),

            .groups = "keep")

ggplot() +
  geom_line(data = df.ts,
            aes(x = year + (month - 1/2)/12, y = value.m*86400*365, group = model),
            color = "darkgrey",linetype = 2) +

  geom_ribbon(data = df.ts.m,
            aes(x = year + (month - 1/2)/12,
                y = value.m*86400*365,
                ymin = (value.m - 1.96*value.se)*86400*365,
                ymax = (value.m + 1.96*value.se)*86400*365),
            fill = "darkgrey", alpha = 0.5) +

  geom_line(data = df.ts.m,
            aes(x = year + (month - 1/2)/12,
                y = value.m*86400*365),
            color = "black") +

  facet_wrap(~variable, nrow = 1) +
  labs(x = "", y = "Carbon flux (kgC/m²/year)") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +

  theme_bw()


df.model.sum <- df.model %>%
  group_by(model,variable,month) %>%
  summarise(value = mean(value),
            .groups = "keep")

df.CC <- df.model.sum %>%
  pivot_wider(names_from = "variable",
              values_from = "value")

df.model.sum2 <- df.CC %>%
  pivot_longer(cols = -c(model,month),
               values_to = "value",
               names_to = "variable") %>%
  mutate(variable = factor(variable,
                           levels = c("gpp","ra",
                                      "npp","rh",
                                      "nep","nee","nbp")))

df.model.sum.ensemble <- df.model.sum2 %>%
  group_by(variable,month) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            value.sd = sd(value,na.rm = TRUE),
            N = sqrt(length(value)),
            value.se = value.sd/sqrt(length(value)),
            .groups = "keep")

ggplot() +
  geom_line(data = df.model.sum2,
            aes(x = month, y = value*86400*365, group = model),
            color = "darkgrey",linetype = 2) +

  geom_ribbon(data = df.model.sum.ensemble,
            aes(x = month,
                y = value.m*86400*365,
                ymin = (value.m - 1.96*value.se)*86400*365,
                ymax = (value.m + 1.96*value.se)*86400*365),
            fill = "darkgrey", alpha = 0.5) +

  geom_line(data = df.model.sum.ensemble,
            aes(x = month, y = value.m*86400*365),
            color = "black") +

  facet_wrap(~variable, nrow = 1) +
  labs(x = "", y = "Carbon flux (kgC/m²/year)") +
  scale_x_continuous(breaks = seq(1,12),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +

  theme_bw()


ggplot() +
  geom_line(data = df.model.sum2 %>%
              filter(variable == "nep"),
            aes(x = month, y = value*86400*365, color = model)) +

  geom_ribbon(data = df.model.sum.ensemble %>%
                filter(variable == "nep"),
              aes(x = month,
                  y = value.m*86400*365,
                  ymin = (value.m - 1.96*value.se)*86400*365,
                  ymax = (value.m + 1.96*value.se)*86400*365),
              fill = "darkgrey", alpha = 0.5) +

  geom_line(data = df.model.sum.ensemble %>%
              filter(variable == "nep"),
            aes(x = month, y = value.m*86400*365),
            color = "black") +
  labs(x = "", y = "Carbon flux (kgC/m²/year)") +
  scale_x_continuous(breaks = seq(1,12),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +

  theme_bw()



ggplot() +

  geom_ribbon(data = df.model.sum.ensemble,
              aes(x = month,
                  y = value.m*86400*365,
                  ymin = (value.m - 1.96*value.se)*86400*365, ymax = (value.m + 1.96*value.se)*86400*365,
              fill = variable), color = NA,
              alpha = 0.5) +

  geom_line(data = df.model.sum.ensemble,
            aes(x = month, y = value.m*86400*365,color = variable)) +


  labs(x = "", y = "Carbon flux (kgC/m²/year)", fill = "",color = "") +
  scale_x_continuous(breaks = seq(1,12),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +

  theme_bw()


stop()
################################################################################

system2("rsync",c("-avz",
                  "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/df.YGB.all.Trendy.ET.RDS",
                  "./outputs/"))

df.model.water <- readRDS("./outputs/df.YGB.all.Trendy.ET.RDS")

df.model.water.sum <- df.model.water %>%
  filter(year %in% years) %>%
  pivot_longer(cols = c("evapotrans"),
               names_to = "variable") %>%
  group_by(variable,model,month) %>%
  summarise(value = mean(value),
            .groups = "keep")

df.model.water.sum.ensemble <- df.model.water.sum %>%
  group_by(variable,month) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            value.sd = sd(value,na.rm = TRUE),
            N = sqrt(length(value)),
            value.se = value.sd/sqrt(length(value)),
            .groups = "keep")

ggplot() +
  geom_line(data = df.model.water.sum,
            aes(x = month, y = value*86400, group = model),
            size = 0.5, color = "darkgrey",linetype = 2) +

  geom_ribbon(data = df.model.water.sum.ensemble,
              aes(x = month,
                  y = value.m*86400,
                  ymin = (value.m - 1.96*value.se)*86400, ymax = (value.m + 1.96*value.se)*86400),
              fill = "darkgrey", alpha = 0.5) +

  geom_line(data = df.model.water.sum.ensemble,
            aes(x = month, y = value.m*86400),
            color = "black") +
  labs(x = "", y = "Latent heat (kgW/m²/d)", fill = "",color = "") +
  scale_x_continuous(breaks = seq(1,12),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +

  theme_bw()

################################################################################


df.WUE <- df.CC %>%
  left_join(df.model.water.sum %>%
              pivot_wider(names_from = "variable",
                          values_from = "value"),
            by = c("model","month")) %>%
  mutate(WUE = 1000*gpp/evapotrans)

df.WUE.sum <- df.WUE %>%
  group_by(month) %>%
  summarise(WUE.m = mean(WUE,na.rm = TRUE),
            WUE.sd = sd(WUE,na.rm = TRUE),
            N = sqrt(length(WUE)),
            WUE.se = WUE.sd/sqrt(length(WUE)),
            .groups = "keep")



ggplot() +
  geom_line(data = df.WUE,
            aes(x = month, y = WUE, group = model),
            size = 0.5, color = "darkgrey",linetype = 2) +

  geom_ribbon(data = df.WUE.sum,
              aes(x = month,
                  y = WUE.m,
                  ymin = (WUE.m - 1.96*WUE.se), ymax = (WUE.m + 1.96*WUE.se)),
              fill = "darkgrey", alpha = 0.5) +

  geom_line(data = df.WUE.sum,
            aes(x = month, y = WUE.m),
            color = "black") +

  labs(x = "", y = "WUE (gC/kgW)", fill = "",color = "") +
  scale_x_continuous(breaks = seq(1,12),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  theme_bw()







