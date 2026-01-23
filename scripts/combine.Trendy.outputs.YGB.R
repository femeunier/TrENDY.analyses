rm(list = ls())

library(dplyr)
library(tidyr)
library(TrENDY.analyses)
library(ggplot2)

system2("rsync",
        paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/*_YGB_v11.RDS",
              "./outputs/"))

model.names <- get.model.names.TRENDY(version = "v11")
# model.names <- c("CLM5.0")

scenarios <- c("S2")
variables <- c("gpp","npp","rh")
op.type = "monthly"

# scenarios <- c("S2")
# variables <- c("cVeg",
#                "cRoot")
# op.type = "yearly"

df.model <- data.frame()
compt.model <- 0

for (cmodel in model.names){

  print(cmodel)
  for (cvariable in variables){

    print(paste0("- ",cvariable))

    for (cscenario in scenarios){

      print(paste0("-- ",cscenario))

      op.file <- paste0("./outputs/Trendy.",cmodel,".",cscenario,".",cvariable,"_YGB_v11.RDS")

      if (!file.exists(op.file)) {
        warning(paste0("could not find file: ",op.file))
        next()
      }

      if (op.type == "monthly"){
        # Montly GPP
        cdf <- readRDS(op.file) %>%
          dplyr::filter(!is.na(value)) %>%
          group_by(lat,lon,year)


        if (cdf %>%
            ungroup() %>%
            filter(year == max(year)) %>% pull(month) %>% max() != 12){

          print(paste0("Correcting time for model ",cmodel))

          temp <- cdf %>%
            ungroup() %>%
            filter(lat == lat[1],
                   lon == lon[1]) %>%
            mutate(time = year + (month-1/2)/12)

          diff <- round((max(temp$time) - (2021 + 11.5/12))*12)

          cdf <- temp %>%
            mutate(time = time + diff/12) %>%
            mutate(year = floor(time),
                   month = round((time - floor(time))*12 + 1/2))

        }

        cdf.sum <- cdf %>%
          group_by(lon,lat,month) %>%
          summarise(value = mean(value,
                                 na.rm = TRUE),
                    .groups = "keep")


      } else if (op.type == "yearly"){

        # Yearly AGB
        cdf <- readRDS(op.file) %>%
          dplyr::filter(!is.na(value))

        cdf.sum <- cdf %>%
          group_by(lon,lat,year) %>%
          summarise(value = mean(value,
                                 na.rm = TRUE),
                    .groups = "keep")

      }

      print(paste0("-- ",unique(cdf$time.unit)," // ",min(cdf$time)," to ",max(cdf$time)))

      df.model <- bind_rows(list(df.model,
                                 cdf.sum %>%
                                   mutate(model = cmodel,
                                          scenario = cscenario,
                                          variable = cvariable)))

      compt.model <- compt.model + 1
    }
  }
}

cdf.sum.sum <- df.model  %>%
  dplyr::select(-scenario) %>%
  pivot_wider(names_from = "variable",
              values_from = "value") %>%
  group_by(model,lon,lat) %>%
  summarise(gpp = mean(gpp,
                        na.rm = TRUE),
            npp = mean(npp,
                        na.rm = TRUE),
            rh = mean(rh,
                      na.rm = TRUE),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +

  geom_raster(data = cdf.sum.sum,
              aes(x = lon, y = lat,
                  fill = rh), na.rm = TRUE, alpha = 1) +

  geom_sf(data = world,
          fill = NA) +

  coord_sf(xlim = c(-10, 45),
           ylim = c(-15, 10)) +


  labs(x = "",y = "") +

  facet_wrap(~ model) +

  theme_bw()


df.model.sum <- df.model %>%
  group_by(model,variable,month) %>%
  summarise(value = mean(value),
            .groups = "keep") %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  mutate(ra = gpp - npp,
         nep = npp - rh) %>%
  pivot_longer(cols = -c(model,month),
               names_to = "variable",
               values_to = "value")

ggplot(data = df.model.sum) +
  geom_line(aes(x = month,
                y = value*86400*365,
                color = model)) +
  facet_wrap(~variable) +
  scale_x_continuous(breaks = seq(1,12),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  labs(x = "", y = "C flux (kgC/m²/yr)", color = "Model") +
  theme_bw() +
  theme(legend.position = c(0.8,0.22))

saveRDS(df.model,"./outputs/Trendy.YGB.S2.GPPcomp.v11.RDS")

ggplot(data = df.model.sum %>% filter(variable == "nep")) +
  geom_line(aes(x = month,
                y = (value*86400*365),
                color = model)) +
  scale_x_continuous(breaks = seq(1,12),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  labs(x = "", y = "C flux (kgC/m²/yr)", color = "Model") +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw()



ggplot(data = df.model.sum %>%
         filter(variable == "nep") %>%
         group_by(model) %>%
         mutate(nep.cum = cumsum(value*86400*365))) +
  geom_line(aes(x = month,
                y = nep.cum,
                color = model)) +
  scale_x_continuous(breaks = seq(1,12),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  labs(x = "", y = "C flux (kgC/m²/yr)", color = "Model") +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw()

df.model.sum %>%
  filter(variable == "nep") %>%
  group_by(model) %>%
  summarise(m = mean(value*86400*365)) %>%
  pull(m) %>%
  summary()

# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/combine.Trendy.outputs.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

