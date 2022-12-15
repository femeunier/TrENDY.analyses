rm(list = ls())

# install.packages(c("ncdf4","reshape2","lubridate","ggplot2","tidyr","RColorBrewer"))

library(ncdf4)
library(reshape2)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
# library(YGB)
library(raster)
library(RColorBrewer)
library(TrENDY.analyses)

maindir <- "/data/gent/vo/000/gvo00074/felicien/TrENDY/"
# maindir <- "/home/femeunier/Documents/projects/TrENDY.analyses/data/"

model.names <- c("CLASSIC","CLM5.0","DLEM","IBIS","ISAM",
                 "ISBA-CTRIP","JSBACH","JULES-ES","LPJ-GUESS",
                 "LPJ","LPX-Bern","ORCHIDEE-CNP","SDGVM","VISIT",
                 "YIBs")
model.dir <- rep("",length(model.names))
scenarios <- c("S2")
variables <- c("cVeg","cRoot")

########################################################################
# For reading
variables.names <- list()
variables.names[[1]] <- c("cVeg")
variables.names[[2]] <- c("cRoot")

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

    for (ivariable in seq(1,length(variables))){

      print(paste0("-- ",variables[ivariable]))

      ncfile <- file.path(maindir,
                         file_name <- paste(cmodel <- model.names[imodel],"_",
                                            cscenario <- scenarios[iscenario],"_",
                                            cvariable <- variables[ivariable],
                                            ".nc",
                                            sep = ""))

      if (!file.exists(ncfile)){

        warning(paste0("Couldn't find this file:",ncfile))
        next()
      }

      cdf <- read.Trendy(ncfile,
                         variables.names = cvariable,
                         years2select = c(1968,Inf))


      all.df <- bind_rows(list(all.df,
                               resample.df.all.col(bigdf = cdf,
                                                   raster2resample = biome.rst,
                                                   var.names = "value") %>%
                                 mutate(model = cmodel,
                                        scenario = cscenario,
                                        variable = cvariable)
      ))

      # ggplot() +
      #   geom_raster(data = cdf %>% filter(time == time[1]),
      #               aes(x = lon, y = lat,
      #                   fill = value),na.rm = TRUE, alpha = 1) +
      #   geom_sf(data = world,
      #           fill = NA) +
      #   scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
      #   labs(x = "",y = "") +
      #   theme_bw()
    }

  }
}

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

all.df.sum <- all.df %>%
  group_by(lon,lat,model,scenario,variable) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

all.df.wide <- all.df %>%
  pivot_wider(values_from = "value",
              names_from = "variable") %>%
  mutate(cAGB = case_when(is.na(cRoot) ~ cVeg,
                          TRUE ~ cVeg - cRoot)) %>%
  filter(!is.na(cAGB))


ggplot() +
  geom_tile(data = all.df.wide %>%
              filter(year == max(year)) %>%
              group_by(lon,lat,model,scenario) %>%
              summarise(cVeg.m = mean(cVeg,na.rm = TRUE),
                        cAGB.m = mean(cAGB,na.rm = TRUE),
                        .groups = "keep"),
            aes(x = lon, y = lat,
                fill = cVeg.m),na.rm = TRUE, alpha = 1) +
  geom_sf(data = world,
          fill = NA) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "") +
  facet_grid(~ model) +
  # scale_x_continuous(limits = c(0,25)) +
  # scale_y_continuous(limits = c(0,5)) +
  theme_bw()

saveRDS(all.df.wide,
        "./outputs/Trendy.AGB.RDS")

# scp /home/femeunier/Documents/projects/TrENDY.analyses/outputs/biome.RDS hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/read.all.biomass.Trendy.R hpc:/data/gent/vo/000/gvo00074/felicien/R/



