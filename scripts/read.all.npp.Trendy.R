rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(raster)
library(RColorBrewer)
library(TrENDY.analyses)

maindir <- "/data/gent/vo/000/gvo00074/felicien/TrENDYv11/"

model.names <- get.model.names.TRENDY(version = "v11")

model.dir <- rep("",length(model.names))
scenarios <- c("S2","S3")
variables <- c("gpp","npp","rh","nbp")

########################################################################
# For reading
variables.names <- list()
variables.names[[1]] <- c("gpp","gpp_nlim")
variables.names[[2]] <- c("npp","npp_nlim")
variables.names[[3]] <- c("rh")
variables.names[[4]] <- c("nbp")

#######################################################################
# For regridding
biome <- readRDS("./outputs/biome.RDS")
biome.rst <- rasterFromXYZ(biome[,c("lon","lat","tmp")])

e <- as(extent(-100, 180, -25, 25), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
biome.rst.crop <- crop(biome.rst, e)

# e <- as(extent(-10, 45, -15, 10), 'SpatialPolygons')
# crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
# biome.rst.crop <- crop(biome.rst, e)

# e <- as(extent(-80, -35, -25, 20), 'SpatialPolygons')
# crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
# biome.rst.crop <- crop(biome.rst, e)



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

      # cdf <- read.Trendy(ncfile,
      #                    variables.names = variables.names[[ivariable]],
      #                    years2select = c(1960,Inf),
      #                    lat2select =  c(-25,20),
      #                    lon2select = c(-80,-35))

      op <- paste0("./outputs/Trendy.",cmodel,".",cscenario,".",cvariable,".centralAfrica.v11.RDS")

      #if (file.exists(op)) next()

      cdf <- read.Trendy(ncfile,
                         variables.names = variables.names[[ivariable]],
                         years2select = c(2000,Inf),
                         lat2select =  c(-35,35),
                         lon2select = c(-30,60))

      print(paste(min(cdf$time),"-",max(cdf$time)))


      saveRDS(cdf,op)


      # cdf.rspld <- resample.df.all.col(bigdf = cdf,
      #                                  raster2resample = biome.rst.crop,
      #                                  var.names = "value",
      #                                  res = 1)
      #
      # saveRDS(cdf.rspld,
      #         paste0("./outputs/Trendy.",cmodel,".",cscenario,".",cvariable,"_pantropical_v11_rspld.RDS"))


    }
  }
}

# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/read.all.npp.Trendy.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
