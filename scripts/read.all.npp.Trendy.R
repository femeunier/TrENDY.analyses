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
model.names <- model.names[seq(length(model.names),1,-1)]


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

      op <- paste0("./outputs/Trendy.",cmodel,".",cscenario,".",cvariable,".pantropical.v11.update.RDS")

      if (file.exists(op)) next()

      cdf <- read.Trendy(ncfile,
                         variables.names = variables.names[[ivariable]],
                         years2select = c(-Inf,Inf),
                         lat2select =  c(-25,25),
                         lon2select = NULL)

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
