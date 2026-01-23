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

maindir <- "/data/gent/vo/000/gvo00074/felicien/TrENDYv14/"
dest.dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDYv14/Formatted"

model.names <- TrENDY.analyses::get.model.names.TRENDY(version = "v14")

model.dir <- rep("",length(model.names))
scenarios <- c("S2")
variables <- c("gpp")

########################################################################
# For reading
variables.names <- list()
variables.names[[1]] <- c("gpp","gpp_nlim")

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


      op <- paste0(dest.dir,"/",
                   "Trendy.",cmodel,".",cscenario,".",cvariable,".pantropical.v14.RDS")

      cdf <- read.Trendy(ncfile,
                         variables.names = variables.names[[ivariable]],
                         years2select = c(-Inf,Inf),
                         lat2select =  c(-25,25),
                         lon2select = NULL)

      print(paste(min(cdf$time),"-",max(cdf$time)))


      saveRDS(cdf,op)


    }
  }
}

# scp /Users/felicien/Documents/projects/TrENDY.analyses/scripts/read.all.CC.Trendy.v14.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
