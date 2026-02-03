rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(lubridate)
library(ggplot2)
library(PCICt)
library(tidyr)
library(raster)
library(RColorBrewer)
library(TrENDY.analyses)

maindir <- "/data/gent/vo/000/gvo00074/felicien/TrENDYv14/"
dest.dir <- "/data/gent/vo/000/gvo00074/felicien/NPP_William/Formatted"

model.names <- get.model.names.TRENDY("v14")

model.dir <- rep("",length(model.names))
scenarios <- c("S2")
variables <- c("cRoot")

########################################################################
# For reading
variables.names <- list()
variables.names[[1]] <- c("cVeg")

all.df <- data.frame()

for (imodel in seq(1,length(model.names))){

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

#       if (file.exists(op)) next

      cdf <- read.Trendy(ncfile,
                         variables.names = variables.names[[ivariable]],
                         years2select = c(-Inf,Inf),
                         lat2select =  c(-25,25),
                         lon2select = NULL)

      # Correct for times

      cdf <- cdf %>%
        mutate(time = as.Date(paste0(year,"/",month,"/01")))

      cdf[["time"]][cdf[["time"]] < as.Date("1700/01/01")] <- as.Date("1700/01/01")

      time.diff <- diff( cdf %>%
                           ungroup() %>%
                           filter(lat == lat[1],
                                  lon == lon[1]) %>% pull(time))


      if (any(time.diff <= 0)){
        # wrong timeseries, correct

        cdf <- cdf %>%
          ungroup() %>%
          group_by(lat,lon) %>%
          mutate(diff.time = c(NA,diff(time))) %>%
          mutate(time.true = case_when((diff.time > 0 | is.na(diff.time)) ~ time,
                                       TRUE ~ NA_Date_)) %>%
          mutate(time.true = as.Date(na.approx(time.true))) %>%
          mutate(time = time.true) %>%
          mutate(year = year(time),
                 month = month(time)) %>%
          dplyr::select(-c(diff.time,time.true))

        print("Correcting timeseries")

      }

      time.diff <- diff( cdf %>%
                           ungroup() %>%
                           filter(lat == lat[1],
                                  lon == lon[1]) %>% pull(time))

      if (any(time.diff <= 0)){

        cdf <- cdf %>%
          group_by(lat,lon) %>%
          mutate(month = rep(1:12,n()/12),
                 year = sort(rep(1700:(1700 + n()/12 - 1),12))) %>%
          mutate(time = as.Date(paste0(year,"/",month,"/01")))

        print("Correcting timeseries (2)")

      }

      if (min(cdf$time) == as.Date("1701/02/01")){
        cdf <- cdf %>%
          ungroup() %>%
          mutate(month = month - 1) %>%
          mutate(year = case_when(month == 0 ~ year - 1,
                                  TRUE ~ year)) %>%
          mutate(month = case_when(month == 0 ~ 12,
                                   TRUE ~ month)) %>%
          mutate(time = as.Date(paste0(year,"/",month,"/01")))

        print("Shifting months")
      }


      print(paste(min(cdf$time),"-",max(cdf$time)))


      saveRDS(cdf,op)

    }
  }
}

# scp /Users/felicien/Documents/projects/TrENDY.analyses/scripts/read.npp.William.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

# A <- readRDS("/data/gent/vo/000/gvo00074/felicien/NPP_William/Formatted/Trendy.LPJmL.S2.gpp.pantropical.v14.RDS") %>%
#   rename(gpp = value)
# B <- readRDS("/data/gent/vo/000/gvo00074/felicien/NPP_William/Formatted/Trendy.LPJmL.S2.ra.pantropical.v14.RDS") %>%
#   rename(ra = value)
# A$ra <- B$ra
#
# Amod <- A %>%
#   mutate(value = gpp - ra) %>%
#   dplyr::select(-c(gpp,ra))
#
# saveRDS(Amod,
#         "/data/gent/vo/000/gvo00074/felicien/NPP_William/Formatted/Trendy.LPJmL.S2.npp.pantropical.v14.RDS")

