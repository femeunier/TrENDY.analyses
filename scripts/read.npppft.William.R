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
library(zoo)

maindir <- "/data/gent/vo/000/gvo00074/felicien/TrENDYv14/"
dest.dir <- "/data/gent/vo/000/gvo00074/felicien/NPP_William/Formatted"

model.names <- c("CABLE-POP","CLASSIC",
                 "CLM-FATES","ELM-FATES",
                 "GDSTEM","iMAPLE",
                 "IBIS",
                 "JSBACH","JULES","LPJ-GUESS",
                 "LPJmL","ORCHIDEE",
                 "SDGVM","LPX-Bern",
                 "EDv3")

PFT.selections <- list(c(2,4),c(3,5),
                       c(2,6),c(1,5),
                       c(4),c(1),
                       c(2,4),
                       c(3,4),c(1,2),c(8:10),
                       c(1,2),c(2,3),
                       c(7,9),c(1,2),
                       c(3:5))


inversion <- rep(FALSE,length(model.names))
inversion[model.names == "LPJ-GUESS"] <- TRUE

model.dir <- rep("",length(model.names))
scenarios <- c("S2")
variables <- c("cVegpft")

########################################################################
# For reading
variables.names <- list()
variables.names[[1]] <- c("cVegpft","cVegpft_nlim","cVeg","cVeg_nlim")

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

      if (file.exists(op)) next

      cdf <- read.Trendy.pft(ncfile,
                         variables.names = variables.names[[ivariable]],
                         PFT.selection = PFT.selections[[imodel]],
                         years2select = c(-Inf,Inf),
                         lat2select =  c(-25,25),
                         lon2select = NULL,
                         invert.dimensions = inversion[imodel])
      print(summary(cdf$value))

      # Correct for times

      cdf <- cdf %>%
        mutate(time = as.Date(paste0(year,"/",month,"/01")))

      cdf[["time"]][cdf[["time"]] < as.Date("1700/01/01")] <- as.Date("1700/01/01")

      time.diff <- diff( cdf %>%
                           ungroup() %>%
                           filter(lat == lat[1],
                                  lon == lon[1],
                                  pft == pft[1]) %>%
                           pull(time))


      if (any(time.diff <= 0)){
        # wrong timeseries, correct

        cdf <- cdf %>%
          ungroup() %>%
          group_by(lat,lon,pft) %>%
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
                                  lon == lon[1],
                                  pft == pft[1]) %>% pull(time))

      if (any(time.diff <= 0)){

        cdf <- cdf %>%
          group_by(lat,lon,pft) %>%
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

      time.diff <- diff( cdf %>%
                           ungroup() %>%
                           filter(lat == lat[1],
                                  lon == lon[1],
                                  pft == pft[1]) %>% pull(time))

      if (any(time.diff <= 0)){

        cdf <- cdf %>%
          group_by(lat,lon,pft) %>%
          mutate(month = rep(1:12,n()/12),
                 year = sort(rep(1700:(1700 + n()/12 - 1),12))) %>%
          mutate(time = as.Date(paste0(year,"/",month,"/01")))

        print("Correcting timeseries (2)")

      }


      print(paste(min(cdf$time),"-",max(cdf$time)))


      saveRDS(cdf,op)

    }
  }
}

# scp /Users/felicien/Documents/projects/TrENDY.analyses/scripts/read.npppft.William.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
