rm(list = ls())

library(dplyr)
library(tidyr)
library(TrENDY.analyses)
library(lubridate)
library(zoo)

# system2("rsync",
#         paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.VISIT-NIES.S2.gpp.pantropical.v11.RDS",
#               "./outputs/"))

model.names <- get.model.names.TRENDY(version = "v11")
scenarios <- c("S2")
variables <- c("gpp","npp","rh","nbp")
lat.min = -35 ; lat.max = 35 ; year.min = 1901
op.type = "monthly"
average = FALSE

df.model <- data.frame()
compt.model <- 0

for (cmodel in model.names){

  print(cmodel)

  OPfile <- paste0("./outputs/Trendy.",cmodel,".",scenarios,".CC.centralAfrica.v11.RDS")
  # if (file.exists(OPfile)) next()

  cdf <- df.model <- data.frame()
  init = TRUE

  for (cvariable in variables){

    print(paste0("- ",cvariable))

    for (cscenario in scenarios){

      print(paste0("-- ",cscenario))

      # op.file <- paste0("./outputs/Trendy.",cmodel,".",cscenario,".",cvariable,"_v11.RDS")
      # op.file <- paste0("./outputs/Trendy.",cmodel,".",cscenario,".",cvariable,".rspld_v11.RDS")
      op.file <- paste0("./outputs/Trendy.",cmodel,".",cscenario,".",cvariable,".centralAfrica.v11.RDS")

      if (!file.exists(op.file)) {
        warning(paste0("could not find file: ",op.file))
        next()
      }


      if (op.type == "monthly"){
        # Montly GPP
        cdf <- readRDS(op.file) %>%
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

        if (average){
          cdf.sum <- cdf %>%
            ungroup() %>%
            mutate(lon = as.numeric(lon),
                   lat = as.numeric(lat)) %>%
            group_by(lon,lat,year) %>%
            summarise(value = mean(value,
                                   na.rm = TRUE),
                      .groups = "keep")

        } else {
          cdf.sum <- cdf %>%
            dplyr::select(-c(time,time.unit))
        }

      } else if (op.type == "yearly"){

        # Yearly AGB
        cdf <- readRDS(op.file) %>%
          dplyr::filter(!is.na(value))

        if (average){
          cdf.sum <- cdf %>%
            ungroup() %>%
            mutate(lon = as.numeric(lon),
                   lat = as.numeric(lat)) %>%
            group_by(lon,lat,year) %>%
            summarise(value = mean(value,
                                   na.rm = TRUE),
                      .groups = "keep") %>%
            dplyr::select(lon,lat,year,value)

        } else {
          cdf.sum <- cdf %>%
            dplyr::select(lon,lat,year,month,value)
        }


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
                                  lon == lon[1]) %>% pull(time))

      print(paste0("-- ",unique(cdf$time.unit)," // ",min(cdf$time)," to ",max(cdf$time)))
      print(paste0("-- ",sort(unique(time.diff))))


      if (init){
        df.model <-  cdf %>%
          mutate(model = cmodel,
                 scenario = cscenario,
                 variable = cvariable)
        init = FALSE

      } else {
        df.model <- bind_rows(list(df.model,
                                   cdf %>%
                                     mutate(model = cmodel,
                                            scenario = cscenario,
                                            variable = cvariable)))
      }



    }
  }

  compt.model <- compt.model + 1

  if (all(c("gpp","npp","rh") %in% unique(df.model$variable))){

    df.model.wide <- df.model %>%
      # mutate(lat = round(lat, digits = 3),
      #        lon = round(lon, digits = 3)) %>%
      dplyr::select(-c(abs.time,model,scenario,time.unit)) %>%
      pivot_wider(names_from = "variable",
                  values_from = "value") %>%
      filter(lat >= lat.min, lat <= lat.max,
             year >= year.min) %>%
      mutate(ra = gpp - npp,
             nep = npp - rh)

    saveRDS(df.model.wide,
            OPfile)
  } else if (all(c("gpp","npp") %in% unique(df.model$variable))){

    df.model.wide <- df.model %>%
      # mutate(lat = round(lat, digits = 3),
      #        lon = round(lon, digits = 3)) %>%
      dplyr::select(-c(abs.time,model,scenario,time.unit)) %>%
      pivot_wider(names_from = "variable",
                  values_from = "value") %>%
      filter(lat >= lat.min, lat <= lat.max,
             year >= year.min) %>%
      mutate(ra = gpp - npp,
             rh = NA,
             nep = NA)

    saveRDS(df.model.wide,
            OPfile)
  }  else if (all(c("rh","npp") %in% unique(df.model$variable))){

    df.model.wide <- df.model %>%
      # mutate(lat = round(lat, digits = 3),
      #        lon = round(lon, digits = 3)) %>%
      dplyr::select(-c(abs.time,model,scenario,time.unit)) %>%
      pivot_wider(names_from = "variable",
                  values_from = "value") %>%
      filter(lat >= lat.min, lat <= lat.max,
             year >= year.min) %>%
      mutate(nep = npp - rh,
             ra = NA,
             gpp = NA)

    saveRDS(df.model.wide,
            OPfile)
  }  else if (all(c("gpp","rh") %in% unique(df.model$variable))){

    df.model.wide <- df.model %>%
      # mutate(lat = round(lat, digits = 3),
      #        lon = round(lon, digits = 3)) %>%
      dplyr::select(-c(abs.time,model,scenario,time.unit)) %>%
      pivot_wider(names_from = "variable",
                  values_from = "value") %>%
      filter(lat >= lat.min, lat <= lat.max,
             year >= year.min) %>%
      mutate(ra = NA,
             rh = NA,
             nep = NA)

    saveRDS(df.model.wide,
            OPfile)

  }  else if (all(c("gpp") %in% unique(df.model$variable))){

    df.model.wide <- df.model %>%
      # mutate(lat = round(lat, digits = 3),
      #        lon = round(lon, digits = 3)) %>%
      dplyr::select(-c(abs.time,model,scenario,time.unit)) %>%
      pivot_wider(names_from = "variable",
                  values_from = "value") %>%
      filter(lat >= lat.min, lat <= lat.max,
             year >= year.min) %>%
      mutate(ra = NA,
             rh = NA,
             npp = NA,
             nep = NA)

    saveRDS(df.model.wide,
            OPfile)
  } else if (all(c("npp") %in% unique(df.model$variable))){

    df.model.wide <- df.model %>%
      # mutate(lat = round(lat, digits = 3),
      #        lon = round(lon, digits = 3)) %>%
      dplyr::select(-c(abs.time,model,scenario,time.unit)) %>%
      pivot_wider(names_from = "variable",
                  values_from = "value") %>%
      filter(lat >= lat.min, lat <= lat.max,
             year >= year.min) %>%
      mutate(ra = NA,
             rh = NA,
             gpp = NA,
             nep = NA)

    saveRDS(df.model.wide,
            OPfile)
  } else if (all(c("rh") %in% unique(df.model$variable))){

    df.model.wide <- df.model %>%
      # mutate(lat = round(lat, digits = 3),
      #        lon = round(lon, digits = 3)) %>%
      dplyr::select(-c(abs.time,model,scenario,time.unit)) %>%
      pivot_wider(names_from = "variable",
                  values_from = "value") %>%
      filter(lat >= lat.min, lat <= lat.max,
             year >= year.min) %>%
      mutate(ra = NA,
             gpp = NA,
             npp = NA,
             nep = NA)

    saveRDS(df.model.wide,
            OPfile)
  } else {

    print("Missing variables")
    print(unique(df.model$variable))

  }
}

# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/combine.Trendy.outputs.CC.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

