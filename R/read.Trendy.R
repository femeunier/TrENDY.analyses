read.Trendy <- function(ncfile,
                        lat.names = c("latitude","lat","lat_FULL"),
                        lon.names = c("longitude","lon","lon_FULL"),
                        time.names = c("time","time_counter"),
                        variables.names = c("npp"),
                        years2select,
                        lat2select =  NULL,
                        lon2select = NULL){

  library(ncdf4)
  library(lubridate)
  library(reshape2)

  ncfile = "/data/gent/vo/000/gvo00074/felicien/TrENDY/IBIS_S2_npp.nc"
  lat.names = c("latitude","lat","lat_FULL")
  lon.names = c("longitude","lon","lon_FULL")
  time.names = c("time","time_counter")
  variables.names = c("npp")
  years2select  = c(1968,Inf)
  lat2select =  c(-20,15)
  lon2select = c(-15,50)

  nc <- nc_open(ncfile)

  lats <- NULL ; i = 1
  while(is.null(lats) & i <= length(lat.names)){
    lats <- tryCatch(suppressMessages(ncvar_get(nc,lat.names[i])),
                     error = function(e) NULL)
    i = i +1
  }

  lons <- NULL ; i = 1
  while(is.null(lons) & i <= length(lon.names)){
    lons <- tryCatch(suppressMessages(ncvar_get(nc,lon.names[i])),
                     error = function(e) NULL)
    i = i +1
  }

  lons[lons>180] <- lons[lons>180] -360
  times <- NULL ; i = 1
  while(is.null(times) & i <= length(time.names)){
    times <- tryCatch(suppressMessages(ncvar_get(nc,time.names[i])),
                      error = function(e) NULL)

    if (!is.null(times)) unit.time <- strsplit(ncatt_get(nc,time.names[i],"units")[["value"]], "\\s+")[[1]]

    i = i + 1
  }

  # Subset

  if (nchar(unit.time[3]) == 4) {
    unit.time[3] <- paste0(unit.time[3],"/01/01")
  } else if (nchar(unit.time[3]) == 7) {
    unit.time[3] <- paste0(unit.time[3],"-01")
  }


  years <- year(unit.time[3]) + (yday(unit.time[3]) -1)/365 +
    hour(paste(unit.time[3],unit.time[4]))/24/365  + times * udunits2::ud.convert(1,unit.time[1],"days")/365  # approximate years
  round.years <- floor(years)

  select <- which(round.years >= years2select[1],round.years <= years2select[2])
  times.selected <- years[select]

  if (!is.null(lat2select)){
    select.lat <- which(lats>=lat2select[1] & lats<=lat2select[2])
    lats <- lats[select.lat]
  } else{
    select.lat <- 1:length(lats)
  }

  if (!is.null(lon2select)){
    select.lon <- which(lons>=lon2select[1] & lons<=lon2select[2])
    lons <- lons[select.lon]
  } else{
    select.lon <- 1:length(lons)
  }

  # Read values

  values <- NULL ; i = 1
  while(is.null(values) & i <= length(variables.names)){
    values <- tryCatch(ncvar_get(nc,variables.names[i],
                                 start = c(min(select.lon),min(select.lat),min(select)),
                                 count = c(length(select.lon),length(select.lat),length(select))),
                       error = function(e) NULL)
    i = i +1
  }


  nc_close(nc)

  cdf <- melt(values) %>%
    rename(lon = Var1,
           lat = Var2,
           time = Var3) %>%
    mutate(lon = lons[lon],
           lat = lats[lat],
           time = times.selected[time]) %>%
    group_by(lat,lon) %>%
    mutate(year =  round.years[select]) %>%
    filter(!is.na(value))

  return(cdf)

}
