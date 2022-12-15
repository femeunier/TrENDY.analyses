read.Trendy <- function(ncfile,
                        lat.names = c("latitude","lat","lat_FULL"),
                        lon.names = c("longitude","lon","lon_FULL"),
                        time.names = c("time","time_counter"),
                        variables.names = c("npp"),
                        years2select){

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

  values <- NULL ; i = 1
  while(is.null(values) & i <= length(variables.names)){
    values <- tryCatch(ncvar_get(nc,variables.names[i]),
                       error = function(e) NULL)
    i = i +1
  }

  nc_close(nc)



  years <- year(unit.time[3]) + (yday(unit.time[3]) -1)/365 +
    hour(paste(unit.time[3],unit.time[4]))/24/365  + times * udunits2::ud.convert(1,unit.time[1],"days")/365  # approximate years

  round.years <- floor(years)

  select <- which(round.years >= years2select[1],round.years <= years2select[2])
  times.selected <- times[select]

  cdf <- melt(values[,,select]) %>%
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
