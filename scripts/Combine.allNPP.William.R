rm(list = ls())

library(dplyr)
library(stringr)

dir <- "/data/gent/vo/000/gvo00074/felicien/NPP_William/Formatted"
files <- list.files(dir,full.names = TRUE)

dest <- c(24.5,0.8)

all.data <- data.frame()

for (ifile in seq(1,length(files))){
 cfile <- files[ifile]
 cbasename <- tools::file_path_sans_ext(basename(cfile))

 print(cbasename)

 csplit <- str_split(cbasename,"\\.")[[1]]

 if (length(csplit) == 6){
   cmodel <- csplit[2]
   cscenario <- csplit[3]
   cvar <- csplit[4]
 } else if (length(csplit) == 7) {
   cmodel <- csplit[2]
   cscenario <- csplit[4]
   cvar <- csplit[5]
 } else {
   stop()
 }

 cdata <- readRDS(cfile) %>%
   mutate(lon_lat = paste0(lon,"_",lat))
 ccoord <- cdata %>%
   ungroup() %>%
   dplyr::select(lon,lat,lon_lat) %>%
   distinct() %>%
   mutate(dist =sqrt( (dest[1] - lon)**2 + (dest[2] - lat)**2)) %>%
   arrange((dist)) %>%
   slice_head(n = 1)

 cdata.filt <- cdata %>%
   filter(lon_lat == (ccoord[["lon_lat"]]))

 all.data <- bind_rows(all.data,
                       cdata.filt %>%
                         dplyr::select(lon,lat,time,value,year,month,any_of("pft")) %>%
                         mutate(var = cvar,
                                model = cmodel))

}

saveRDS(all.data,
        "./outputs/YGB.NPP.William.RDS")

################################################################################


all.data <- data.frame()

for (ifile in seq(1,length(files))){
  cfile <- files[ifile]
  cbasename <- tools::file_path_sans_ext(basename(cfile))

  print(cbasename)

  csplit <- str_split(cbasename,"\\.")[[1]]
  cmodel <- csplit[2]
  cscenario <- csplit[3]
  cvar <- csplit[4]

  cdata <- readRDS(cfile) %>%
    mutate(lon_lat = paste0(lon,"_",lat)) %>%
    filter(abs(lat) <= 23.5,
           lon >= -15 & lon <= 55)

  cdata.filt <- cdata %>%
    filter(lon_lat == (ccoord[["lon_lat"]]))

  all.data <- bind_rows(all.data,
                        cdata.filt %>%
                          dplyr::select(lon,lat,time,value,year,month,any_of("pft")) %>%
                          mutate(var = cvar,
                                 model = cmodel))

}

saveRDS(all.data,
        "./outputs/CA.NPP.William.RDS")


# scp /Users/felicien/Documents/projects/TrENDY.analyses/scripts/Combine.allNPP.William.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/


