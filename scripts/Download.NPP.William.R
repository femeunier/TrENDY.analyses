rm(list = ls())

library(httr2)
library(xml2)
library(stringr)
library(TrENDY.analyses)
library(dplyr)

dest.dir <- "/data/gent/vo/000/gvo00074/felicien/NPP_William/"

# NPPPFT
files <- list_trendy_files(must_contain = c("S2", "_npppft","trendyv12"))

files2download <- files %>%
  filter(model %in%
           c("CABLE-POP","CLASSIC",
             "IBIS","ISAM","JSBACH","LPJ-GUESS",
             "LPJmL","OCN","ORCHIDEE","SDGVM",
             "VISIT","LPX-Bern"))

# NPP
files2 <- list_trendy_files(must_contain = c("S2", "_npp.nc","trendyv12"))

files2download2 <- files2 %>%
  # filter(model %in%
  #          c("CARDAMOM","CLM5.0","DLEM","EDv3","E3SM",
  #            "ISBA-CTRIP","JULES","LPJwsl","YIBs")) %>%
  filter(!grepl("_mean",destination))

# VISIT
files2download3 <- bind_rows(list_trendy_files(must_contain = c("S2", "_gpp.nc","trendyv12")),
                             list_trendy_files(must_contain = c("S2", "_ra.nc","trendyv12"))) %>%
  filter(model == "VISIT")


all.models <- unique(sort(c(unique(files2download$model),
                            unique(files2download2$model),
                            unique(files2download3$model))))

all.files2download <- bind_rows(files2download,
                                files2download2,
                                files2download3)

for (irow in seq(1,nrow(all.files2download))){

  print(paste0(irow,"/",nrow(all.files2download)))

  dest.file <- file.path(dest.dir,
                         all.files2download$destination[irow])
  if (file.exists(dest.file)) next()

  system2("wget",
          c(all.files2download$url[irow],
            "-O",
            dest.file))

}


