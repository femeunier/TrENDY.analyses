rm(list = ls())

library(dplyr)

years <- c(seq(1950,2020,10),2022)

df.all <- data.frame()
for (cyear in years){

  print(cyear)
  cfile <- paste0("./outputs/monthly.climate.pantropical.",cyear,".RDS")
  cdata <- readRDS(cfile)

  df.all <- bind_rows(df.all,
                      cdata)
}

saveRDS(df.all,
        "./outputs/monthly.climate.pantropical.CRUJRA.RDS")
