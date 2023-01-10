rm(list = ls())

library(dplyr)

model.names <- c("CLASSIC","CLM5.0","DLEM","IBIS","ISAM","ISBA-CTRIP",
                 "JSBACH","JULES-ES-1p0","LPJ-GUESS",
                 "LPJ","LPX-Bern","OCN","ORCHIDEE-CNP","ORCHIDEE",
                 "ORCHIDEEv3","SDGVM","VISIT","YIBs")

model.names <- c("CABLE-POP","CLASSIC","CLM5.0","DLEM","IBIS","ISAM","ISBA-CTRIP",
                 "JSBACH","JULES","LPJ-GUESS",
                 "LPJ","LPX-Bern","OCN","ORCHIDEE",
                 "SDGVM","VISIT-NIES","VISIT","YIBs")

scenarios <- paste0("S",c(0:3))
variables <- c("cVeg","cRoot","npp")

df <- data.frame()
for (imodel in seq(1,length(model.names))){
  for (iscenar in seq(1,length(scenarios))){
    for (ivar in seq(1,length(variables))){
      op.name <- file.path("/data/gent/vo/000/gvo00074/felicien/TrENDY/",
                           paste0(model.names[imodel],"_",scenarios[iscenar],"_",variables[ivar],".nc"))

      df <- bind_rows(list(df,
                           data.frame(model = model.names[imodel],
                                      scenar = scenarios[iscenar],
                                      variable = variables[ivar],
                                      exists = file.exists(op.name))))

    }
  }
}

