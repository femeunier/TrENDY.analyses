rm(list = ls())

library(dplyr)
library(TrENDY.analyses)

model.names <- get.model.names.TRENDY(version = "v11")
dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDYv11/"

scenarios <- paste0("S",c(0:3))
variables <- c("cVeg","cRoot","npp")

df <- data.frame()
for (imodel in seq(1,length(model.names))){
  for (iscenar in seq(1,length(scenarios))){
    for (ivar in seq(1,length(variables))){
      op.name <- file.path(dir,
                           paste0(model.names[imodel],"_",scenarios[iscenar],"_",variables[ivar],".nc"))

      df <- bind_rows(list(df,
                           data.frame(model = model.names[imodel],
                                      scenar = scenarios[iscenar],
                                      variable = variables[ivar],
                                      exists = file.exists(op.name))))

    }
  }
}

