rm(list = ls())

library(httr2)
library(xml2)
library(stringr)
library(dplyr)
library(TrENDY.analyses)

dest.dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDYv14/"
model.dirs <- get.model.dirs.TRENDY("v14")
model.names <- get.model.names.TRENDY("v14")

variable.name <- "npp"
scenario <- c("S2")

extensions <- c(".nc",".nc.gz",".nc.tar.gz")

for (imodel in seq(model.names)){

  print(paste0("-",model.names[imodel]))

  for (iextension in seq(1,length(extensions))){
    dest.file.name <- paste0(model.names[imodel],"_",
                             scenario,"_",variable.name,extensions[iextension])

    url <- paste0("https://s3.eu-west-1.wasabisys.com/gcb-2025-upload/land/output/",
                  model.dirs[imodel],"/",scenario,"/",dest.file.name)
    if (resp_status(resp <- request(url) |>
        req_method("HEAD") |>
        req_error(is_error = function(r) FALSE) |>
        req_perform()) == 200) {

      if (file.exists( file.path(dest.dir,dest.file.name))) next

      system2("wget",
              c(url,
                "-O",
                file.path(dest.dir,dest.file.name)))
    }
  }
}
