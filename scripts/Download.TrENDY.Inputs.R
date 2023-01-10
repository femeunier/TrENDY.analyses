rm(list = ls())

library(stringr)

# ml lftp/4.9.2-GCCcore-11.2.0

# if (dir.exists(localdir)){
#   system2("fusermount",paste("-u",localdir))
#   system2("rm",paste("-rf",localdir))
# }
# sshfs trendy-v9@trendy.ex.ac.uk:/input /data/gent/vo/000/gvo00074/felicien/TrENDY/localdir

dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDY/"
opdir <- "/data/gent/vo/000/gvo00074/felicien/TrENDY/inputs/"
localdir <- file.path(dir,"localdir")


folders <- c("tmin","tmp","tmax","pre")
folders <- c("tmp")

# dir.create(localdir,
#            showWarnings = FALSE)
# system2("sshfs", paste0("trendy-v9@trendy.ex.ac.uk:/input ", localdir))  # password = gcb-2020 # sshfs trendy-v9@trendy.ex.ac.uk:/output /data/gent/vo/000/gvo00074/felicien/TrENDY/localdir

setwd((opdir))

for (ifolder in seq(1,length(folders))){

  files <- list.files(file.path(localdir,"CRUJRA2020",folders[ifolder]),
                               pattern = "",
                               recursive = TRUE)
  years <- as.numeric(unlist(lapply(str_split(files,pattern="\\."),"[[",6)))

  files2download <- files[years == 1987]



  for (ifile in seq(1,length(files2download))){
    if (!file.exists(file.path(opdir,basename(files2download[ifile])))){
      system2('lftp',paste0('sftp://trendy-v9:gcb-2020@trendy.ex.ac.uk -e "get /input/','CRUJRA2020/',folders[ifolder],'/',files2download[ifile],' ; exit"'))
    }
  }
}
