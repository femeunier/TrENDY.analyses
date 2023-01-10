rm(list = ls())

# ml lftp/4.9.2-GCCcore-11.2.0
# sshfs GCBland2022data@trendy.ex.ac.uk:/ /data/gent/vo/000/gvo00074/felicien/TrENDYv11/localdir
# pwd = gcbland-2022

dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDYv11/"
localdir <- file.path(dir,"localdir")

# if (dir.exists(localdir)){
#   system2("fusermount",paste("-u",localdir))
#   system2("rm",paste("-rf",localdir))
# }
# #
# dir.create(localdir,
#            showWarnings = FALSE)

# system2("sshfs", paste0("trendy-v9@trendy.ex.ac.uk:/output ", localdir))  # password = gcb-2020 # sshfs trendy-v9@trendy.ex.ac.uk:/output /data/gent/vo/000/gvo00074/felicien/TrENDY/localdir

files2download <- list.files(localdir,
                             pattern = "*cVeg.nc*",
                             recursive = TRUE)

setwd(paste0(dir))

for (ifile in seq(1,length(files2download))){
  if (!file.exists(file.path(dir,basename(files2download[ifile])))){
    # system2('lftp',paste0('sftp://GCBland2022data@trendy.ex.ac.uk/ -e "get /',files2download[ifile],' ; exit"'))
    system2('lftp',paste0('-u GCBland2022data,gcbland-2022 sftp://trendy.ex.ac.uk/ -e "get /',files2download[ifile],' ; exit"'))
  }
}
