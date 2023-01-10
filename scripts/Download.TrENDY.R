rm(list = ls())

# ml lftp/4.9.2-GCCcore-11.2.0

dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDY/"
# dir <- "/home/femeunier/Documents/projects/TrENDY.analyses/"
localdir <- file.path(dir,"localdir")

if (dir.exists(localdir)){
  system2("fusermount",paste("-u",localdir))
  system2("rm",paste("-rf",localdir))
}

dir.create(localdir,
           showWarnings = FALSE)

system2("sshfs", paste0("trendy-v9@trendy.ex.ac.uk:/output ", localdir))  # password = gcb-2020 # sshfs trendy-v9@trendy.ex.ac.uk:/output /data/gent/vo/000/gvo00074/felicien/TrENDY/localdir

# sshfs trendy-v9@trendy.ex.ac.uk:/output /data/gent/vo/000/gvo00074/felicien/TrENDY/localdir

files2download <- list.files(localdir,
                             pattern = "VISIT_S3_gpp.nc.gz",
                             recursive = TRUE)

setwd(paste0(dir))

for (ifile in seq(1,length(files2download))){
  if (!file.exists(file.path(dir,basename(files2download[ifile])))){
    system2('lftp',paste0('sftp://trendy-v9:gcb-2020@trendy.ex.ac.uk -e "get /output/',files2download[ifile],' ; exit"'))
    # print(paste('lftp',paste0('sftp://trendy-v9:gcb-2020@trendy.ex.ac.uk -e "get /output/',files2download[ifile],' ; exit"')))
  }
}

# cVeg.files <- list.files("/data/gent/vo/000/gvo00074/felicien/TrENDY/localdir",
#                          pattern = "*cVeg.nc",
#                          recursive = TRUE)
#
# setwd("/data/gent/vo/000/gvo00074/felicien/TrENDY")
#
# for (ifile in seq(1,length(cVeg.files))){
#   system2('lftp',
#           paste0('sftp://trendy-v9:gcb-2020@trendy.ex.ac.uk -e "get /output/',cVeg.files[ifile],' ; exit"'))
#
# }
#
# cRoot.files <- list.files("/data/gent/vo/000/gvo00074/felicien/TrENDY/localdir",
#                          pattern = "*cRoot*",
#                          recursive = TRUE)
#
# for (ifile in seq(1,length(cRoot.files))){
#   system2('lftp',
#           paste0('sftp://trendy-v9:gcb-2020@trendy.ex.ac.uk -e "get /output/',cRoot.files[ifile],' ; exit"'))
#
# }

# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/Download.TrENDY.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

