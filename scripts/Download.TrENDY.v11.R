rm(list = ls())

# ml lftp/4.9.2-GCCcore-11.2.0
# sshfs GCBland2022data@trendy.ex.ac.uk:/ /data/gent/vo/000/gvo00074/felicien/TrENDYv11/localdir
# pwd = gcbland-2022

# sshfs user-39@trendydl.exeter.ac.uk:/ /data/gent/vo/000/gvo00074/felicien/TrENDYv11/localdir2
# Tei@m`o%j6yiethi

dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDYv11/"
localdir <- file.path(dir,"localdir2")

# if (dir.exists(localdir)){
#   system2("fusermount",paste("-u",localdir))
#   system2("rm",paste("-rf",localdir))
# }
# #
# dir.create(localdir,
#            showWarnings = FALSE)

# system2("sshfs", paste0("trendy-v9@trendy.ex.ac.uk:/output ", localdir))  # password = gcb-2020 # sshfs trendy-v9@trendy.ex.ac.uk:/output /data/gent/vo/000/gvo00074/felicien/TrENDY/localdir
#
# files2download <- list.files(localdir,
#                              pattern = "*S3_rh.nc*",
#                              recursive = TRUE)
# # files2download <- c(list.files(localdir,
# #                                pattern = "*nbp.nc*",
# #                                recursive = TRUE))
#
# files2download <- files2download[grepl("v11",files2download)]
#
# setwd(paste0(dir))
#
# for (ifile in seq(1,length(files2download))){
#   # if (!file.exists(file.path(dir,basename(files2download[ifile])))){
#     # system2('lftp',paste0('sftp://GCBland2022data@trendy.ex.ac.uk/ -e "get /',files2download[ifile],' ; exit"'))
#     system2('lftp',paste0('-u user-39,Tei@m\\`o%j6yiethi sftp://trendydl.exeter.ac.uk/ -e "get /',files2download[ifile],' ; exit"'))
#   # }
# }

for (scenar in c("S2","S3")){
  for (cvar in c("cVeg","cRoot")){

    files2download <- list.files(localdir,
                                 pattern = paste0("*",scenar,"_",cvar,".nc*"),
                                 recursive = TRUE)
    # files2download <- c(list.files(localdir,
    #                                pattern = "*nbp.nc*",
    #                                recursive = TRUE))

    files2download <- files2download[grepl("v11",files2download)]

    setwd(paste0(dir))

    for (ifile in seq(1,length(files2download))){
      # if (!file.exists(file.path(dir,basename(files2download[ifile])))){
      # system2('lftp',paste0('sftp://GCBland2022data@trendy.ex.ac.uk/ -e "get /',files2download[ifile],' ; exit"'))
      system2('lftp',paste0('-u user-39,Tei@m\\`o%j6yiethi sftp://trendydl.exeter.ac.uk/ -e "get /',files2download[ifile],' ; exit"'))
      # }
    }
  }
}


# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/Download.TrENDY.v11.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
