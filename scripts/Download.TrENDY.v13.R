rm(list = ls())

# ml lftp/4.9.2-GCCcore-11.2.0
# sshfs user-75@trendydl.exeter.ac.uk:/gcb2024/LAND/OUTPUT /home/femeunier/Documents/data/TRENDYv13/localdir/
# sftp user-75@trendydl.exeter.ac.uk
# ahn1ca:a)pu5Chia


# CABLE-POP   CARDAMOM    CLASSIC     CLM5.0      DLEM        ED          ELM         IBIS        ISAM        ISBA-CTRIP  JSBACH      JULES       LPJ-GUESS   LPJml       LPJwsl      LPX         OCN
# ORCHIDEE    SDGVM       VISIT       VISIT-UT    iMAPLE

sp.folder <- "/gcb2024/LAND/OUTPUT"
dir <- "/home/femeunier/Documents/data/TRENDYv13"
localdir <- file.path(dir,"localdir")

# if (dir.exists(localdir)){
#   system2("fusermount",paste("-u",localdir))
#   system2("rm",paste("-rf",localdir))
# }
# #
# dir.create(localdir,
#            showWarnings = FALSE)

files2download <- c(list.files(file.path(localdir),
                               pattern = "*_gpp.nc*",
                               recursive = TRUE))

files2download <- file.path(sp.folder,
                            files2download[grepl("S3",files2download)])

setwd(paste0(dir))
overwrite = FALSE
for (ifile in seq(1,length(files2download))){
  # if (!file.exists(file.path(dir,basename(files2download[ifile])))){
  # system2('lftp',paste0('sftp://GCBland2022data@trendy.ex.ac.uk/ -e "get /',files2download[ifile],' ; exit"'))
 print(paste0("Downloading:", files2download[ifile]))

  if (!overwrite & file.exists(file.path(dir,basename(files2download[ifile])))){
    next()
  }
   system2('lftp',paste0('-u user-75,ahn1ca:a\\)pu5Chia sftp://trendydl.exeter.ac.uk/ -e "get ',files2download[ifile],' ; exit"'))
  # }
}

system2("rsync",c("-avz",paste0(dir,"/*gpp*"),
                  "hpc:/data/gent/vo/000/gvo00074/felicien/TrENDYv13"))

