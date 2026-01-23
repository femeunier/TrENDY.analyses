rm(list = ls())

Dir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/CB/out"
dirs <- dir(Dir)

for (idir in seq(1150,length(dirs))){

  print(idir)

  setwd(file.path(Dir,dirs[idir],"histo"))


  a <- tryCatch(system2("find",
          paste("./history-S-*","-name","'*'","!","-name","'history-S*-01-01-*'"),
          stdout=TRUE, stderr=TRUE),
          error=function(err) NA)

  if (length(a) > 0){
    tryCatch(system2("rm",
                        paste("$(find","./history-S-*","-name","'*'","!","-name","'history-S*-01-01-*')"),
                        stdout=TRUE, stderr=TRUE),
                error=function(err) NA)}
}

# rm $(find /kyukon/scratch/gent/vo/000/gvo00074/felicien/CB/out/CB_X_8W_Y_7N/histo/history-S-* -name '*' ! -name 'history-S*-01-01-*')
