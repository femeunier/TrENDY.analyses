resample.df.all.col <- function(bigdf,
                                raster2resample,
                                var.names = "value",
                                res = NULL){

  lat <- unique(as.vector(bigdf %>% pull(lat)))
  lon <- unique(as.vector(bigdf %>% pull(lon)))

  bigdf <- bigdf %>% ungroup() %>% mutate(id = 1:nrow(bigdf))

  col.names <- colnames(bigdf)
  col2loop <- col.names[which(!(col.names %in% c("id","lon","lat",var.names)))]

  cdfcol2loop <- (bigdf[,col2loop])

  other.vars <- cdfcol2loop %>% distinct()

  all.df <- data.frame()

  for (irow in seq(1,nrow(other.vars))){
    tempdf <- suppressMessages(cbind(cdfcol2loop,id = bigdf[["id"]]) %>% inner_join(other.vars[irow,]))

    cdf <- bigdf %>% dplyr::filter(id %in% (tempdf %>% pull(id)))

    for (i in seq(1,length(var.names))){

      var.name <- var.names[i]

      if (length(unique(diff(sort(unique(lat))))) > 1){


        if (is.null(res)){
          res <- mean(c(diff(sort(unique(lat))),diff(sort(unique(lon)))))
        }

        dfr <- raster(SpatialPixelsDataFrame(points = cdf[c("lon","lat")],
                                             data = cdf[var.name],
                                             tolerance = res/10))

      } else {
        dfr <- rasterFromXYZ(cdf[,c("lon","lat",var.name)])
      }

      dfr.rspld <- resample(dfr,raster2resample)
      dfr.rspld[is.na(raster2resample)] <- NA

      df.rspld <- raster::as.data.frame(dfr.rspld,
                                        xy = TRUE) %>%
        rename(lon = x,
               lat = y)

      if (i == 1){
        new.df <- df.rspld
      } else{
        new.df[[var.name]] <- as.vector(dfr.rspld)
      }
    }

    all.df <- bind_rows(list(all.df,
                             new.df %>% mutate(merging.col = 1) %>% left_join(other.vars[irow,] %>% mutate(merging.col = 1),
                                                                              by = "merging.col") %>%
                               dplyr::select(-merging.col)
                             ))

  }

  return(all.df)

}


