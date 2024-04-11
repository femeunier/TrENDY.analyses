rm(list = ls())

library(geodata)

lats = c(0.5,10) ; lons = c(25,30)

MAP <- MAT <- c()
days <- c(31,28,31,30,31,30,31,31,30,31,30,31)

for (i in seq(1,length(lats))){

  print(i/length(lats))

  clon <- lons[i] ; clat <- lats[i]
  xy <- rbind(c(clon,clat))
  p <- vect(xy, crs="+proj=longlat +datum=WGS84")

  tavg <- worldclim_tile("tavg", clon, clat, "./data/", version="2.1")
  prec <- worldclim_tile("prec", clon, clat, "./data/", version="2.1")

  t.avg <- as.numeric(as.vector(raster::extract(tavg, xy)))
  MAT[i] <- weighted.mean(t.avg,days)

  prec.avg <- as.numeric(as.vector(raster::extract(prec, xy)))
  MAP[i] <- sum(prec.avg)

}

