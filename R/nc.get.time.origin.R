nc.get.time.origin <- function (f, v, time.dim.name, correct.for.gregorian.julian = FALSE,
                                return.bounds = FALSE){
  if (missing(time.dim.name)) {
    if (missing(v))
      dim.axes <- ncdf4.helpers:::nc.get.dim.axes(f)
    else {
      if (!(v %in% names(f$var)))
        stop(paste("Variable '", v, "' not found in file.",
                   sep = ""))
      dim.axes <- nc.get.dim.axes(f, v)
    }
    num.T.axes <- sum(dim.axes == "T", na.rm = TRUE)
    if (num.T.axes == 0)
      return(NA)
    else if (num.T.axes > 1)
      stop("More than one time axis found; please specify a variable or provide a name for the time axis.")
    time.dim.name <- names(dim.axes[dim.axes == "T" & !is.na(dim.axes)])
  }
  if (!(time.dim.name %in% names(f$dim)))
    stop(paste("Couldn't find dimension '", time.dim.name,
               "' in file.", sep = ""))
  if (!f$dim[[time.dim.name]]$create_dimvar)
    stop(paste("Couldn't find dimension variable for dim '",
               time.dim.name, "' in file.", sep = ""))
  if (f$dim$time$len == 0) {
    return(NA)
  }
  time.units <- f$dim$time$units
  time.split <- strsplit(f$dim$time$units, " ")[[1]]

  if (length(time.split) == 0){
    time.split = strsplit("months since 2003-01", " ")[[1]]
  }

  time.res <- time.split[1]
  time.calendar.att <- ncdf4::ncatt_get(f, time.dim.name,
                                        "calendar")
  if (time.split[2] == "as") {
    return(PCICt::as.PCICt.default(as.character(f$dim$time$vals),
                                   cal = ifelse(time.calendar.att$hasatt, time.calendar.att$value,
                                                "gregorian"), format = strsplit(time.split[3],
                                                                                "\\.")[[1]][1]))
  } else {
    time.origin.string <- time.split[3]
    if (time.split[1] == "months")
      time.origin.string <- paste(time.origin.string,
                                  "-01", sep = "")
    if (length(time.split) > 3)
      time.origin.string <- paste(time.origin.string,
                                  time.split[4:length(time.split)])


    if (nchar(time.split[3]) == 4) {
      time.origin.string <- paste0(time.split[3],"/01/01")
    } else if (nchar(time.split[3]) == 7) {
      time.origin.string <- paste0(time.split[3],"-01")
    }

    if (time.split[3] == "AD"){
      time.split[3] <- "0001-01-01"
      time.split[c(4,5)] <- NA
      time.origin.string <- time.split[3]
    }


    cal <- ifelse(time.calendar.att$hasatt, time.calendar.att$value,
                  "gregorian")
    time.origin.string <- gsub("O", "0", time.origin.string)
    time.origin <- PCICt::as.PCICt.default(time.origin.string,
                                           cal = cal)

    return(time.origin)
  }
}
