get.model.names.TRENDY <- function(version = "v11"){


  if (version == "v11"){
    model.names <- c("CABLE-POP","CLASSIC","CLM5.0","DLEM","IBIS","ISAM","ISBA-CTRIP",
                     "JSBACH","JULES","LPJ-GUESS",
                     "LPJ","LPX-Bern","OCN","ORCHIDEE",
                     "SDGVM","VISIT-NIES","VISIT","YIBs")  # v11
  } else {
    model.names <- c("CLASSIC","CLM5.0","DLEM","IBIS","ISAM","ISBA-CTRIP",
                     "JSBACH","JULES-ES-1p0","LPJ-GUESS",
                     "LPJ","LPX-Bern","OCN","ORCHIDEE-CNP","ORCHIDEE",
                     "ORCHIDEEv3","SDGVM","VISIT","YIBs")  # v9
  }


  return(model.names)
}
