nc.get.time.multiplier <- function (x)
{
  return(switch(x, days = 86400, hours = 3600, minutes = 60,
                months = 86400 * 365/12,  month = 86400 * 365/12,
                years = 365*86400, year = 365*86400, yr = 365.25*86400,
                seconds = 1))
}
