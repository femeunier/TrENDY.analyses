rm(list = ls())

library(dplyr)
library(tidyr)
library(TrENDY.analyses)

# system2("rsync",
#         paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/*pantropical.v11.RDS",
#               "./outputs/"))

model.names <- get.model.names.TRENDY(version = "v13")

# scenarios <- c("S3")
# variables <- c("nbp")
# op.type = "monthly"
# average = FALSE

scenarios <- c("S2")
variables <- c("gpp")
op.type = "monthly"
average = FALSE

df.model <- data.frame()
compt.model <- 0

init = TRUE

for (cmodel in model.names){

  print(cmodel)
  for (cvariable in variables){

    print(paste0("- ",cvariable))

    for (cscenario in scenarios){

      print(paste0("-- ",cscenario))

      # op.file <- paste0("./outputs/Trendy.",cmodel,".",cscenario,".",cvariable,"_v11.RDS")
      # op.file <- paste0("./outputs/Trendy.",cmodel,".",cscenario,".",cvariable,".rspld_v11.RDS")
      op.file <- paste0("./outputs/Trendy.",cmodel,".",cscenario,".",cvariable,"_pantropical_v11.RDS")

      if (!file.exists(op.file)) {
        warning(paste0("could not find file: ",op.file))
        next()
      }


      if (op.type == "monthly"){
        # Montly GPP
        cdf <- readRDS(op.file) %>%
          dplyr::filter(!is.na(value))

        # cdf.sum <- cdf %>%
        #   group_by(lon,lat,year,month) %>%
        #   summarise(value = mean(value,
        #                          na.rm = TRUE),
        #             .groups = "keep")

        if (cdf %>%
            ungroup() %>%
            filter(year == max(year)) %>% pull(month) %>% max() != 12){

          print(paste0("Correcting time for model ",cmodel))

          temp <- cdf %>%
            ungroup() %>%
            filter(lat == lat[1],
                   lon == lon[1]) %>%
            mutate(time = year + (month-1/2)/12)

          diff <- round((max(temp$time) - (2021 + 11.5/12))*12)

          cdf.sum <- temp %>%
            mutate(time = time + diff/12) %>%
            mutate(year = floor(time),
                   month = round((time - floor(time))*12 + 1/2)) %>%
            dplyr::select(-time)

        }


        if (average){
          cdf.sum <- cdf %>%
            ungroup() %>%
            mutate(lon = as.numeric(lon),
                   lat = as.numeric(lat)) %>%
            group_by(lon,lat,year) %>%
            summarise(value = mean(value,
                                   na.rm = TRUE),
                      .groups = "keep")

        } else {
          cdf.sum <- cdf %>%
            dplyr::select(-c(time,time.unit))
        }

      } else if (op.type == "yearly"){

        # Yearly AGB
        cdf <- readRDS(op.file) %>%
          dplyr::filter(!is.na(value))

        if (average){
          cdf.sum <- cdf %>%
            ungroup() %>%
            mutate(lon = as.numeric(lon),
                   lat = as.numeric(lat)) %>%
            group_by(lon,lat,year) %>%
            summarise(value = mean(value,
                                   na.rm = TRUE),
                      .groups = "keep") %>%
            dplyr::select(lon,lat,year,value)

        } else {
          cdf.sum <- cdf %>%
            dplyr::select(lon,lat,year,month,value)
        }


      }

      print(paste0("-- ",unique(cdf$time.unit)," // ",min(cdf$time)," to ",max(cdf$time)))


      if (init){
        df.model <-  cdf.sum %>%
          mutate(model = cmodel,
                 scenario = cscenario,
                 variable = cvariable)
        init = FALSE

      } else {
        df.model <- bind_rows(list(df.model,
          cdf.sum %>%
            mutate(model = cmodel,
                   scenario = cscenario,
                   variable = cvariable)))
      }



      compt.model <- compt.model + 1
    }
  }

  # df.model.wide <- df.model %>%
  #   dplyr::select(-c(abs.time,model,scenario)) %>%
  #   pivot_wider(names_from = "variable",
  #               values_from = "value") %>%
  #   filter(year >= 1900) %>%
  #   mutate(ra = gpp - npp,
  #          nep = npp - rh)


  saveRDS(df.model.wide,
          paste0("./outputs/Trendy.",cmodel,".S2.NBP.pantropical.v11.RDS"))

}

df.model %>%
  group_by(scenario,model) %>%
  summarise(Nyear = length(unique(year)),
            Ndata.per.year = sum(year == year[1])/1e6)

df.model.select <- df.model %>%
  group_by(model) %>%
  filter(lat == lat[1],lon == lon[1])

ggplot(data = df.model.select) +
  geom_point(aes(x = year, y = value, color = model)) +
  facet_grid(variable ~ scenario) +
  theme_bw()

# df.model.wide <- data.frame()
# models <- unique(df.model$model)
#
# for (cmodel in models){
#   print(cmodel)
#   cdf <- df.model  %>%
#     filter(model == cmodel) %>%
#     dplyr::select(-scenario) %>%
#     pivot_wider(names_from = "variable",
#                 values_from = "value")
#
#
#   if (!("cRoot") %in% colnames(cdf)){
#     cdf <- cdf %>%
#       mutate(cRoot = NA_real_)
#   }
#   df.model.wide <- bind_rows(df.model.wide,
#                              cdf %>%
#                                mutate(cAGB = case_when(!is.na(cRoot) ~ cVeg - cRoot,
#                                                        TRUE ~ cVeg))
#   )
# }


#
#
# types <- df.model.wide %>%
#   group_by(model) %>%
#   summarise(type = all(is.na(cRoot)))

# cdf.sum.sum <- df.model.wide
#   group_by(model,lon,lat) %>%
#   summarise(cVeg = mean(cVeg,
#                         na.rm = TRUE),
#             cRoot = mean(cVeg,
#                         na.rm = TRUE),
#             .groups = "keep") %>%
#   mutate(cAGB = case_when(!is.na(cRoot) ~ cVeg - cRoot,
#                           TRUE ~ cVeg))

# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#
# ggplot() +
#
#   geom_raster(data = cdf.sum.sum %>% filter(lon >= -80,
#                                             lon <= -35),
#               aes(x = lon, y = lat,
#                   fill = cVeg), na.rm = TRUE, alpha = 1) +
#
#   geom_sf(data = world,
#           fill = NA) +
#
#   coord_sf(xlim = c(-80, -35),
#            ylim = c(-20, 25)) +
#
#   # scale_fill_gradient(low ="white",high = "darkgreen",na.value = "transparent",
#   #                     limits = c(0,25),
#   #                     oob = scales::squish) +
#   labs(x = "",y = "") +
#
#   facet_wrap(~ model) +
#
#   theme_bw()

df.model.wide <- df.model %>%
  ungroup() %>%
  dplyr::select(model,lon,lat,year,variable,value) %>%
  pivot_wider(names_from = "variable",
              values_from = "value")


# saveRDS(df.model.wide,"./outputs/Trendy.Global.cAGB.v11.RDS")

# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/combine.Trendy.outputs.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

