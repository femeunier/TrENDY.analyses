rm(list = ls())

library(dplyr)
library(feather)
library(ggplot2)
library(tidyr)
require(xgboost)
library(caret)
library(lubridate)

file.in <- "/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Global.Monthly.Climate.sample.feather"
data.in <- read_feather(file.in)


file.out <- "/home/femeunier/Documents/projects/TrENDY.analyses/outputs/cVeg.S2.ORCHIDEE.Global.feather"
data.out <- read_feather(file.out)

data.out.sample <- data.out %>%
  mutate(lat_lon = paste(lat,lon, sep = "_")) %>%
  dplyr::filter(lat_lon %in% data.in[["lat_lon"]])

ggplot(data = data.out.sample) +
  geom_line(aes(x = year, y = cVeg,
                group = interaction(lat,lon))) +
  theme_bw()


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

data.out.sample.m <- data.out.sample %>%
  group_by(lat,lon) %>%
  summarise(cVeg.m = mean(cVeg),
            .groups = "keep") %>%
  mutate(rand = runif(length(cVeg.m))) %>%
  mutate(train = case_when(rand < 0.7 ~ TRUE,
                           TRUE ~ FALSE))

data.out.sample.m %>%
  group_by(train) %>%
  summarise(n())

data.out.m <- data.out %>%
  group_by(lat,lon) %>%
  summarise(cVeg.m = mean(cVeg),
            .groups = "keep")

ggplot() +
  geom_raster(data = data.out.m,
            aes(x=lon, y = lat, fill = cVeg.m)) +
  geom_point(data = data.out.sample.m,
              aes(x=lon, y = lat, color = train),shape = 1, size = 0.1) +
  geom_sf(data = world,fill = NA) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = NA) +
  labs(x = "",y = "") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_color_manual(values = c("red","black")) +
  labs(x = "",y = "",fill = "AGB (kgC/m²)")


ggplot(data.out %>% filter(lat <= 23, lat >= -23) %>%
         group_by(year) %>%
         summarise(cVeg.m = mean(cVeg),
                   .groups = "keep")) +
  geom_line(aes(x = year, y = cVeg.m)) +
  theme_bw()

in.out <- data.in %>%
  left_join(data.out,
            by = c("year","month","lon","lat")) %>%
  ungroup() %>%
  left_join(data.out.sample.m %>% dplyr::select(lat,lon,train),
            by = c("lon","lat")) %>%
  filter(!is.na(cVeg))

in.out.long <- in.out %>%
  dplyr::select(-c(lat_lon)) %>%
  pivot_longer(cols = -c("lon","lat","cVeg"),
               names_to = "var",
               values_to = "value")

ggplot(data = in.out.long,
       aes(x = value,y = cVeg)) +
  geom_bin2d(bins = 1000) +
  scale_fill_continuous(type = "viridis") +
  facet_wrap(~var,scales = "free") +

  theme_bw()


###################################################################
library(dplyr)
library(xgboost)
library(caret)
library(readxl)

df_xautry <- read_excel("/home/femeunier/Downloads//xautry_reg.xlsx")
df_xautry$date <- as.Date(df_xautry$date)
#Splitting train and test data set
train <- df_xautry[df_xautry$date < "2021-01-01",]
test <- df_xautry[-(1:nrow(train)),]

train_Dmatrix <- train %>%
  dplyr::select(xe, xau_usd_ounce) %>%
  as.matrix()

pred_Dmatrix <- test %>%
  dplyr::select(xe, xau_usd_ounce) %>%
  as.matrix()

targets <- train$xau_try_gram


xgb_trcontrol <- trainControl(
  method = "cv",
  number = 10,
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)
#Building parameters set
xgb_grid <- base::expand.grid(
  list(
    nrounds = seq(100,200),
    max_depth = c(6,15,20),
    colsample_bytree = 1,
    eta = 0.5,
    gamma = 0,
    min_child_weight = 1,
    subsample = 1)
)

model_xgb <- caret::train(
  train_Dmatrix,targets,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 10
)

model_xgb$bestTune

fitted <- model_xgb %>%
  stats::predict(train_Dmatrix) %>%
  stats::ts(start = c(2013,1),frequency = 12)
ts_xautrygram <- ts(targets,start=c(2013,1),frequency=12)
forecast_xgb <- model_xgb %>% stats::predict(pred_Dmatrix)
forecast_ts <- ts(forecast_xgb,start=c(2021,1),frequency=12)
#Preparing forecast object
forecast_xautrygram <- list(
  model = model_xgb$modelInfo,
  method = model_xgb$method,
  mean = forecast_ts,
  x = ts_xautrygram,
  fitted = fitted,
  residuals = as.numeric(ts_xautrygram) - as.numeric(fitted)
)
class(forecast_xautrygram) <- "forecast"

#The function to convert decimal time label to wanted format
date_transform <- function(x) {format(date_decimal(x), "%Y")}
#Making a time series varibale for observed data
observed_values <- ts(test$xau_try_gram,start=c(2021,1),frequency=12)


#Plot forecasting
library(ggplot2)
library(forecast)
autoplot(forecast_xautrygram)+
  autolayer(forecast_xautrygram$mean,series="Predicted",size=0.75) +
  autolayer(forecast_xautrygram$x,series ="Train",size=0.75 ) +
  autolayer(observed_values,series = "Observed",size=0.75) +
  scale_x_continuous(labels =date_transform,breaks = seq(2013,2021,2) ) +
  guides(colour=guide_legend(title = "Time Series")) +
  ylab("Price") + xlab("Time") +
  ggtitle("") +
  theme_bw()

xgb_imp <- xgb.importance(
  feature_names = colnames(train_Dmatrix),
  model = model_xgb$finalModel)
xgb.ggplot.importance(xgb_imp,
                      n_clusters = c(2))+
  ggtitle("") +
  theme_bw()+
  theme(legend.position="none")





train <- list(data = as.matrix(in.out %>% filter(train) %>% dplyr::select(-c(lat_lon,cVeg,train))),
              label = in.out %>% filter(train) %>% dplyr::pull(cVeg))
test <- list(data = as.matrix(in.out %>% filter(!train) %>% dplyr::select(-c(lat_lon,cVeg,train))),
             label = in.out %>% filter(!train) %>% dplyr::pull(cVeg))

bst <- xgboost(data = train$data,
               label = train$label,
               max.depth = 6,
               eta = 0.3, nthread = 2, nrounds = 25)


pred <- predict(bst, test$data)

plot(pred,test$label)
