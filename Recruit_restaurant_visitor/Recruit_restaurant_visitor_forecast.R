#new script for xgboost
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(plotly)
library(data.table)
library(xgboost)
library(gbm)

air_reserve <- read_csv('Recuit restaurant visitor forecasting/air_reserve.csv')
air_store_info <- read_csv('Recuit restaurant visitor forecasting/air_store_info.csv')
air_visit_data <- read_csv('Recuit restaurant visitor forecasting/air_visit_data.csv')

date_info <- read_csv('Recuit restaurant visitor forecasting/date_info.csv')

hpg_reserve <- read_csv('Recuit restaurant visitor forecasting/hpg_reserve.csv')
hpg_store_info <- read_csv('Recuit restaurant visitor forecasting/hpg_store_info.csv')

store_id_relation <- read_csv('Recuit restaurant visitor forecasting/store_id_relation.csv')
test <- read_csv('Recuit restaurant visitor forecasting/sample_submission.csv')
birth_data <- read_csv('Recuit restaurant visitor forecasting/UNdata_Export_20180109_141950991.csv')

air_reserve_nearest_active_station <- read_csv('Recuit restaurant visitor forecasting/rrv-weather-data/air_store_info_with_nearest_active_station.csv')


birth_data <- birth_data[1:219,c(-3,-5,-6,-7, -9)]
birth_data <- subset(birth_data, Month != "Unknown")
birth_data <- subset(birth_data, Month != "Total")



colnames(date_info)[1] <- 'visit_date'
date_info <- date_info[,-2]

birth_data <- aggregate(birth_data$Value, by = list(birth_data$Month), mean)
colnames(birth_data) <- c('month','birth_rate')


hpg_reserve <- inner_join(hpg_reserve, store_id_relation, by = 'hpg_store_id')
hpg_reserve <- hpg_reserve[,-1]

hpg_reserve <- hpg_reserve[,c(colnames(air_reserve))]
air_reserve <- rbind(air_reserve, hpg_reserve)

air_reserve$visit_date <- date(air_reserve$visit_datetime)
air_reserve_1 <- aggregate(air_reserve$reserve_visitors, by = list(air_reserve$air_store_id, air_reserve$visit_date),sum)
air_reserve_2<- aggregate(air_reserve$reserve_visitors, by = list(air_reserve$air_store_id, air_reserve$visit_date),mean)
air_reserve_3 <- aggregate(air_reserve$reserve_visitors, by = list(air_reserve$air_store_id, air_reserve$visit_date),median)
colnames(air_reserve_1) <- c('air_store_id','visit_date','total_reservations')
colnames(air_reserve_2) <- c('air_store_id','visit_date','mean_reservations')
colnames(air_reserve_3) <- c('air_store_id','visit_date','median_reservations')


#remove NA and cleaning of data 
date <- data.frame(visit_date = do.call(rbind, strsplit(test$id, split = "_"))[,3])
test$id <- sub("(.*?_.*?)_.*", "\\1", test$id)
colnames(test)[1] <- 'air_store_id'
test <- cbind(test, date)
test$visit_date <- as.Date(test$visit_date)

train <- air_visit_data


#applying all these to train and test

train$day_of_week <- day(train$visit_date)
train$month <- months(train$visit_date)
train$year <- year(train$visit_date)
train <- left_join(train, date_info, by = 'visit_date')
train <- left_join(train, birth_data , by = 'month')
stores_mean_visitors <- aggregate(train$visitors, by= list(train$air_store_id,train$day_of_week) , mean)
stores_max_visitors <- aggregate(train$visitors, by= list(train$air_store_id,train$day_of_week), max)
stores_min_visitors <- aggregate(train$visitors, by= list(train$air_store_id,train$day_of_week), min)
stores_median_visitors <- aggregate(train$visitors, by = list(train$air_store_id, train$day_of_week), median)

colnames(stores_mean_visitors) <- c('air_store_id','day_of_week','store_mean_visit')
colnames(stores_max_visitors) <- c('air_store_id','day_of_week','store_max_visit')
colnames(stores_min_visitors) <- c('air_store_id','day_of_week','store_min_visit')
colnames(stores_median_visitors) <- c('air_store_id','day_of_week','store_median_visit')
train <- left_join(train, stores_min_visitors, by = c('air_store_id','day_of_week'))
train <- left_join(train, stores_max_visitors, by = c('air_store_id','day_of_week'))
train <- left_join(train, stores_mean_visitors, by = c('air_store_id','day_of_week'))
train <- left_join(train, stores_median_visitors, by = c('air_store_id','day_of_week'))
train <- left_join(train, air_reserve_1, by = c('air_store_id','visit_date'))
train <- left_join(train, air_reserve_2, by = c('air_store_id','visit_date'))
train <- left_join(train, air_reserve_3, by = c('air_store_id','visit_date'))

train <- left_join(train, air_store_info, by = 'air_store_id')
restaurants_in_same_area <- air_store_info %>% group_by(air_area_name) %>% count()
colnames(restaurants_in_same_area)[2] <- 'Restaurants_in_same_area'
restaurants_with_same_cuisine <- air_store_info %>% group_by(air_genre_name) %>% count()
colnames(restaurants_with_same_cuisine)[2] <- 'Restaurants_with_same_cuisine'
train <- left_join(train, restaurants_in_same_area , by = 'air_area_name')
train <- left_join(train, restaurants_with_same_cuisine, by = 'air_genre_name')

train <- 
  train[,!(names(train) %in% c('air_area_name','air_genre_name'))]

train$month <- month(train$visit_date)



test$day_of_week <- day(test$visit_date)
test$month <- months(test$visit_date)
test$year <- year(test$visit_date)
test <- left_join(test, date_info, by = 'visit_date')
test <- left_join(test, birth_data, by = 'month')
unique_stores <- data.frame(air_store_id =unique(test$air_store_id))

stores <- left_join(unique_stores, air_visit_data, by = 'air_store_id')
stores$day_of_week <- day(stores$visit_date)
stores_mean_visitors <- aggregate(stores$visitors, by= list(stores$air_store_id,stores$day_of_week) , mean)
stores_max_visitors <- aggregate(stores$visitors, by= list(stores$air_store_id,stores$day_of_week), max)
stores_min_visitors <- aggregate(stores$visitors, by= list(stores$air_store_id,stores$day_of_week), min)
stores_median_visitors <- aggregate(stores$visitors, by = list(stores$air_store_id, stores$day_of_week), median)

colnames(stores_mean_visitors) <- c('air_store_id','day_of_week','store_mean_visit')
colnames(stores_max_visitors) <- c('air_store_id','day_of_week','store_max_visit')
colnames(stores_min_visitors) <- c('air_store_id','day_of_week','store_min_visit')
colnames(stores_median_visitors) <- c('air_store_id','day_of_week','store_median_visit')
test <- left_join(test, stores_min_visitors, by = c('air_store_id','day_of_week'))
test <- left_join(test, stores_max_visitors, by = c('air_store_id','day_of_week'))
test <- left_join(test, stores_mean_visitors, by = c('air_store_id','day_of_week'))
test <- left_join(test, stores_median_visitors, by = c('air_store_id','day_of_week'))
test <- left_join(test, air_reserve_1, by = c('air_store_id','visit_date'))
test <- left_join(test, air_reserve_2, by = c('air_store_id','visit_date'))
test <- left_join(test, air_reserve_3, by = c('air_store_id','visit_date'))

test <- left_join(test, air_store_info, by = 'air_store_id')
restaurants_in_same_area <- air_store_info %>% group_by(air_area_name) %>% count()
colnames(restaurants_in_same_area)[2] <- 'Restaurants_in_same_area'
restaurants_with_same_cuisine <- air_store_info %>% group_by(air_genre_name) %>% count()
colnames(restaurants_with_same_cuisine)[2] <- 'Restaurants_with_same_cuisine'
test <- left_join(test, restaurants_in_same_area , by = 'air_area_name')
test <- left_join(test, restaurants_with_same_cuisine, by = 'air_genre_name')

test <- 
  test[,!(names(test) %in% c('air_area_name','air_genre_name'))]
test$month <- month(test$visit_date)


train[which(is.na(train$total_reservations) == TRUE),]$total_reservations <- 0
train[which(is.na(train$mean_reservations) == TRUE),]$mean_reservations <- 0
train[which(is.na(train$median_reservations) == TRUE),]$median_reservations <- 0

test[which(is.na(test$total_reservations) == TRUE),]$total_reservations <- 0
test[which(is.na(test$mean_reservations) == TRUE),]$mean_reservations <- 0
test[which(is.na(test$median_reservations) == TRUE),]$median_reservations <- 0

train$long_and_lat <- train$latitude + train$longitude
test$long_and_lat <- test$latitude+ test$longitude


train1 <- 
  train[,!(names(train) %in% c('air_store_id','visit_date','visitors'))]

test1 <- 
  test[,!(names(test) %in% c('air_store_id','visit_date','visitors'))]


labeltr = log1p(train$visitors)
labelts = test$visitors

train1 <- as.matrix(train1)
test1 <- as.matrix(test1)

trainD <- xgb.DMatrix(data = train1, label = labeltr)
testD <- xgb.DMatrix(data = test1, label = labelts)

bst <- xgb.train(data = trainD,
                 max_depth=8,
                 learning_rate=0.01,
                 nrounds=500,
                 objective='reg:linear',
                 gamma=0,
                 min_child_weight=1,
                 subsample=1,
                 colsample_bytree=1,
                 scale_pos_weight=1,
                 seed=27,
                 eval_metric = 'rmse',
                 watchlist = list(train = trainD, test = testD),
                 print_every_n = 10
                 )

dt <- xgb.importance(model = bst, feature_names = colnames(trainD))
xgb.plot.importance(dt)






