library(dplyr)
library(tidyverse)
estate <- read_csv("Real estate.csv",show_col_types = FALSE)
colnames(estate) <- c('No','transaction_date',
                                'house_age',
                                "nearest_MRT_station",
                                "near_convenience_stores",
                                "latitude",
                                "longitude",
                                "house_price_of_unit_area")

##Predict "house price of unit area"
#Split Data
set.seed(10)
n <- nrow(estate)
id<-sample(1:n, size = n*0.7)
train_data <- estate[id,]
test_data <- estate[-id,]


#train data
train_model <- lm(house_price_of_unit_area ~ house_age + 
                    nearest_MRT_station + 
                    near_convenience_stores, 
                  data = train_data)

train_data$predict_price <- predict(train_model)

## Root Mean Square Error (RMSE)  Check Prediction Error from linear
train_squared_error <- (train_data$house_price_of_unit_area - train_data$predict_price) ^ 2
train_rmse <- sqrt(mean(squared_error))
cat("RMSE", rmse)

#test_data
test_data$predict_price <- predict(train_model,newdata = test_data)

## Root Mean Square Error (RMSE)  Check Prediction Error from linear
test_squared_error <- (test_data$house_price_of_unit_area - test_data$predict_price) ^ 2
test_rmse <- sqrt(mean(test_squared_error))
cat("RMSE", test_rmse)



