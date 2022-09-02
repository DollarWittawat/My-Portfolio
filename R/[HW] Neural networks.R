#Neural Networks

install.packages(c("dplyr", "nnet", "NeuralNetTools"))
library(dplyr)
library(nnet)
library(NeuralNetTools)
library(tidyverse)
library(titanic)

#Check Data Set
tibble(titanic_train)

#Drop NA
titanic_train <- na.omit(titanic_train)

##Split Data
set.seed(24)
n<-nrow(titanic_train)
id <- sample(1:n, size=n*0.7) ## 70% train 30% test
train_data <- titanic_train[id,]
test_data <-titanic_train[-id,]

# model training 
nn_model <- nnet(Survived ~ Pclass+Sex+Age+SibSp, data = train_data,size = 4)
plotnet(nn_model)

train_data$predict <- predict(nn_model)
train_data$predict <- ifelse(train_data$predict >= 0.5, 1,0)
train_mean <- mean(train_data$predict == train_data$Survived)


#test training

test_data$predict <- predict(nn_model, newdata = test_data)
test_data$predict <- ifelse(test_data$predict >= 0.5, 1,0)
test_mean <- mean(test_data$predict == test_data$Survived)

cat("Test_Data_Accuracy",train_mean )
cat("Train_Data_Accuracy",test_mean )

#compare to Logistic Regression, Neural Networks have higher accuracy.