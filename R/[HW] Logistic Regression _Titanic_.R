library(dplyr)
library(tidyverse)
library(nycflights13)
library(titanic)

##Logistic Regression (1/0)

##Question
#Predict How Many Survivor from Titanic accident by using "titanic_train" data set

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

#Train Model
model_train <- glm(Survived ~ Pclass+Sex,data = train_data,family ="binomial")
train_data$train_predicted <- predict(model_train, type = "response")
train_data$train_predicted <- ifelse(train_data$train_predicted >= 0.5, 1,0)

#Train Model Evaluation
mean(train_data$Survived == train_data$train_predicted) #Check Mean
train_cmax = table(train_data$train_predicted, train_data$Survived, dnn = c("Predicted", "Actual"))
cat("Accuracy",train_cmax[1,1]+ train_cmax[2,2]/sum(train_cmax))
cat("Precision",train_cmax[2,2] /(train_cmax[2,1]+train_cmax[2,2]))
cat("Recall",train_cmax[2,2] /(train_cmax[1,2]+train_cmax[2,2]))
cat("F1",2*((0.7540984*0.6831683)/(0.7540984+0.6831683)))

#Test Model
test_data$test_predicted <- predict(model_train, newdata = test_data, type = "response")
test_data$test_predicted <- ifelse(test_data$test_predicted >= 0.5, 1,0)

##Test Model Evaluation
mean(test_data$Survived == test_data$test_predicted) #Check Mean
test_cmax = table(test_data$test_predicted, test_data$Survived, dnn = c("Predicted", "Actual"))
cat("Accuracy",test_cmax[1,1]+ test_cmax[2,2]/sum(test_cmax))
cat("Precision",test_cmax[2,2] /(test_cmax[2,1]+test_cmax[2,2]))
cat("Recall",test_cmax[2,2] /(test_cmax[1,2]+test_cmax[2,2]))
cat("F1",2*((0.7564103*0.6831683)/(0.7564103+0.6704545)))
select(test_data,Name,Sex,Pclass,Survived,Predicted_Survived = test_predicted)%>% view()
