library(dplyr)
library(tidyverse)
library(caret)
library(titanic)

Titanic

##Logistic Regression (1/0)

##Question
#Predict How Many Survivor from Titanic accident by using "titanic_train" data set

#Check Data Set
tibble(titanic_train)

#Change survived into factor
titanic_train$Survived <- factor(titanic_train$Survived, levels = c(0,1),
                    labels = c("Death","Survived") )

#Drop NA
sum(is.na(titanic_train))
titanic_train <- na.omit(titanic_train)

##Split Data
set.seed(24)
n<-nrow(titanic_train)
id <- sample(1:n, size=n*0.7) ## 70% train 30% test
train_df <- titanic_train[id,]
test_df <-titanic_train[-id,]

#Train Model
set.seed(42)
ctrl <- trainControl(method = "cv",
                     number = 3)
logistic_model <- train(Survived ~ Sex + Age +SibSp + Fare + Pclass ,data = train_df, 
                        method = 'glm',
                        preProcess = c("center", "scale"),trControl = ctrl)

#Test Model
test_df$test_predicted <- predict(logistic_model, newdata = test_df)
p <- predict(logistic_model, newdata = test_df)

##Confusionmatrix
confusionMatrix(p, test_df$Survived,, positive = "Survived", mode = "prec_recall")