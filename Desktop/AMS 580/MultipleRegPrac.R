##Practice for multiple regression
library(MASS)
library(tidyverse)
library(caret)
library(glmnet)

data <- read.csv("math.csv",sep=";")
data <- subset(data, select = -c(G1,G2))
head(data)
#Use the random seed 123 to divide the data into 75% training and 25% testing 
set.seed(123)
training.samples <- data$G3 %>% createDataPartition(p=0.75, list=FALSE)
train.data <- data[training.samples, ]
test.data <- data[-training.samples, ]

#Find the best model using stepwise variable selection method based on BIC criterion using the training data
fit <- lm(G3~., data=train.data)
fit_step <- stepAIC(fit, k=log(nrow(train.data)),trace=1)
pred <- fit_step %>% predict(test.data)
##display the coefficients of the fitted model
## make prediction on the testing data, and report the RMSE and the coefficient of determination R^2
data.frame(RMSE=RMSE(pred,test.data$G3),Rsquare=R2(pred,test.data$G3))

#Find the best model using the best subset variable selection method(based on SSE criterion) using training data
library(leaps)
fit_bs <- regsubsets(G3~., data=train.data, nvmax=30)
result <- summary(fit_bs)
which.min(result$bic)
result$which[3,]
fit_bs <- lm(G3~Mjob+failures+romantic, data=train.data)
pred <- fit_bs %>% predict(test.data)
data.frame(RMSE=RMSE(pred,test.data$G3),Rsquare=R2(pred,test.data$G3))

