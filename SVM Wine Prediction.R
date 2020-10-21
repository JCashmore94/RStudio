#Bring File into R
setwd("C:/Users/James/Desktop/BCU/Datamining")
wine <- read.csv("winequality.csv")

#Load required library for problem area
library(tidyverse)
library(dplyr)
library(caret)
library(mlbench)
library(e1071)
library(Hmisc)
library(randomForest)
library(BBmisc)
library(ROSE)
library(Boruta)
library(corrplot)
library(Amelia)
library(ElemStatLearn)
library(klaR)
library(rpart)
library(class)
library(gmodels)

Check layout of wine Dataset
str(wine)
summary(wine)

#Check for NA, Null or Missing values.
colSums((is.na(wine)))
is.null(wine)
missmap(wine)

#Change integer values for factors
wine[sapply(wine, is.integer)] <- lapply(wine[sapply(wine, is.integer)], as.factor)

#Remove Color and Quality attributes.
wine$color <- NULL
wine$quality <- NULL

#Normalise numeric variables
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wine.norm <- as.data.frame(lapply(wine[c(1:11)], normalize))

#Check data cleaning process with results
summary(wine.norm)
#Balance dataset

wine.balance <- ROSE(good~., data = wine, N = 5000, seed = 222)$data
summary(wine.balance)
colSums((is.na(wine.balance)))
is.null(wine.balance)
missmap(wine.balance)

summary(wine)

prop.table(table(wine$good))

Ind <- sample(2, nrow(wine), replace = TRUE, prob = c(0.7, 0.3))
wine.train <- wine[Ind == 1,]
wine.test <- wine[Ind == 2,]

str(wine.train)
summary(wine.train)
str(wine.train)
str(wine.test)

svm_model <- svm(good~., data = wine.train, type = "C-classification", kernel = "radial")
summary(svm_model)
print(svm_model)

svm_predictions <- predict(svm_model, newdata = wine.test[,-ncol(wine.test)], type = "class")

svm_confusion <- table(svm_predictions, wine.test[, ncol(wine.test)])
confusionMatrix(svm_confusion, mode = "everything")
