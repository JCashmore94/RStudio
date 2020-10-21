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

wine.balance <- ROSE(good~., data = wine, N = 5000, seed = 222)$data
summary(wine.balance)
colSums((is.na(wine.balance)))
is.null(wine.balance)
missmap(wine.train_labels)

#Normalise numeric variables
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wine.norm <- as.data.frame(lapply(wine[c(1:11)], normalize))

wine_train <- wine.norm[1:3500,]
wine_test <- wine.norm[3501:5000,]

wine_train_labels <- wine[1:3500, 12]
wine_test_labels <- wine[3501:5000, 12]



wine_test_pred <- knn(train = wine_train, test = wine_test, cl = wine_train_labels, k = 10)


CrossTable(x = wine_test_pred, y = wine_test_labels, chisq = FALSE)

CM <- table(wine_test_labels, y = wine_test_pred)

accuracy1 = (sum(diag(CM)))/sum(CM)
accuracy1 * 100

xtab <- table(wine_test_pred, wine_test_labels)
confusionMatrix(xtab, mode = "everything")