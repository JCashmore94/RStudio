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

summary(wine)

Ind <- sample(2, nrow(wine), replace = TRUE, prob = c(0.7, 0.3))
wine.train <- wine[Ind == 1,]
wine.test <- wine[Ind == 2,]
oversample <- ROSE(good~., data = wine.train, N = 10000, seed = 222)$data
summary(oversample)

Ind1 <- sample(2, nrow(oversample), replace = TRUE, prob = c(0.7, 0.3))
wine.train1 <- oversample[Ind1 == 1,]
wine.test1 <- oversample[Ind1 == 2,]

summary(wine)
summary(oversample)
summary(wine.train1)

set.seed(1234)
RF500 <- randomForest(good~., data = wine.train1, ntree = 500, mtry = 3)
RFPredict <- predict(RF500, newdata = wine.test1)
table(predict(RF500), wine.test1$good)
TBL <- table(RFPredict, wine.test1$good)
accuracy1 <- (sum(diag(TBL)))/sum(TBL)
accuracy1 * 100
confusion <-  table(RFPredict, wine.test1[, ncol(wine.test)])
confusionMatrix(confusion, mode = "everything")
summary(wine)
str(wine)
