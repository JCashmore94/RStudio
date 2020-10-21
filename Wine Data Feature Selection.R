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

#Check layout of wine Dataset
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


prop.table(table(wine$good))

Independent <- sample(2, nrow(wine), replace = TRUE, prob = c(0.7, 0.3))
wine.train <- wine[Independent == 1,]
wine.test <- wine[Independent == 2,]

prop.table(table(wine.train$good))
prop.table(table(wine.test$good))

str(wine.train)
str(wine.train)

#Pearsons Correlation Test Wine Dataset
set.seed(123)
cortest <- select_if(wine.balance, is.numeric)
summary(cortest)
correlationmatrix <- cor(cortest)
print(correlationmatrix)
highcor <- findCorrelation(correlationmatrix, cutoff = 0.75)
print(highcor)
corrplot.mixed(
  cor(cortest),
  upper = "shade",
  lower = "number",
  tl.pos = "lt",
  addCoef.col = "black",
  number.cex = .4
)

#Boruta Wine Dataset
set.seed(111)
boruta.Feature <- Boruta(good ~., data = wine.balance, doTrace = 2)
print(boruta.Feature)
plot(boruta.Feature, las = 2, cex.axis = 0.5,
     xlab = NULL)
attStats(boruta.Feature)


#LVQ Wine Dataset
set.seed(123)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model <- train(good~., data = wine.balance, method = "lvq", preProcess = "scale", trControl = control)
importance <- varImp(model, scale = FALSE)
print(importance)
plot(importance)
