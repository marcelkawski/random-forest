library(plyr)
library(tidyverse)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)


dat <- read_csv("data/spam/data.csv")
glimpse(dat)

colnames(dat)[ncol(dat)] = 'cl'

set.seed(100)

trainRowNumbers <- createDataPartition(dat$cl, p=0.7, list=FALSE)
train <- dat[trainRowNumbers,]
test <- dat[-trainRowNumbers,]
dim(train); dim(test) 

tree_model = rpart(
  cl~ ., 
  data = train, 
  method="class", 
  minsplit = 10,
  minbucket=3
) 

summary(tree_model)
prp(tree_model)


PredictCART_train = predict(tree_model, data = train, type = "class")
table(train$cl, PredictCART_train)

PredictCART = predict(tree_model, newdata = test, type = "class")
table(test$cl, PredictCART)
