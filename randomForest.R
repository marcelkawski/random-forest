library(caret)
library(randomForest)
library(fields)
library(caTools)
library(readxl)

source('utils.R')


getRFConfMatrix = function(data, split_ratio = .8) {
  sample = sample.split(data$cl, SplitRatio = split_ratio)
  train_data = subset(data, sample == TRUE)
  test_data  = subset(data, sample == FALSE)
  
  rf = randomForest(cl ~ ., data = train_data)
  prediction = predict(rf, newdata = test_data)
  
  return (confusionMatrix(prediction, test_data$cl))
}

