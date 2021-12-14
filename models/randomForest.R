library(caret)
library(randomForest)
library(fields)
library(caTools)
library(readxl)

source('utils.R')

getRFModel = function(formula, data) {
  return (randomForest(formula, data = data))
}

makeRFPrediction = function(model, data) {
  return (predict(model, newdata = data))
}


getRFConfMatrix = function(data, split_ratio = .8) {
  sample = sample.split(data$cl, SplitRatio = split_ratio)
  train_data = subset(data, sample == TRUE)
  test_data  = subset(data, sample == FALSE)
  
  rf = getRFModel(cl ~ ., train_data)
  prediction = makeRFPrediction(rf, test_data)
  
  return (confusionMatrix(prediction, test_data$cl))
}


