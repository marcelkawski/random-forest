library(rpart)
library(caret)

source('utils.R')

getRpartTreeModel = function(formula, data, method) {
  return (rpart(formula, data, method = method))
}

makeRpartTreePrediction = function(model, data, type) {
  return (predict(model, newdata = data, type = type))
}

getRpartTreeConfMatrix = function(data, split_ratio = .8) {
  sample = sample.split(data$cl, SplitRatio = split_ratio)
  train_data = subset(data, sample == TRUE)
  test_data  = subset(data, sample == FALSE)
  
  tree = getRpartTreeModel(cl ~ ., train_data, 'class')
  prediction = makeRpartTreePrediction(tree, test_data, 'class')

  return (confusionMatrix(prediction, test_data$cl))
}