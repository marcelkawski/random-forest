library(rpart)


getRpartTreeModel = function(formula, data, method) {
  return (rpart(formula, data, method = method))
}

makeRpartTreePrediction = function(model, data, type) {
  return (predict(model, newdata = data, type = type))
}

getRpartTreeConfMatrix = function(data, splitRatio = .8) {
  sample = sample.split(data$cl, SplitRatio = splitRatio)
  trainData = subset(data, sample == TRUE)
  testData  = subset(data, sample == FALSE)
  
  tree = getRpartTreeModel(cl ~ ., trainData, 'class')
  prediction = makeRpartTreePrediction(tree, testData, 'class')
  
  return (confusionMatrix(prediction, testData$cl))
}