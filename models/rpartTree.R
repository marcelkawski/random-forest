library(rpart)
library(caret)


getRpartTreeModel = function(formula, data, method) {
  return (rpart(formula, data, method = method, parms = list(split = "information"), minsplit = 40))
}

makeRpartTreePrediction = function(model, data, type) {
  return (predict(model, newdata = data, type = type))
}

getRpartTreeConfMatrix = function(data, splitRatio = .8) {
  attach(splitData(data))
  
  tree = getRpartTreeModel(cl ~ ., trainData, 'class')
  prediction = makeRpartTreePrediction(tree, testData, 'class')
  
  return (confusionMatrix(prediction, testData$cl, mode="everything"))
}
