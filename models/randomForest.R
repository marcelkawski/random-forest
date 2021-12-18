library(randomForest)
library(caret)
library(caTools)


getRFModel = function(formula, data) {
  return (randomForest(formula, data = data))
}

makeRFPrediction = function(model, data) {
  return (predict(model, newdata = data))
}


getRFConfMatrix = function(data) {
  attach(splitData(data))
  
  rf = getRFModel(cl ~ ., trainData)
  prediction = makeRFPrediction(rf, testData)
  
  return (confusionMatrix(prediction, testData$cl))
}
