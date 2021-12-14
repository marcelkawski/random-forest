library(randomForest)

getRFModel = function(formula, data) {
  return (randomForest(formula, data = data))
}

makeRFPrediction = function(model, data) {
  return (predict(model, newdata = data))
}


getRFConfMatrix = function(data, splitRatio = .8) {
  sample = sample.split(data$cl, SplitRatio = splitRatio)
  trainData = subset(data, sample == TRUE)
  testData  = subset(data, sample == FALSE)
  
  rf = getRFModel(cl ~ ., trainData)
  prediction = makeRFPrediction(rf, testData)
  
  return (confusionMatrix(prediction, testData$cl))
}


