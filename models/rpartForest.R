library(rpart)
library(caret)
library(gsubfn)

source('utils.R')


getRpartForestModel = function(data, method, treesNum) {
  forest = list()
  for (i in 1:treesNum) {
    # sampling rows
    samples = data[sample(nrow(data), nrow(data), replace = TRUE), ]
    
    tree = rpart(cl ~ ., samples, method = method)
    forest[[i]] = tree
  }
  
  return (forest)
}

makeRpartForestPrediction = function(forest, data, type) {
  predsCols = list()
  for (i in 1:length(forest)) {
    predsCols[[i]] = paste('pred', i, sep = '')
  }
  
  for (i in 1:length(forest)) {
    treePred = predict(forest[[i]], newdata = data, type = type)
    data[, predsCols[[i]]] = unlist(treePred)
  }
  
  data$prediction = vector(mode = "integer", length = nrow(data))
  for (i in 1:nrow(data)) {
    preds = unlist(data[i, unlist(predsCols)])
    data$prediction[i] = getMostFrequent(preds)
  }
  
  return (data$prediction)
}


getRpartForestConfMatrix = function(data, treesNum = 2) {
  attach(splitData(data))
  
  forest = getRpartForestModel(trainData, 'class', treesNum)
  prediction = makeRpartForestPrediction(forest, testData, 'class')
  
  return (confusionMatrix(as.factor(prediction), 
                          as.factor(as.character(testData$cl))))
}
