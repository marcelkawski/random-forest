library(rpart)


getRpartForestModel = function(data, method, treesNum) {
  # classCounter = list()
  # classCounter.names = classes
  # classCounter = vector('list', length(classCounter.names))
  # names(classCounter) = classCounter.names
  
  # for (name in names(classCounter)) {
  #   classCounter[[name]] = 0
  # }
  
  forest = list()
  for (i in 1:treesNum) {
    # sampling rows
    samples = data[sample(nrow(data), 0.2 * nrow(data)), ]
    
    # sampling columns
    colNums = sample(ncol(samples) - 1, 0.5 * ncol(samples))
    colNums[[length(colNums) + 1]] = ncol(samples) # adding class columns
    samples = samples[, colNums]
    
    tree = rpart(cl ~ ., samples, method = method)
    # classCounter[[prediction]] = classCounter[[prediction]] + 1
    forest[[i]] = tree
  }
  
  print(forest)
  return (forest)
}


makeRpartForestPrediction = function(forest, data) {
  
}


getRpartForestConfMatrix = function(data, splitRatio = .8, treesNum = 5) {
  sample = sample.split(data$cl, SplitRatio = splitRatio)
  trainData = subset(data, sample == TRUE)
  testData  = subset(data, sample == FALSE)
  
  forest = getRpartForestModel(trainData, 'class', treesNum)
  prediction = makeRpartForestPrediction(forest, testData, 'class')
  
  # return (confusionMatrix(prediction, testData$cl))
}
