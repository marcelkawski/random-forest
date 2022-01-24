library(rpart)
library(caret)


getRpartForestModel = function(data, method, treesNum) {
  forest = list()
  for (i in 1:treesNum) {
    # sampling rows
    samples = data[sample(nrow(data), nrow(data), replace = TRUE), ]
    
    tree <- rpart(cl ~ ., samples, method = method, minsplit=10, parms=list(split="information"))
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


getRpartForestConfMatrix = function(data, treesNum = 300) {
  attach(splitData(data))
  
  methodFuncs <- list(eval=evaluationFunction, split=splitFunction, init=initFunction)
  forest = getRpartForestModel(trainData, alist, treesNum)
  prediction = makeRpartForestPrediction(forest, testData, 'vector')
  
  prediction = convertPrediction(prediction = prediction, testData = testData)
  
  return (confusionMatrix(as.factor(prediction), 
                          as.factor(as.character(testData$cl)), mode="everything"))
}

convertPrediction = function(prediction, testData){
  
  # print(min(prediction))
  # print(max(prediction))
  
  minPrediction = min(prediction)
  maxPrediction = max(prediction)
  
  no_classes = length(levels(testData$cl))
  
  step = (as.double(maxPrediction) - as.double(minPrediction))/no_classes
  
  for (i in 1:length(prediction)){
    for (j in 1:no_classes){
      if (prediction[i] == as.double(maxPrediction)){
        prediction[i] = levels(testData$cl)[no_classes]
        break
      }
      if ((prediction[i] >= as.double(minPrediction) + (j-1)*step) & (prediction[i] < as.double(minPrediction) + (j)*step))
        prediction[i] = levels(testData$cl)[j]
      
    }
  }
  
  # print(prediction)
  
  return (prediction)
}

evaluationFunction <- function(y, wt, parms) {
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)
  list(label= wmean, deviance=rss)
}

splitFunction <- function(y, wt, x, parms, continuous) {
  # Center y
  n <- length(y)
  y <- y- sum(y*wt)/sum(wt)
  
  if (continuous) {
    # continuous x variable
    temp <- cumsum(y*wt)[-n]
    
    left.wt  <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
    list(goodness= goodness, direction=sign(lmean))
  }
  else {
    # Categorical X variable
    ux <- sort(unique(x))
    wtsum <- tapply(wt, x, sum)
    ysum  <- tapply(y*wt, x, sum)
    means <- ysum/wtsum
    
    # For anova splits, we can order the categories by their means
    #  then use the same code as for a non-categorical
    ord <- order(means)
    n <- length(ord)
    temp <- cumsum(ysum[ord])[-n]
    left.wt  <- cumsum(wtsum[ord])[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),
         direction = ux[ord])
  }
}

initFunction <- function(y, offset, parms, wt) {
  if (!is.null(offset)) y <- y-offset
  list(y=y, parms=0, numresp=1, numy=1, summary= function(yval, dev, wt, ylevel, digits ) {
         paste("  mean=", format(signif(yval, digits)),
               ", MSE=" , format(signif(dev/wt, digits)), sep='') })
}



