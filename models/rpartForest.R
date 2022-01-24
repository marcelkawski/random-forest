library(rpart)
library(caret)


getRpartForestModel = function(data, method, treesNum) {
  forest = list()
  for (i in 1:treesNum) {
    # sampling rows
    samples = data[sample(nrow(data), nrow(data), replace = TRUE), ]
    
    tree <- rpart(cl ~ ., samples, method = alist)
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


getRpartForestConfMatrix = function(data, treesNum = 5) {
  attach(splitData(data))
  
  alist <- list(eval=evaluationFunction, split=splitFunction, init=initFunction)
  forest = getRpartForestModel(trainData, alist, treesNum)
  prediction = makeRpartForestPrediction(forest, testData, 'vector')
  
  prediction = convertPrediction(prediction, testData)
  
  return (confusionMatrix(as.factor(prediction), 
                          as.factor(as.character(testData$cl))))
}

convertPrediction = function(prediction, testData){
  
  print(min(prediction))
  print(max(prediction))
  
  treshold = as.double(min(prediction)) + ((as.double(max(prediction)) - 
                                              as.double(min(prediction)))/2)
  
  for (i in 1:length(prediction)){
    if(prediction[i] <  treshold){
      prediction[i] = levels(testData$cl)[1]
    }
    else{
      prediction[i] = levels(testData$cl)[2]
    }
  }
  
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



