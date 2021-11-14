library(rpart)
library(caret)

source('utils.R')

getRpartAccuracy = function(dat) {
  set.seed(100)
  
  trainRowNumbers <- createDataPartition(dat$cl, p=0.7, list=FALSE)
  train <- dat[trainRowNumbers,]
  test <- dat[-trainRowNumbers,]
  dim(train); dim(test) 
  
  tree_model = rpart(
    cl~ ., 
    data = train, 
    method="class", 
  ) 
  
  lmat <- matrix(c(0,4,3,0), nrow=2, ncol=2, byrow=F)
  tree_model2 <- rpart(
    cl~ ., 
    data=train,
    method='class', 
    parms=list(split='gini', loss=lmat)
  )
  
  tree_model3 <- rpart(
    cl~ ., 
    data=train,
    method='class', 
    parms=list(split='gini', prior= c(.65,.35))
  )
  
  lmat <- matrix(c(0,4,3,0), nrow=2, ncol=2, byrow=F)
  tree_model4 <- rpart(
    cl~ ., 
    data=train,
    method='class', 
    parms=list(split='information', loss=lmat)
  )
  
  tree_model5 <- rpart(
    cl~ ., 
    data=train,
    method='class', 
    parms=list(split='information', prior= c(.65,.35))
  )

  
  PredictCART = predict(tree_model, newdata = test, type = "class")
  results <- table(test$cl, PredictCART)
  accuracy <- sum(diag(results))/sum(results)
  print(accuracy)
  
  PredictCART = predict(tree_model2, newdata = test, type = "class")
  results <- table(test$cl, PredictCART)
  accuracy <- sum(diag(results))/sum(results)
  print(accuracy)
  
  PredictCART = predict(tree_model3, newdata = test, type = "class")
  results <- table(test$cl, PredictCART)
  accuracy <- sum(diag(results))/sum(results)
  print(accuracy)
  
  PredictCART = predict(tree_model4, newdata = test, type = "class")
  results <- table(test$cl, PredictCART)
  accuracy <- sum(diag(results))/sum(results)
  print(accuracy)
  
  PredictCART = predict(tree_model5, newdata = test, type = "class")
  results <- table(test$cl, PredictCART)
  accuracy <- sum(diag(results))/sum(results)
  print(accuracy)

  
  return(accuracy)
}