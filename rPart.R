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
    minsplit = 10,
    minbucket=3
  ) 
  
  PredictCART_train = predict(tree_model, data = train, type = "class")
  results <- table(train$cl, PredictCART_train)
  accuracy <- sum(diag(results))/sum(results)
  # print(accuracy)
  
  PredictCART = predict(tree_model, newdata = test, type = "class")
  results <- table(test$cl, PredictCART)
  accuracy <- sum(diag(results))/sum(results)
  # print(accuracy)
  
  return(accuracy)
}