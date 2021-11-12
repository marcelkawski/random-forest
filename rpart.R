library(rpart)
library(caret)

source('utils.R')

getRpartConfMatrix = function(data, split_ratio = .8) {
  sample = sample.split(data$cl, SplitRatio = split_ratio)
  train_data = subset(data, sample == TRUE)
  test_data  = subset(data, sample == FALSE)
  
  tree = rpart(cl~., train_data, method = "class")
  prediction = predict(tree, newdata = test_data, type = 'class')
  
  print(length(prediction))
  print(length(test_data$cl))

  return (confusionMatrix(prediction, test_data$cl))
}