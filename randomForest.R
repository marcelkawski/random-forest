library(caret)
library(randomForest)
library(fields)
library(caTools)
library(readxl)


getRFConfMatrix <- function(path, doc_type, output_attr = 'cl', 
                            header = FALSE, sep = ',', dec = '.') {
  if (doc_type == 'txt')
    data = read.delim(path, header = header, sep = sep, dec = dec)
  else if (doc_type == 'xlsx')
    data = as.data.frame(read_excel(path = path, col_names = header))

  colnames(data)[ncol(data)] = 'cl'
  data$cl = factor(data$cl)

  sample = sample.split(data$cl, SplitRatio = .8)
  train_data = subset(data, sample == TRUE)
  test_data  = subset(data, sample == FALSE)

  rf = randomForest(cl ~ ., data = train_data)
  prediction = predict(rf, newdata = test_data)
  cm = confusionMatrix(prediction, test_data$cl)
  
  return(cm)
}


