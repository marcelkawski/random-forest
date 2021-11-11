library(caret)
library(randomForest)
library(fields)
library(caTools)

data = read.delim('data/telescope/data', header = FALSE, sep = ',', dec = '.')
colnames(data) = c('length', 'width', 'size', 'conc', 'conc1', 'asym', 'm3long', 
                   'm3trans', 'alpha', 'dist', 'cl')
head(data)

classes = unique(data$cl)
for(i in 1:nrow(data)) {
  row = data[i,]
  if (row$cl == classes[1]) data[i,]$cl = 0
  else data[i,]$cl = 1
}
data$cl = factor(data$cl)

head(data)
tail(data)

sample = sample.split(data$cl, SplitRatio = .8)
train_data = subset(data, sample == TRUE)
test_data  = subset(data, sample == FALSE)

summary(train_data)

head(train_data)
tail(train_data)

rf = randomForest(cl ~ ., data = train_data)
prediction = predict(rf, newdata = test_data)
prediction
confusionMatrix(prediction, test_data$cl)

