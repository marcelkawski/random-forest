source('randomForest.R')
source('rpart.R')
source('utils.R')

data1 = getData(path = 'data/telescope/data', doc_type = 'txt', header = FALSE)
data2 = getData(path = 'data/dryBean/data.xlsx', doc_type = 'xlsx', header = TRUE)
data3 = getData(path = 'data/spam/data.csv', doc_type = 'csv', header = FALSE)

rf_cm1 = getRFConfMatrix(data1)
rf_cm2 = getRFConfMatrix(data2)
rf_cm3 = getRFConfMatrix(data3)

rpart_acc1 <- getRpartAccuracy(data1)
rpart_acc2 <- getRpartAccuracy(data2)
rpart_acc3 <- getRpartAccuracy(data3)

print(rpart_acc1)
