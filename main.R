source('models/randomForest.R')
source('models/rpartTree.R')
source('utils.R')

data1 = getData(path = 'data/telescope/data', doc_type = 'txt', header = FALSE)
# data2 = getData(path = 'data/dryBean/data.xlsx', doc_type = 'xlsx')
# data3 = getData(path = 'data/spam/data.csv', doc_type = 'csv', header = FALSE)
# 
# rf_cm1 = getRFConfMatrix(data1)
# rf_cm2 = getRFConfMatrix(data2)
# rf_cm3 = getRFConfMatrix(data3)

rpart_tree_cm1 = getRpartTreeConfMatrix(data1)
# rpart_tree_cm2 = getRpartTreeConfMatrix(data2)
# rpart_tree_cm3 = getRpartTreeConfMatrix(data3)

rpart_tree_cm1
  