source('models/randomForest.R')
source('models/rpartTree.R')
source('models/rpartForest.R')
source('utils.R')

data1 = getData(path = 'data/telescope/data', docType = 'txt', header = FALSE)
# data2 = getData(path = 'data/dryBean/data.xlsx', docType = 'xlsx')
# data3 = getData(path = 'data/spam/data.csv', docType = 'csv', header = FALSE)
# 
# rfCm1 = getRFConfMatrix(data1)
# rfCm2 = getRFConfMatrix(data2)
# rfCm3 = getRFConfMatrix(data3)

# rpartTreeCm1 = getRpartTreeConfMatrix(data1)
# rpartTreeCm2 = getRpartTreeConfMatrix(data2)
# rpartTreeCm3 = getRpartTreeConfMatrix(data3)

getRpartForestConfMatrix(data1)
  