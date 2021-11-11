source('randomForest.R')

cm1 = getRFConfMatrix(path = 'data/telescope/data', doc_type = 'txt',
                      header = FALSE)
cm2 = getRFConfMatrix(path = 'data/dryBean/data.xlsx',
                     doc_type = 'xlsx')
cm3 = getRFConfMatrix(path = 'data/spam/data.csv', doc_type = 'csv')
