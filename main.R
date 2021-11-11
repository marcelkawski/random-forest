source('randomForest.R')

cm1 = getRFConfMatrix(path = 'data/telescope/data', doc_type = 'txt')
cm2 = getRFConfMatrix(path = 'data/dryBean/Dry_Bean_Dataset.xlsx',
                     doc_type = 'xlsx', header = TRUE)
