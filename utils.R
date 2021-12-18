library(readxl)
library(gsubfn)
library(caTools)


getData = function(path, docType, header = FALSE, sep = ',', dec = '.') {
  if (docType == 'txt')
    data = read.delim(path, header = header, sep = sep, dec = dec)
  else if (docType == 'xlsx')
    data = as.data.frame(read_excel(path = path))
  else if (docType == 'csv')
    data = read.table(path, header = header, sep = sep)
  
  colnames(data)[ncol(data)] = 'cl'
  data$cl = factor(data$cl)
  
  return (data)
}

getMostFrequent = function(x) {
  tab = sort(table(x))
  out =  names(tail(tab, 1))
  return (out)
}

splitData = function(data, splitRatio = .8) {
  sample = sample.split(data$cl, SplitRatio = splitRatio)
  trainData = subset(data, sample == TRUE)
  testData  = subset(data, sample == FALSE)
  
  return (list(trainData = trainData, testData = testData))
}
