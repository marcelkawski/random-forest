getData = function(path, docType, header, sep = ',', dec = '.') {
  if (docType == 'txt')
    data = read.delim(path, header = header, sep = sep, dec = dec)
  else if (docType == 'xlsx')
    data = as.data.frame(read_excel(path = path, col_names = header))
  else if (docType == 'csv')
    data = read.table(path, header = header, sep = sep)
  
  colnames(data)[ncol(data)] = 'cl'
  data$cl = factor(data$cl)
  
  return (data)
}