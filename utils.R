getData = function(path, doc_type, header, sep = ',', dec = '.') {
  if (doc_type == 'txt')
    data = read.delim(path, header = header, sep = sep, dec = dec)
  else if (doc_type == 'xlsx')
    data = as.data.frame(read_excel(path = path, col_names = header))
  else if (doc_type == 'csv')
    data = read.table(path, header = header, sep = sep)
  
  colnames(data)[ncol(data)] = 'cl'
  data$cl = factor(data$cl)
  
  return (data)
}