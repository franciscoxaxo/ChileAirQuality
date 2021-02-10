datamean<- function(data){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datamean <- data[, lapply(.SD, mean, na.rm = TRUE), by = .(site), .SDcols = 3:len]
  datamean <- as.data.frame(datamean)
  return(datamean)
}
