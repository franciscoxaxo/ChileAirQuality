datamean<- function(data){
  library(data.table)
  len = length(data)
  datamean<-data[, lapply(.SD, mean), by = .(site), .SDcols = 3:len]
  return(datamean)
}


