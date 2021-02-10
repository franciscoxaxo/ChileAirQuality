datamean<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datamean <- data[, lapply(.SD, mean, na.rm = TRUE), by = .(site, longitude, latitude), .SDcols = inicio:len]
  datamean <- as.data.frame(datamean)
  return(datamean)
}


datasd<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datasd <- data[, lapply(.SD, sd, na.rm = TRUE), by = .(site, longitude, latitude), .SDcols = inicio:len]
  datasd <- as.data.frame(datasd)
  return(datasd)
}

datamedian<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datamedian <- data[, lapply(.SD, median, na.rm = TRUE), by = .(site, longitude, latitude), .SDcols = inicio:len]
  datamedian <- as.data.frame(datamedian)
  return(datamedian)
}
