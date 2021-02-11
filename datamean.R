CV<- function(x, dec = 3){
  cv = (sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))*100
  cv = trunc(cv, digits = t)
  return(cv)
}

meant<-function(x, dec = 3){
  meant = trunc(mean(x, na.rm = TRUE), digits = t)
  return(meant)
}

mediant<-function(x, dec = 3){
  mediant = trunc(median(x, na.rm = TRUE), digits = t)
  return(mediant)
}

sdt<-function(x, dec = 3){
  sd = trunc(sd(x, na.rm = TRUE), digits = t)
  return(sd)
}

datamean<- function(data, col = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datamean <- data[, lapply(.SD, meant), by = .(site, longitude, latitude), .SDcols = col:len]
  datamean <- as.data.frame(datamean)
  return(datamean)
}


datasd<- function(data, col = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datasd <- data[, lapply(.SD, sdt), by = .(site, longitude, latitude), .SDcols = col:len]
  datasd <- as.data.frame(datasd)
  return(datasd)
}

datamedian<- function(data, col = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datamedian <- data[, lapply(.SD, mediant), by = .(site, longitude, latitude), .SDcols = col:len]
  datamedian <- as.data.frame(datamedian)
  return(datamedian)
}

datacv<- function(data, col = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datacv <- data[, lapply(.SD, CV), by = .(site, longitude, latitude), .SDcols = col:len]
  datacv <- as.data.frame(datacv)
  return(datacv)
}
