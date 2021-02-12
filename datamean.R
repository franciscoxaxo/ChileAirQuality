CV<- function(x, dec = 3){
  cv = (sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))*100
  cv = round(cv, dec)
  cv = paste(cv, "%")
  return(cv)
}

meant<-function(x, dec = 3){
  meant = round(mean(x, na.rm = TRUE), dec)
  return(meant)
}

mediant<-function(x, dec = 3){
  mediant = round(median(x, na.rm = TRUE), dec)
  return(mediant)
}

sdt<-function(x, dec = 3){
  sd = round(sd(x, na.rm = TRUE), dec)
  return(sd)
}

datamean<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datamean <- data[, lapply(.SD, meant), by = .(site, longitude, latitude), .SDcols = inicio:len]
  datamean <- as.data.frame(datamean)
  return(datamean)
}


datasd<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datasd <- data[, lapply(.SD, sdt), by = .(site, longitude, latitude), .SDcols = inicio:len]
  datasd <- as.data.frame(datasd)
  return(datasd)
}

datamedian<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datamedian <- data[, lapply(.SD, mediant), by = .(site, longitude, latitude), .SDcols = inicio:len]
  datamedian <- as.data.frame(datamedian)
  return(datamedian)
}

datacv<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datacv <- data[, lapply(.SD, CV), by = .(site, longitude, latitude), .SDcols = inicio:len]
  datacv <- as.data.frame(datacv)
  return(datacv)
}
