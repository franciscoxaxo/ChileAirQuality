CV<- function(x, t = 3){
  cv = (sd(x)/mean(x))*100
  cv = trunc(cv, digits = t)
  return(cv)
}

meant<-function(x, t = 3){
  meant = trunc(mean(x), digits = t)
  return(meant)
}

mediant<-function(x, t = 3){
  mediant = trunc(median(x), digits = t)
  return(mediant)
}

sdt<-function(x, t = 3){
  sd = trunc(sd(x), digits = t)
  return(sd)
}

datamean<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datamean <- data[, lapply(.SD, meant, na.rm = TRUE), by = .(site, longitude, latitude), .SDcols = inicio:len]
  datamean <- as.data.frame(datamean)
  return(datamean)
}


datasd<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datasd <- data[, lapply(.SD, sdt, na.rm = TRUE), by = .(site, longitude, latitude), .SDcols = inicio:len]
  datasd <- as.data.frame(datasd)
  return(datasd)
}

datamedian<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datamedian <- data[, lapply(.SD, mediant, na.rm = TRUE), by = .(site, longitude, latitude), .SDcols = inicio:len]
  datamedian <- as.data.frame(datamedian)
  return(datamedian)
}

datacv<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datacv <- data[, lapply(.SD, CV, na.rm = TRUE), by = .(site, longitude, latitude), .SDcols = inicio:len]
  datacv <- as.data.frame(datacv)
  return(datacv)
}
