CV<- function(x, dec = 3){
  cv = (sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))*100
  cv = round(cv, dec)
  if(!is.na(cv)){
    cv = paste(cv, "%")
  }
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


datamean2<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datamean <- data[, lapply(.SD, meant), by = .(Nombre, Latitud, Longitud), .SDcols = inicio:len]
  datamean <- as.data.frame(datamean)
  return(datamean)
}


datasd2<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datasd <- data[, lapply(.SD, sdt), by = .(Nombre, Latitud, Longitud), .SDcols = inicio:len]
  datasd <- as.data.frame(datasd)
  return(datasd)
}

datamedian2<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datamedian <- data[, lapply(.SD, mediant), by = .(Nombre, Latitud, Longitud), .SDcols = inicio:len]
  datamedian <- as.data.frame(datamedian)
  return(datamedian)
}

datacv2<- function(data, inicio = 5){
  library(data.table)
  len = length(data)
  data<- data.table(data)
  datacv <- data[, lapply(.SD, CV), by = .(Nombre, Latitud, Longitud), .SDcols = inicio:len]
  datacv <- as.data.frame(datacv)
  return(datacv)
}

comparFunction<- function(data){
  obs <- data
  comparar <- data.frame(
    par<- c("Temperatura", "PuntoRocio", "Humedad", "PresionQFE", "PresionQFF", "dd_Valor", "ff_Valor"),
    nom <- c("Ts_Valor", "Td_Valor", "HR_Valor",  "QFE_Valor", "QFF_Valor", "dd_Valor", "ff_Valor")
  )
  a <- NULL
  for(i in 1:length(obs)){
    aux <- obs[i]
    if(aux == "Viento"){
      a <- c(a, "dd_Valor", "ff_Valor")
    }else{
      for(j in 1:nrow(comparar)){
        aux1 <- comparar[j, 1]
        aux2 <- comparar[j, 2]
        if(aux == aux1){
          a <- c(a, aux2)
          }
      }
    }
    
  }
  print(a)
  return(a)
}
