#install.packages("tidyr")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("rvest")
#install.packages("purrr")
#install.packages("robotstxt")
#install.packages("xml2")


#Llamada de librerias

library(robotstxt)
library(selectr)
library(xml2)
#library(dplyr)
library(stringr)
library(magrittr)
#library(tidyr)
library(lubridate)
library(tibble)
library(purrr)
library(rvest)

#Imput de datos#
Comunas <- c("SA","TA")
Contaminantes <-c("PM10","PM25","O3","CO","CH4")
fi<-"02/05/2020 1:00"
ft<-"03/05/2020 1:00"

#Fin de Imput#


#Conversion de Fechas#
#Fechas para tablas#
Fecha_inicio<- as.POSIXct(strptime(fi, format = "%d/%m/%Y %H:%M"))
Fecha_termino<- as.POSIXct(strptime(ft, format = "%d/%m/%Y %H:%M"))



#Fechas para araña#
Fecha_inicio_para_arana<-as.character(Fecha_inicio, format("%y%m%d"))
Hora_inicio_para_arana<- as.character(Fecha_inicio, format( "%H%M")) 
Fecha_termino_para_arana<- as.character(Fecha_termino, format("%y%m%d"))
Hora_termino_para_arana<-as.character(Fecha_termino, format( "%H%M")) 
horas<-(as.numeric(Fecha_termino)/3600-as.numeric(Fecha_inicio)/3600) #horas entre fecha y fecha


col_fecha= NULL; n =NULL
for(n in 0:horas) 
{

  col_fecha<-c(col_fecha, as.character(Fecha_inicio+3600*n, "%d/%m/%Y %H:%M")  )
}
col_fecha
length(col_fecha)
n = NULL
#Selector de arañas#

aux = NULL; i =NULL
for(i in 1:length(Comunas))
{
  aux<-Comunas[i]
  
  if(aux == "SA")
    {
    col_ciudad<-rep(aux,horas+1)
    p= NULL; auxiliar = NULL
    for(p in 1:length(Contaminantes))
    {
      auxiliar<- Contaminantes[p]
      if(Contaminantes[p]=="PM10")
      {
        #Insertar araña PM10-Stgo Aquí#
        PM10_col1<-c("","","","","","","","","","","","","","","","",17,18,19,20,21,22,23,24,25)
        PM10_col2<-c("","","","","","","","",9,10,11,12,13,14,15,16,"","","","","","","","","")
        PM10_col3<-c(1,2,3,4,5,6,7,8,"","","","","","","","","","","","","","","","","")
        PM10<-gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3))
        print("PM10 Stgo")
      } else
      {
        if(Contaminantes[p]=="PM25")
        {
          #Insertar araña PM25-Stgo Aquí#
          PM25_col1<-c("","","","","","","","","","","","","","","","",17,18,19,20,21,22,23,24,25)
          PM25_col2<-c("","","","","","","","",9,10,11,12,13,14,15,16,"","","","","","","","","")
          PM25_col3<-c(1,2,3,4,5,6,7,8,"","","","","","","","","","","","","","","","","")
          PM25<-gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3))
          print("PM25 Stgo")
        } else
        {
          if(Contaminantes[p]=="O3")
          {
            #Insertar araña O3-Stgo Aquí#
            O3_col1<-c("","","","","","","","","","","","","","","","",17,18,19,20,21,22,23,24,25)
            O3_col2<-c("","","","","","","","",9,10,11,12,13,14,15,16,"","","","","","","","","")
            O3_col3<-c(1,2,3,4,5,6,7,8,"","","","","","","","","","","","","","","","","")
            O3<-gsub(" ", "",paste(O3_col1,O3_col2,O3_col3))
            print("O3 Stgo")
          } else
          {
            if(Contaminantes[p]=="CO")
            {
              #Insertar araña CO-Stgo Aquí#
              CO_col1<-c("","","","","","","","","","","","","","","","",17,18,19,20,21,22,23,24,25)
              CO_col2<-c("","","","","","","","",9,10,11,12,13,14,15,16,"","","","","","","","","")
              CO_col3<-c(1,2,3,4,5,6,7,8,"","","","","","","","","","","","","","","","","")
              CO<-gsub(" ", "",paste(CO_col1,CO_col2,CO_col3))
              print("CO Stgo")
            } else
            {
              print(paste("Contaminante",auxiliar,"no soportado en el Software"))
            }
          }
        }
      }
    }
    data<-data.frame(col_fecha,col_ciudad,PM10,PM25,O3,CO) #Crear data frame con datos falta ampliar
    data
    write.csv(data,gsub(" ", "",paste(Comunas[i],".CSV")), row.names = FALSE) #Crear CSV
  } else
    {
      print( paste("Comuna",aux,"no soportada en el Software"))
  }
}
i = NULL; p= NULL

