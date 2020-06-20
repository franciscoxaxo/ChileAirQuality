install.packages("tidyr")
install.packages("lubridate")
install.packages("dplyr")
install.packages("rvest")
install.packages("purrr")
install.packages("robotstxt")
install.packages("xml2")


#Llamada de librerias

library(robotstxt)
library(selectr)
library(xml2)
library(dplyr)
library(stringr)
library(magrittr)
library(tidyr)
library(lubridate)
library(tibble)
library(purrr)
library(rvest)

#Imput de datos#
Comunas <- c("SA","TA")
Contaminantes <-c("PM10","PM25","CH4")
fi<-"02/05/2020 1:00"
ft<-"03/05/2020 1:00"

#Fin de Imput#


#Conversion de Fechas#
#Fechas para tablas#
Fecha_inicio<‐ as.POSIXct(strptime(fi, format = "%d/%m/%Y %H:%M", "GMT"))
Fecha_termino<- as.POSIXct(strptime(ft, format = "%d/%m/%Y %H:%M", "GMT"))


#Fechas para araña#
Fecha_inicio_para_arana<-as.character(Fecha_inicio, format("%y%m%d"))
Hora_inicio_para_arana<- as.character(Fecha_inicio, format( "%H%M")) 
Fecha_termino_para_arana<- as.character(Fecha_termino, format("%y%m%d"))
Hora_termino_para_arana<-as.character(Fecha_termino, format( "%H%M")) 
horas<-(as.numeric(Fecha_termino)/3600-as.numeric(Fecha_inicio)/3600) #horas entre fecha y fecha

#Selector de arañas#

for(i in 1:length(Comunas))
{
  aux<-Comunas[i]
  #Crear SA.CSV?#
  for(i in 1:horas) 
  {
    col_fecha[i]<-Fecha_inicio+3600*i
  }
  i = NULL
  col_ciudad<-rep(Comunas[i],horas)
  if(aux == "SA")
    
    {
    for(p in 1:length(Contaminantes))
    {
      auxiliar<- Contaminantes[p]
      if(Contaminantes[p]=="PM10")
      {
        #Insertar araña PM10-Stgo Aquí#
        PM10_col1
        PM10_col2
        PM10_col3
        PM10<-gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3))
        
        print("PM10 Stgo")
      } else
      {
        if(Contaminantes[p]=="PM25")
        {
          #Insertar araña PM25-Stgo Aquí#
          PM25_col1
          PM25_col2
          PM25_col3
          PM25<-gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3))
          print("PM25 Stgo")
        } else
        {
          if(Contaminantes[p]=="O3")
          {
            #Insertar araña O3-Stgo Aquí#
            O3_col1
            O3_col2
            O3_col3
            O3<-gsub(" ", "",paste(O3_col1,O3_col2,O3_col3))
            print("O3 Stgo")
          } else
          {
            if(Contaminantes[p]=="CO")
            {
              #Insertar araña CO-Stgo Aquí#
              CO_col1
              CO_col2
              CO_col3
              CO<-gsub(" ", "",paste(CO_col1,CO_col2,CO_col3))
              print("CO Stgo")
            } else{
              print(paste("Contaminante",auxiliar,"no soportado en el Software"))
            }
          }
        }
      }
    }
    data<-data.frame(col_fecha,col_ciudad,PM10,PM25,CO,O3) #Crear data frame con datos
    write.csv(data,gsub(" ", "",paste("C:\\",Comunas[i],".CSV")), row.names = FALSE) #Crear CSV
  } else
    {
      print( paste("Comuna",aux,"no soportada en el Software"))
  }
}
i = NULL
p= NULL
