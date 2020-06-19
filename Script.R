#install.packages("tidyr")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("rvest")
#install.packages("purrr")
#install.packages("robotstxt")


#Llamada de librerias

library(robotstxt)
library(selectr)
library(xml2)
library(dplyr)
library(stringr)
library(magrittr)
library(tidyr)
library(ggplot2)
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


for(i in Fecha_inicio:Fecha_termino )
{
  col_fecha[i]<-Fecha_inicio++
}


i = NULL
col_fecha


#z<-gsub(" ", "",paste(a,b,d)) permite unir 3 columnas a,b y d



#Selector de arañas#

for(i in 1:length(Comunas))
{
  aux<-Comunas[i]
  #Crear SA.CSV?#
  #Columna fecha con formato fecha#
  #Columna Comuna#
  if(aux == "SA")
    {
    for(p in 1:length(Contaminantes))
    {
      auxiliar<- Contaminantes[p]
      if(Contaminantes[p]=="PM10")
      {
        #Insertar araña PM10-Stgo Aquí#
        #Concatenar 3 columnas#
        #Unir a SA.CSV#
        print("PM10 Stgo")
      } else
      {
        if(Contaminantes[p]=="PM25")
        {
          #Insertar araña PM25-Stgo Aquí#
          #Concatenar 3 columnas#
          #Unir a SA.CSV#
          print("PM25 Stgo")
        } else
        {
          if(Contaminantes[p]=="O3")
          {
            #Insertar araña O3-Stgo Aquí#
            #Concatenar 3 columnas#
            #Unir a SA.CSV#
            print("O3 Stgo")
          } else
          {
            if(Contaminantes[p]=="CO")
            {
              #Insertar araña CO-Stgo Aquí#
              #Unir 3 columnas de contaminantes#
              #Unir a SA.CSV#
              print("CO Stgo")
            } else{
              print(paste("Contaminante",auxiliar,"no soportado en el Software"))
            }
          }
        }
      }
    }

  } else
    {
      print( paste("Comuna",aux,"no soportada en el Software"))
  }
  
}
i = NULL
p= NULL
