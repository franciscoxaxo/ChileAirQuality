#install.packages("robotstxt")
#install.packages("xml2")
#install.packages("rvest")

#Llamada de librerias

library(robotstxt)
library(selectr)
library(xml2)
library(stringr)
library(magrittr)
library(tibble)
library(rvest)

#Imput de datos#
Comunas <- c("SA","TA","QU")
Contaminantes <-c("PM10","PM25","NO","NOX","CH4")
imput_fecha_inicio<-"01/03/2014"
imput_fecha_termino<-"02/03/2014"

#Fin de Imput#

#Conversion de Fechas#
#Fechas para tablas#
fi<-paste(imput_fecha_inicio,"1:00")
ft<-paste(imput_fecha_termino,"23:00")
Fecha_inicio<- as.POSIXct(strptime(fi, format = "%d/%m/%Y %H:%M"))
Fecha_termino<- as.POSIXct(strptime(ft, format = "%d/%m/%Y %H:%M"))

#Fechas para araña#
Fecha_inicio_para_arana<-as.character(Fecha_inicio, format("%y%m%d"))
Hora_inicio_para_arana<- as.character(Fecha_inicio, format( "%H%M")) 
Fecha_termino_para_arana<- as.character(Fecha_termino, format("%y%m%d"))
Hora_termino_para_arana<-as.character(Fecha_termino, format( "%H%M"))
id_fecha<-gsub(" ","",paste(".horario.horario.ic&from=",Fecha_inicio_para_arana,"&to=",Fecha_termino_para_arana))
horas<-(as.numeric(Fecha_termino)/3600-as.numeric(Fecha_inicio)/3600) #horas entre fecha y fecha

col_fecha= NULL; n =NULL
for(n in 0:horas) 
{
  col_fecha<-c(col_fecha, as.character(Fecha_inicio+3600*n, "%d/%m/%Y %H:%M"))
}
#Selector de arañas#

aux = NULL; i =NULL; data = NULL; auxiliar= NULL; p= NULL
for(i in 1:length(Comunas))
{
  aux<-Comunas[i]
  
  if(aux == "SA")
    {
    ciudad_arana<-"RM/D14"
    col_ciudad<-rep(aux,horas+1)
    data<-data.frame(col_fecha,col_ciudad)
    p= NULL; auxiliar = NULL
    for(p in 1:length(Contaminantes))
    {
      auxiliar<- Contaminantes[p]
      if(auxiliar=="PM10")
      {
        #Insertar araña PM10-Stgo Aquí#
        contaminante_arana<-"PM10//PM10"
        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,"/Cal/",contaminante_arana,id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
        PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
        PM10_col1<-PM10_Bruto$Registros.validados
        PM10_col2<-PM10_Bruto$Registros.preliminares
        PM10_col3<-PM10_Bruto$Registros.no.validados
        PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
        data<-data.frame(data,PM10)
        print("PM10 Stgo")
      } else
      {
        if(Contaminantes[p]=="PM25")
        {
          #Insertar araña PM25-Stgo Aquí#
          contaminante_arana<-"PM25//PM25"
          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,"/Cal/",contaminante_arana,id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
          PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
          PM25_col1<-PM25_Bruto$Registros.validados
          PM25_col2<-PM25_Bruto$Registros.preliminares
          PM25_col3<-PM25_Bruto$Registros.no.validados
          PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
          data<-data.frame(data,PM25)
          print("PM25 Stgo")
        } else
        {
          if(Contaminantes[p]=="O3")
          {
            #Insertar araña O3-Stgo Aquí#
            O3_col1<-c("","","","","","","","","","","","","","","","",17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)
            O3_col2<-c("","","","","","","","",9,10,11,12,13,14,15,16,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
            O3_col3<-c(1,2,3,4,5,6,7,8,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
            O3<-gsub(" ", "",paste(O3_col1,O3_col2,O3_col3))
            data<-data.frame(data,O3)
            print("O3 Stgo")
          } else
          {
            if(Contaminantes[p]=="CO")
            {
              #Insertar araña CO-Stgo Aquí#
              CO_col1<-c("","","","","","","","","","","","","","","","",17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)
              CO_col2<-c("","","","","","","","",9,10,11,12,13,14,15,16,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
              CO_col3<-c(1,2,3,4,5,6,7,8,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
              CO<-gsub(" ", "",paste(CO_col1,CO_col2,CO_col3))
              data<-data.frame(data,CO)
              print("CO Stgo")
            } else
            {
              if(Contaminantes[p]=="NO")
              {
                #Insertar araña CO-Stgo Aquí#
                NO_col1<-c("","","","","","","","","","","","","","","","",17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)
                NO_col2<-c("","","","","","","","",9,10,11,12,13,14,15,16,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
                NO_col3<-c(1,2,3,4,5,6,7,8,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
                NO<-gsub(" ", "",paste(NO_col1,NO_col2,NO_col3))
                data<-data.frame(data,NO)
                print("NO Stgo")
              } else
              {
                if(Contaminantes[p]=="NO2")
                {
                  #Insertar araña CO-Stgo Aquí#
                  NO2_col1<-c("","","","","","","","","","","","","","","","",17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)
                  NO2_col2<-c("","","","","","","","",9,10,11,12,13,14,15,16,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
                  NO2_col3<-c(1,2,3,4,5,6,7,8,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
                  NO2<-gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3))
                  data<-data.frame(data,NO2)
                  print("NO2 Stgo")
                } else
                {
                  if(Contaminantes[p]=="NOX")
                  {
                    #Insertar araña CO-Stgo Aquí#
                    NOX_col1<-c("","","","","","","","","","","","","","","","",17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)
                    NOX_col2<-c("","","","","","","","",9,10,11,12,13,14,15,16,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
                    NOX_col3<-c(1,2,3,4,5,6,7,8,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
                    NOX<-gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3))
                    data<-data.frame(data,NOX)
                    print("NOX Stgo")
                  } else
                  {
                    print(paste("Contaminante",auxiliar,"no soportado en el Software"))
                  }
                }
              }
            }
          }
        }
      }
    }
    data
    write.csv(data,gsub(" ", "",paste(Comunas[i],".CSV")), row.names = FALSE) #Crear CSV
  } else
    {
      print(paste("Comuna",aux,"no soportada en el Software"))
  }
}


