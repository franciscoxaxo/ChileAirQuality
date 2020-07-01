##CODIGOS DE COMUNAS SOPORTADAS##
#PARQUE O'HIGGINS---->SA
#Cerrillos---->CE
#Cerrillos i---->CEi
#Cerro Navia----->CN
#El Bosque---->EB
#Independencia----->IN
#La Florida---->LF
#Las Condes---->LC
#Pudahuel---->PU
#Puente Alto---->PA
#Quilicura----->QU
#Quilicura i----->QUI

#Contaminantes y Componentes meteorologicas soportadas#
#PM10--->PM10
#PM25---->PM25
#O3--->O3
#NO----->NO
#NO2----->NO2
#NOX----->NOX
#Temperatura--->Temperatura
#Humedad relativa--->HR
#Direccion del viento----->WD
#Velocidad del Viento---->WS

#Formato Fecha
#dd/mm/aaaa

#Input de datos#
#Comunas <- c("SA","PA","QU","LC")
#Contaminantes <-c("PM10","PM25","NO","NOX","CH4","Temperatura", "HR", "WD","WS")
#input_fecha_inicio<-"01/04/2014"
#input_fecha_termino<-"02/04/2014"

#Fin de Input#
spider_sinca<-function(Comunas, Contaminantes, input_fecha_inicio, input_fecha_termino){
  #Conversion de Fechas#
  #Fechas para tablas#
  fi<-paste(input_fecha_inicio,"1:00")
  ft<-paste(input_fecha_termino,"23:00")
  Fecha_inicio<- as.POSIXct(strptime(fi, format = "%d/%m/%Y %H:%M"))
  Fecha_termino<- as.POSIXct(strptime(ft, format = "%d/%m/%Y %H:%M"))
  
  #Fechas para ara?a#
  Fecha_inicio_para_arana<-as.character(Fecha_inicio, format("%y%m%d"))
  Hora_inicio_para_arana<- as.character(Fecha_inicio, format( "%H%M")) 
  Fecha_termino_para_arana<- as.character(Fecha_termino, format("%y%m%d"))
  Hora_termino_para_arana<-as.character(Fecha_termino, format( "%H%M"))
  id_fecha<-gsub(" ","",paste("from=",Fecha_inicio_para_arana,"&to=",Fecha_termino_para_arana))
  horas<-(as.numeric(Fecha_termino)/3600-as.numeric(Fecha_inicio)/3600) #horas entre fecha y fecha
  
  #Data frame vacio#
  
  
  col_fecha= NULL; n =NULL
  for(n in 0:horas) 
  {
    col_fecha<-c(col_fecha, as.character(Fecha_inicio+3600*n, "%d/%m/%Y %H:%M"))
  }
  data<-data.frame(col_fecha)
  data_total<-data.frame()
  #Selector de aranas#
  
  i =NULL; aux= NULL
  for(i in 1:length(Comunas))
  {
    aux<-Comunas[i]
    auxiliar = NULL;  data = NULL;  p= NULL
    if(aux == "SA")
    {
      ciudad_arana<-"RM/D14"
      col_ciudad<-rep(aux,horas+1)
      data<-data.frame(col_fecha,col_ciudad)
      p= NULL; auxiliar = NULL
      for(p in 1:length(Contaminantes))
      {
        auxiliar<- Contaminantes[p]
        url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL; PM10 = NULL
        if(auxiliar=="PM10")
        {
          contaminante_arana<-"/Cal/PM10//PM10"
          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
          try(
            {
              PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
              PM10_col1<-PM10_Bruto$Registros.validados
              PM10_col2<-PM10_Bruto$Registros.preliminares
              PM10_col3<-PM10_Bruto$Registros.no.validados
              PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
              data<-data.frame(data,PM10)
              print(paste(auxiliar,aux))
            }
            ,silent = True)
          
        } else
        {
          url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL; PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL
          if(Contaminantes[p]=="PM25")
          {
            contaminante_arana<-"/Cal/PM25//PM25"
            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
            try(
              {
                PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                PM25_col1<-PM25_Bruto$Registros.validados
                PM25_col2<-PM25_Bruto$Registros.preliminares
                PM25_col3<-PM25_Bruto$Registros.no.validados
                PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                data<-data.frame(data,PM25)
                print(paste(auxiliar,aux))
              }
              , silent = TRUE)
          } else
          {
            url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL
            if(Contaminantes[p]=="O3")
            {
              contaminante_arana<-"/Cal/0008//0008"
              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
              try(
                {
                  O3_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                  O3_col1<-O3_Bruto$Registros.validados
                  O3_col2<-O3_Bruto$Registros.preliminares
                  O3_col3<-O3_Bruto$Registros.no.validados
                  O3<-gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                  data<-data.frame(data,O3)
                  print(paste(auxiliar,aux))
                }
                , silent = TRUE)
            } else
            {
              url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL
              if(Contaminantes[p]=="CO")
              {
                contaminante_arana<-"/Cal/0004//0004"
                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                try(
                  {
                    CO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                    CO_col1<-CO_Bruto$Registros.validados
                    CO_col2<-CO_Bruto$Registros.preliminares
                    CO_col3<-CO_Bruto$Registros.no.validados
                    CO<-gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                    data<-data.frame(data,CO)
                    print(paste(auxiliar,aux))
                  }
                  , silent = TRUE)
              } else
              {
                url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL
                if(Contaminantes[p]=="NO")
                {
                  contaminante_arana<-"/Cal/0002//0002"
                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                  try(
                    {
                      NO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                      NO_col1<-NO_Bruto$Registros.validados
                      NO_col2<-NO_Bruto$Registros.preliminares
                      NO_col3<-NO_Bruto$Registros.no.validados
                      NO<-gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                      data<-data.frame(data,NO)
                      print(paste(auxiliar,aux))
                    }
                    , silent = TRUE)
                } else
                {
                  url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL
                  if(Contaminantes[p]=="NO2")
                  {
                    contaminante_arana<-"/Cal/0003//0003"
                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                    try(
                      {
                        NO2_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                        NO2_col1<-NO2_Bruto$Registros.validados
                        NO2_col2<-NO2_Bruto$Registros.preliminares
                        NO2_col3<-NO2_Bruto$Registros.no.validados
                        NO2<-gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                        data<-data.frame(data,NO2)
                        print(paste(auxiliar,aux))
                      }
                    )
                  } else
                  {
                    url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                    if(Contaminantes[p]=="NOX")
                    {
                      contaminante_arana<-"/Cal/0NOX//0NOX"
                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                      try(
                        {
                          NOX_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                          NOX_col1<-NOX_Bruto$Registros.validados
                          NOX_col2<-NOX_Bruto$Registros.preliminares
                          NOX_col3<-NOX_Bruto$Registros.no.validados
                          NOX<-gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                          data<-data.frame(data,NOX)
                          print(paste(auxiliar,aux))
                        }
                        , silent = TRUE)
                    } else
                    {
                      url = NULL; contaminante_arana= NULL; temperatura_bruto =NULL;temperatura_col1 = NULL; temperatura = NULL
                      if(Contaminantes[p]=="Temperatura")
                      {
                        contaminante_arana<-"/Met/TEMP//"
                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                        try(
                          {
                            temperatura_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                            temperatura_col1<-temperatura_bruto$X
                            Temperatura<-gsub("NA","",gsub(" ", "",temperatura_col1))
                            data<-data.frame(data,Temperatura)
                            print(paste(auxiliar,aux))
                          }
                          , silent = TRUE)
                      } else
                      {
                        url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                        if(Contaminantes[p]=="HR")
                        {
                          contaminante_arana<-"/Met/RHUM//"
                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                          try(
                            {
                              HR_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                              HR_col1<-HR_bruto$X
                              HR<-gsub("NA","",gsub(" ", "",HR_col1))
                              data<-data.frame(data,HR)
                              print(paste(auxiliar,aux))
                            }
                            , silent = TRUE)
                        } else
                        {
                          url = NULL; contaminante_arana= NULL; WD_bruto =NULL;WD_col1 = NULL; WD = NULL
                          if(Contaminantes[p]=="WD")
                          {
                            contaminante_arana<-"/Met/WDIR//"
                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000_spec.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                            try(
                              {
                                WD_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                WD_col1<-WD_bruto$X
                                WD<-gsub("NA","",gsub(" ", "",WD_col1))
                                data<-data.frame(data,WD)
                                print(paste(auxiliar,aux))
                              }
                              , silent = TRUE)
                          } else
                          {
                            url = NULL; contaminante_arana= NULL; WS_bruto =NULL;WS_col1 = NULL; WS = NULL
                            if(Contaminantes[p]=="WS")
                            {
                              contaminante_arana<-"/Met/WSPD//"
                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                              try(
                                {
                                  WS_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                  WS_col1<-WS_bruto$X
                                  WS<-gsub("NA","",gsub(" ", "",WS_col1))
                                  data<-data.frame(data,WS)
                                  print(paste(auxiliar,aux))
                                }
                                , silent = TRUE)
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
            }
          }
        }
      }
      data_Stgo<-data
      try(
        {
          data_total<-rbind(data_Stgo)
        }
        , silent = T)
      
    } else
    {
      auxiliar = NULL; data = NULL; p= NULL
      if(aux == "CE")
      {
        ciudad_arana<-"RM/D31"
        col_ciudad<-rep(aux,horas+1)
        data<-data.frame(col_fecha,col_ciudad)
        p= NULL; auxiliar = NULL
        for(p in 1:length(Contaminantes))
        {
          auxiliar<- Contaminantes[p]
          url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL; PM10 = NULL
          if(auxiliar=="PM10")
          {
            contaminante_arana<-"/Cal/PM10//PM10"
            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
            try(
              {
                PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                PM10_col1<-PM10_Bruto$Registros.validados
                PM10_col2<-PM10_Bruto$Registros.preliminares
                PM10_col3<-PM10_Bruto$Registros.no.validados
                PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
                data<-data.frame(data,PM10)
                print(paste(auxiliar,aux))
              }
              ,silent = True)
            
          } else
          {
            url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL; PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL
            if(Contaminantes[p]=="PM25")
            {
              contaminante_arana<-"/Cal/PM25//PM25"
              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
              try(
                {
                  PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                  PM25_col1<-PM25_Bruto$Registros.validados
                  PM25_col2<-PM25_Bruto$Registros.preliminares
                  PM25_col3<-PM25_Bruto$Registros.no.validados
                  PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                  data<-data.frame(data,PM25)
                  print(paste(auxiliar,aux))
                }
                , silent = TRUE)
            } else
            {
              url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL
              if(Contaminantes[p]=="O3")
              {
                contaminante_arana<-"/Cal/0008//0008"
                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                try(
                  {
                    O3_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                    O3_col1<-O3_Bruto$Registros.validados
                    O3_col2<-O3_Bruto$Registros.preliminares
                    O3_col3<-O3_Bruto$Registros.no.validados
                    O3<-gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                    data<-data.frame(data,O3)
                    print(paste(auxiliar,aux))
                  }
                  , silent = TRUE)
              } else
              {
                url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL
                if(Contaminantes[p]=="CO")
                {
                  contaminante_arana<-"/Cal/0004//0004"
                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                  try(
                    {
                      CO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                      CO_col1<-CO_Bruto$Registros.validados
                      CO_col2<-CO_Bruto$Registros.preliminares
                      CO_col3<-CO_Bruto$Registros.no.validados
                      CO<-gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                      data<-data.frame(data,CO)
                      print(paste(auxiliar,aux))
                    }
                    , silent = TRUE)
                } else
                {
                  url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL
                  if(Contaminantes[p]=="NO")
                  {
                    contaminante_arana<-"/Cal/0002//0002"
                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                    try(
                      {
                        NO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                        NO_col1<-NO_Bruto$Registros.validados
                        NO_col2<-NO_Bruto$Registros.preliminares
                        NO_col3<-NO_Bruto$Registros.no.validados
                        NO<-gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                        data<-data.frame(data,NO)
                        print(paste(auxiliar,aux))
                      }
                      , silent = TRUE)
                  } else
                  {
                    url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL
                    if(Contaminantes[p]=="NO2")
                    {
                      contaminante_arana<-"/Cal/0003//0003"
                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                      try(
                        {
                          NO2_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                          NO2_col1<-NO2_Bruto$Registros.validados
                          NO2_col2<-NO2_Bruto$Registros.preliminares
                          NO2_col3<-NO2_Bruto$Registros.no.validados
                          NO2<-gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                          data<-data.frame(data,NO2)
                          print(paste(auxiliar,aux))
                        }
                      )
                    } else
                    {
                      url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                      if(Contaminantes[p]=="NOX")
                      {
                        contaminante_arana<-"/Cal/0NOX//0NOX"
                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                        try(
                          {
                            NOX_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                            NOX_col1<-NOX_Bruto$Registros.validados
                            NOX_col2<-NOX_Bruto$Registros.preliminares
                            NOX_col3<-NOX_Bruto$Registros.no.validados
                            NOX<-gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                            data<-data.frame(data,NOX)
                            print(paste(auxiliar,aux))
                          }
                          , silent = TRUE)
                      } else
                      {
                        url = NULL; contaminante_arana= NULL; temperatura_bruto =NULL;temperatura_col1 = NULL; temperatura = NULL
                        if(Contaminantes[p]=="Temperatura")
                        {
                          contaminante_arana<-"/Met/TEMP//"
                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                          try(
                            {
                              temperatura_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                              temperatura_col1<-temperatura_bruto$X
                              Temperatura<-gsub("NA","",gsub(" ", "",temperatura_col1))
                              data<-data.frame(data,Temperatura)
                              print(paste(auxiliar,aux))
                            }
                            , silent = TRUE)
                        } else
                        {
                          url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                          if(Contaminantes[p]=="HR")
                          {
                            contaminante_arana<-"/Met/RHUM//"
                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                            try(
                              {
                                HR_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                HR_col1<-HR_bruto$X
                                HR<-gsub("NA","",gsub(" ", "",HR_col1))
                                data<-data.frame(data,HR)
                                print(paste(auxiliar,aux))
                              }
                              , silent = TRUE)
                          } else
                          {
                            url = NULL; contaminante_arana= NULL; WD_bruto =NULL;WD_col1 = NULL; WD = NULL
                            if(Contaminantes[p]=="WD")
                            {
                              contaminante_arana<-"/Met/WDIR//"
                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000_spec.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                              try(
                                {
                                  WD_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                  WD_col1<-WD_bruto$X
                                  WD<-gsub("NA","",gsub(" ", "",WD_col1))
                                  data<-data.frame(data,WD)
                                  print(paste(auxiliar,aux))
                                }
                                , silent = TRUE)
                            } else
                            {
                              url = NULL; contaminante_arana= NULL; WS_bruto =NULL;WS_col1 = NULL; WS = NULL
                              if(Contaminantes[p]=="WS")
                              {
                                contaminante_arana<-"/Met/WSPD//"
                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                try(
                                  {
                                    WS_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                    WS_col1<-WS_bruto$X
                                    WS<-gsub("NA","",gsub(" ", "",WS_col1))
                                    data<-data.frame(data,WS)
                                    print(paste(auxiliar,aux))
                                  }
                                  , silent = TRUE)
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
              }
            }
          }
        }
        data_Cerrillos<-data
        try(
          {
            data_total<-rbind(data_total, data_Cerrillos)
          }
          , silent = T)
      } else
      {
        auxiliar = NULL; data = NULL; p= NULL
        if(aux == "CEi")
        {
          ciudad_arana<-"RM/D16"
          col_ciudad<-rep(aux,horas+1)
          data<-data.frame(col_fecha,col_ciudad)
          p= NULL; auxiliar = NULL
          for(p in 1:length(Contaminantes))
          {
            auxiliar<- Contaminantes[p]
            url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL; PM10 = NULL
            if(auxiliar=="PM10")
            {
              contaminante_arana<-"/Cal/PM10//PM10"
              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
              try(
                {
                  PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                  PM10_col1<-PM10_Bruto$Registros.validados
                  PM10_col2<-PM10_Bruto$Registros.preliminares
                  PM10_col3<-PM10_Bruto$Registros.no.validados
                  PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
                  data<-data.frame(data,PM10)
                  print(paste(auxiliar,aux))
                }
                ,silent = True)
              
            } else
            {
              url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL; PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL
              if(Contaminantes[p]=="PM25")
              {
                contaminante_arana<-"/Cal/PM25//PM25"
                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                try(
                  {
                    PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                    PM25_col1<-PM25_Bruto$Registros.validados
                    PM25_col2<-PM25_Bruto$Registros.preliminares
                    PM25_col3<-PM25_Bruto$Registros.no.validados
                    PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                    data<-data.frame(data,PM25)
                    print(paste(auxiliar,aux))
                  }
                  , silent = TRUE)
              } else
              {
                url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL
                if(Contaminantes[p]=="O3")
                {
                  contaminante_arana<-"/Cal/0008//0008"
                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                  try(
                    {
                      O3_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                      O3_col1<-O3_Bruto$Registros.validados
                      O3_col2<-O3_Bruto$Registros.preliminares
                      O3_col3<-O3_Bruto$Registros.no.validados
                      O3<-gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                      data<-data.frame(data,O3)
                      print(paste(auxiliar,aux))
                    }
                    , silent = TRUE)
                } else
                {
                  url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL
                  if(Contaminantes[p]=="CO")
                  {
                    contaminante_arana<-"/Cal/0004//0004"
                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                    try(
                      {
                        CO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                        CO_col1<-CO_Bruto$Registros.validados
                        CO_col2<-CO_Bruto$Registros.preliminares
                        CO_col3<-CO_Bruto$Registros.no.validados
                        CO<-gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                        data<-data.frame(data,CO)
                        print(paste(auxiliar,aux))
                      }
                      , silent = TRUE)
                  } else
                  {
                    url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL
                    if(Contaminantes[p]=="NO")
                    {
                      contaminante_arana<-"/Cal/0002//0002"
                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                      try(
                        {
                          NO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                          NO_col1<-NO_Bruto$Registros.validados
                          NO_col2<-NO_Bruto$Registros.preliminares
                          NO_col3<-NO_Bruto$Registros.no.validados
                          NO<-gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                          data<-data.frame(data,NO)
                          print(paste(auxiliar,aux))
                        }
                        , silent = TRUE)
                    } else
                    {
                      url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL
                      if(Contaminantes[p]=="NO2")
                      {
                        contaminante_arana<-"/Cal/0003//0003"
                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                        try(
                          {
                            NO2_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                            NO2_col1<-NO2_Bruto$Registros.validados
                            NO2_col2<-NO2_Bruto$Registros.preliminares
                            NO2_col3<-NO2_Bruto$Registros.no.validados
                            NO2<-gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                            data<-data.frame(data,NO2)
                            print(paste(auxiliar,aux))
                          }
                        )
                      } else
                      {
                        url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                        if(Contaminantes[p]=="NOX")
                        {
                          contaminante_arana<-"/Cal/0NOX//0NOX"
                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                          try(
                            {
                              NOX_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                              NOX_col1<-NOX_Bruto$Registros.validados
                              NOX_col2<-NOX_Bruto$Registros.preliminares
                              NOX_col3<-NOX_Bruto$Registros.no.validados
                              NOX<-gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                              data<-data.frame(data,NOX)
                              print(paste(auxiliar,aux))
                            }
                            , silent = TRUE)
                        } else
                        {
                          url = NULL; contaminante_arana= NULL; temperatura_bruto =NULL;temperatura_col1 = NULL; temperatura = NULL
                          if(Contaminantes[p]=="Temperatura")
                          {
                            contaminante_arana<-"/Met/TEMP//"
                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                            try(
                              {
                                temperatura_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                temperatura_col1<-temperatura_bruto$X
                                Temperatura<-gsub("NA","",gsub(" ", "",temperatura_col1))
                                data<-data.frame(data,Temperatura)
                                print(paste(auxiliar,aux))
                              }
                              , silent = TRUE)
                          } else
                          {
                            url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                            if(Contaminantes[p]=="HR")
                            {
                              contaminante_arana<-"/Met/RHUM//"
                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                              try(
                                {
                                  HR_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                  HR_col1<-HR_bruto$X
                                  HR<-gsub("NA","",gsub(" ", "",HR_col1))
                                  data<-data.frame(data,HR)
                                  print(paste(auxiliar,aux))
                                }
                                , silent = TRUE)
                            } else
                            {
                              url = NULL; contaminante_arana= NULL; WD_bruto =NULL;WD_col1 = NULL; WD = NULL
                              if(Contaminantes[p]=="WD")
                              {
                                contaminante_arana<-"/Met/WDIR//"
                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000_spec.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                try(
                                  {
                                    WD_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                    WD_col1<-WD_bruto$X
                                    WD<-gsub("NA","",gsub(" ", "",WD_col1))
                                    data<-data.frame(data,WD)
                                    print(paste(auxiliar,aux))
                                  }
                                  , silent = TRUE)
                              } else
                              {
                                url = NULL; contaminante_arana= NULL; WS_bruto =NULL;WS_col1 = NULL; WS = NULL
                                if(Contaminantes[p]=="WS")
                                {
                                  contaminante_arana<-"/Met/WSPD//"
                                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                  try(
                                    {
                                      WS_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                      WS_col1<-WS_bruto$X
                                      WS<-gsub("NA","",gsub(" ", "",WS_col1))
                                      data<-data.frame(data,WS)
                                      print(paste(auxiliar,aux))
                                    }
                                    , silent = TRUE)
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
                }
              }
            }
          }
          data_Cerrillos_i<-data
          try(
            {
              data_total<-rbind(data_total, data_Cerrillos_i)
            }
            , silent = T)
        } else
        {
          auxiliar = NULL; data = NULL; p= NULL
          if(aux == "CN")
          {
            ciudad_arana<-"RM/D18"
            col_ciudad<-rep(aux,horas+1)
            data<-data.frame(col_fecha,col_ciudad)
            p= NULL; auxiliar = NULL
            for(p in 1:length(Contaminantes))
            {
              auxiliar<- Contaminantes[p]
              url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL; PM10 = NULL
              if(auxiliar=="PM10")
              {
                contaminante_arana<-"/Cal/PM10//PM10"
                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                try(
                  {
                    PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                    PM10_col1<-PM10_Bruto$Registros.validados
                    PM10_col2<-PM10_Bruto$Registros.preliminares
                    PM10_col3<-PM10_Bruto$Registros.no.validados
                    PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
                    data<-data.frame(data,PM10)
                    print(paste(auxiliar,aux))
                  }
                  ,silent = True)
                
              } else
              {
                url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL; PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL
                if(Contaminantes[p]=="PM25")
                {
                  contaminante_arana<-"/Cal/PM25//PM25"
                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                  try(
                    {
                      PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                      PM25_col1<-PM25_Bruto$Registros.validados
                      PM25_col2<-PM25_Bruto$Registros.preliminares
                      PM25_col3<-PM25_Bruto$Registros.no.validados
                      PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                      data<-data.frame(data,PM25)
                      print(paste(auxiliar,aux))
                    }
                    , silent = TRUE)
                } else
                {
                  url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL
                  if(Contaminantes[p]=="O3")
                  {
                    contaminante_arana<-"/Cal/0008//0008"
                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                    try(
                      {
                        O3_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                        O3_col1<-O3_Bruto$Registros.validados
                        O3_col2<-O3_Bruto$Registros.preliminares
                        O3_col3<-O3_Bruto$Registros.no.validados
                        O3<-gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                        data<-data.frame(data,O3)
                        print(paste(auxiliar,aux))
                      }
                      , silent = TRUE)
                  } else
                  {
                    url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL
                    if(Contaminantes[p]=="CO")
                    {
                      contaminante_arana<-"/Cal/0004//0004"
                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                      try(
                        {
                          CO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                          CO_col1<-CO_Bruto$Registros.validados
                          CO_col2<-CO_Bruto$Registros.preliminares
                          CO_col3<-CO_Bruto$Registros.no.validados
                          CO<-gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                          data<-data.frame(data,CO)
                          print(paste(auxiliar,aux))
                        }
                        , silent = TRUE)
                    } else
                    {
                      url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL
                      if(Contaminantes[p]=="NO")
                      {
                        contaminante_arana<-"/Cal/0002//0002"
                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                        try(
                          {
                            NO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                            NO_col1<-NO_Bruto$Registros.validados
                            NO_col2<-NO_Bruto$Registros.preliminares
                            NO_col3<-NO_Bruto$Registros.no.validados
                            NO<-gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                            data<-data.frame(data,NO)
                            print(paste(auxiliar,aux))
                          }
                          , silent = TRUE)
                      } else
                      {
                        url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL
                        if(Contaminantes[p]=="NO2")
                        {
                          contaminante_arana<-"/Cal/0003//0003"
                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                          try(
                            {
                              NO2_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                              NO2_col1<-NO2_Bruto$Registros.validados
                              NO2_col2<-NO2_Bruto$Registros.preliminares
                              NO2_col3<-NO2_Bruto$Registros.no.validados
                              NO2<-gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                              data<-data.frame(data,NO2)
                              print(paste(auxiliar,aux))
                            }
                          )
                        } else
                        {
                          url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                          if(Contaminantes[p]=="NOX")
                          {
                            contaminante_arana<-"/Cal/0NOX//0NOX"
                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                            try(
                              {
                                NOX_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                NOX_col1<-NOX_Bruto$Registros.validados
                                NOX_col2<-NOX_Bruto$Registros.preliminares
                                NOX_col3<-NOX_Bruto$Registros.no.validados
                                NOX<-gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                                data<-data.frame(data,NOX)
                                print(paste(auxiliar,aux))
                              }
                              , silent = TRUE)
                          } else
                          {
                            url = NULL; contaminante_arana= NULL; temperatura_bruto =NULL;temperatura_col1 = NULL; temperatura = NULL
                            if(Contaminantes[p]=="Temperatura")
                            {
                              contaminante_arana<-"/Met/TEMP//"
                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                              try(
                                {
                                  temperatura_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                  temperatura_col1<-temperatura_bruto$X
                                  Temperatura<-gsub("NA","",gsub(" ", "",temperatura_col1))
                                  data<-data.frame(data,Temperatura)
                                  print(paste(auxiliar,aux))
                                }
                                , silent = TRUE)
                            } else
                            {
                              url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                              if(Contaminantes[p]=="HR")
                              {
                                contaminante_arana<-"/Met/RHUM//"
                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                try(
                                  {
                                    HR_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                    HR_col1<-HR_bruto$X
                                    HR<-gsub("NA","",gsub(" ", "",HR_col1))
                                    data<-data.frame(data,HR)
                                    print(paste(auxiliar,aux))
                                  }
                                  , silent = TRUE)
                              } else
                              {
                                url = NULL; contaminante_arana= NULL; WD_bruto =NULL;WD_col1 = NULL; WD = NULL
                                if(Contaminantes[p]=="WD")
                                {
                                  contaminante_arana<-"/Met/WDIR//"
                                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000_spec.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                  try(
                                    {
                                      WD_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                      WD_col1<-WD_bruto$X
                                      WD<-gsub("NA","",gsub(" ", "",WD_col1))
                                      data<-data.frame(data,WD)
                                      print(paste(auxiliar,aux))
                                    }
                                    , silent = TRUE)
                                } else
                                {
                                  url = NULL; contaminante_arana= NULL; WS_bruto =NULL;WS_col1 = NULL; WS = NULL
                                  if(Contaminantes[p]=="WS")
                                  {
                                    contaminante_arana<-"/Met/WSPD//"
                                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                    try(
                                      {
                                        WS_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                        WS_col1<-WS_bruto$X
                                        WS<-gsub("NA","",gsub(" ", "",WS_col1))
                                        data<-data.frame(data,WS)
                                        print(paste(auxiliar,aux))
                                      }
                                      , silent = TRUE)
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
                  }
                }
              }
            }
            data_Cerro_Navia<-data
            try(
              {
                data_total<-rbind(data_total, data_Cerro_Navia)
              }
              , silent = T)
          } else
          {
            auxiliar = NULL; data = NULL; p= NULL
            if(aux == "EB")
            {
              ciudad_arana<-"RM/D17"
              col_ciudad<-rep(aux,horas+1)
              data<-data.frame(col_fecha,col_ciudad)
              p= NULL; auxiliar = NULL
              for(p in 1:length(Contaminantes))
              {
                auxiliar<- Contaminantes[p]
                url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL; PM10 = NULL
                if(auxiliar=="PM10")
                {
                  contaminante_arana<-"/Cal/PM10//PM10"
                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                  try(
                    {
                      PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                      PM10_col1<-PM10_Bruto$Registros.validados
                      PM10_col2<-PM10_Bruto$Registros.preliminares
                      PM10_col3<-PM10_Bruto$Registros.no.validados
                      PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
                      data<-data.frame(data,PM10)
                      print(paste(auxiliar,aux))
                    }
                    ,silent = True)
                  
                } else
                {
                  url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL; PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL
                  if(Contaminantes[p]=="PM25")
                  {
                    contaminante_arana<-"/Cal/PM25//PM25"
                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                    try(
                      {
                        PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                        PM25_col1<-PM25_Bruto$Registros.validados
                        PM25_col2<-PM25_Bruto$Registros.preliminares
                        PM25_col3<-PM25_Bruto$Registros.no.validados
                        PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                        data<-data.frame(data,PM25)
                        print(paste(auxiliar,aux))
                      }
                      , silent = TRUE)
                  } else
                  {
                    url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL
                    if(Contaminantes[p]=="O3")
                    {
                      contaminante_arana<-"/Cal/0008//0008"
                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                      try(
                        {
                          O3_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                          O3_col1<-O3_Bruto$Registros.validados
                          O3_col2<-O3_Bruto$Registros.preliminares
                          O3_col3<-O3_Bruto$Registros.no.validados
                          O3<-gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                          data<-data.frame(data,O3)
                          print(paste(auxiliar,aux))
                        }
                        , silent = TRUE)
                    } else
                    {
                      url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL
                      if(Contaminantes[p]=="CO")
                      {
                        contaminante_arana<-"/Cal/0004//0004"
                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                        try(
                          {
                            CO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                            CO_col1<-CO_Bruto$Registros.validados
                            CO_col2<-CO_Bruto$Registros.preliminares
                            CO_col3<-CO_Bruto$Registros.no.validados
                            CO<-gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                            data<-data.frame(data,CO)
                            print(paste(auxiliar,aux))
                          }
                          , silent = TRUE)
                      } else
                      {
                        url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL
                        if(Contaminantes[p]=="NO")
                        {
                          contaminante_arana<-"/Cal/0002//0002"
                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                          try(
                            {
                              NO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                              NO_col1<-NO_Bruto$Registros.validados
                              NO_col2<-NO_Bruto$Registros.preliminares
                              NO_col3<-NO_Bruto$Registros.no.validados
                              NO<-gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                              data<-data.frame(data,NO)
                              print(paste(auxiliar,aux))
                            }
                            , silent = TRUE)
                        } else
                        {
                          url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL
                          if(Contaminantes[p]=="NO2")
                          {
                            contaminante_arana<-"/Cal/0003//0003"
                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                            try(
                              {
                                NO2_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                NO2_col1<-NO2_Bruto$Registros.validados
                                NO2_col2<-NO2_Bruto$Registros.preliminares
                                NO2_col3<-NO2_Bruto$Registros.no.validados
                                NO2<-gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                                data<-data.frame(data,NO2)
                                print(paste(auxiliar,aux))
                              }
                            )
                          } else
                          {
                            url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                            if(Contaminantes[p]=="NOX")
                            {
                              contaminante_arana<-"/Cal/0NOX//0NOX"
                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                              try(
                                {
                                  NOX_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                  NOX_col1<-NOX_Bruto$Registros.validados
                                  NOX_col2<-NOX_Bruto$Registros.preliminares
                                  NOX_col3<-NOX_Bruto$Registros.no.validados
                                  NOX<-gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                                  data<-data.frame(data,NOX)
                                  print(paste(auxiliar,aux))
                                }
                                , silent = TRUE)
                            } else
                            {
                              url = NULL; contaminante_arana= NULL; temperatura_bruto =NULL;temperatura_col1 = NULL; temperatura = NULL
                              if(Contaminantes[p]=="Temperatura")
                              {
                                contaminante_arana<-"/Met/TEMP//"
                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                try(
                                  {
                                    temperatura_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                    temperatura_col1<-temperatura_bruto$X
                                    Temperatura<-gsub("NA","",gsub(" ", "",temperatura_col1))
                                    data<-data.frame(data,Temperatura)
                                    print(paste(auxiliar,aux))
                                  }
                                  , silent = TRUE)
                              } else
                              {
                                url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                                if(Contaminantes[p]=="HR")
                                {
                                  contaminante_arana<-"/Met/RHUM//"
                                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                  try(
                                    {
                                      HR_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                      HR_col1<-HR_bruto$X
                                      HR<-gsub("NA","",gsub(" ", "",HR_col1))
                                      data<-data.frame(data,HR)
                                      print(paste(auxiliar,aux))
                                    }
                                    , silent = TRUE)
                                } else
                                {
                                  url = NULL; contaminante_arana= NULL; WD_bruto =NULL;WD_col1 = NULL; WD = NULL
                                  if(Contaminantes[p]=="WD")
                                  {
                                    contaminante_arana<-"/Met/WDIR//"
                                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000_spec.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                    try(
                                      {
                                        WD_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                        WD_col1<-WD_bruto$X
                                        WD<-gsub("NA","",gsub(" ", "",WD_col1))
                                        data<-data.frame(data,WD)
                                        print(paste(auxiliar,aux))
                                      }
                                      , silent = TRUE)
                                  } else
                                  {
                                    url = NULL; contaminante_arana= NULL; WS_bruto =NULL;WS_col1 = NULL; WS = NULL
                                    if(Contaminantes[p]=="WS")
                                    {
                                      contaminante_arana<-"/Met/WSPD//"
                                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                      try(
                                        {
                                          WS_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                          WS_col1<-WS_bruto$X
                                          WS<-gsub("NA","",gsub(" ", "",WS_col1))
                                          data<-data.frame(data,WS)
                                          print(paste(auxiliar,aux))
                                        }
                                        , silent = TRUE)
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
                    }
                  }
                }
              }
              data_El_Bosque<-data
              try(
                {
                  data_total<-rbind(data_total, data_El_Bosque)
                }
                , silent = T)
            } else
            {
              auxiliar = NULL; data = NULL; p= NULL
              if(aux == "IN")#Independencia
              {
                ciudad_arana<-"RM/D11"
                col_ciudad<-rep(aux,horas+1)
                data<-data.frame(col_fecha,col_ciudad)
                p= NULL; auxiliar = NULL
                for(p in 1:length(Contaminantes))
                {
                  auxiliar<- Contaminantes[p]
                  url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL; PM10 = NULL
                  if(auxiliar=="PM10")
                  {
                    contaminante_arana<-"/Cal/PM10//PM10"
                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                    try(
                      {
                        PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                        PM10_col1<-PM10_Bruto$Registros.validados
                        PM10_col2<-PM10_Bruto$Registros.preliminares
                        PM10_col3<-PM10_Bruto$Registros.no.validados
                        PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
                        data<-data.frame(data,PM10)
                        print(paste(auxiliar,aux))
                      }
                      ,silent = True)
                    
                  } else
                  {
                    url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL; PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL
                    if(Contaminantes[p]=="PM25")
                    {
                      contaminante_arana<-"/Cal/PM25//PM25"
                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                      try(
                        {
                          PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                          PM25_col1<-PM25_Bruto$Registros.validados
                          PM25_col2<-PM25_Bruto$Registros.preliminares
                          PM25_col3<-PM25_Bruto$Registros.no.validados
                          PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                          data<-data.frame(data,PM25)
                          print(paste(auxiliar,aux))
                        }
                        , silent = TRUE)
                    } else
                    {
                      url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL
                      if(Contaminantes[p]=="O3")
                      {
                        contaminante_arana<-"/Cal/0008//0008"
                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                        try(
                          {
                            O3_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                            O3_col1<-O3_Bruto$Registros.validados
                            O3_col2<-O3_Bruto$Registros.preliminares
                            O3_col3<-O3_Bruto$Registros.no.validados
                            O3<-gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                            data<-data.frame(data,O3)
                            print(paste(auxiliar,aux))
                          }
                          , silent = TRUE)
                      } else
                      {
                        url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL
                        if(Contaminantes[p]=="CO")
                        {
                          contaminante_arana<-"/Cal/0004//0004"
                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                          try(
                            {
                              CO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                              CO_col1<-CO_Bruto$Registros.validados
                              CO_col2<-CO_Bruto$Registros.preliminares
                              CO_col3<-CO_Bruto$Registros.no.validados
                              CO<-gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                              data<-data.frame(data,CO)
                              print(paste(auxiliar,aux))
                            }
                            , silent = TRUE)
                        } else
                        {
                          url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL
                          if(Contaminantes[p]=="NO")
                          {
                            contaminante_arana<-"/Cal/0002//0002"
                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                            try(
                              {
                                NO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                NO_col1<-NO_Bruto$Registros.validados
                                NO_col2<-NO_Bruto$Registros.preliminares
                                NO_col3<-NO_Bruto$Registros.no.validados
                                NO<-gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                                data<-data.frame(data,NO)
                                print(paste(auxiliar,aux))
                              }
                              , silent = TRUE)
                          } else
                          {
                            url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL
                            if(Contaminantes[p]=="NO2")
                            {
                              contaminante_arana<-"/Cal/0003//0003"
                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                              try(
                                {
                                  NO2_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                  NO2_col1<-NO2_Bruto$Registros.validados
                                  NO2_col2<-NO2_Bruto$Registros.preliminares
                                  NO2_col3<-NO2_Bruto$Registros.no.validados
                                  NO2<-gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                                  data<-data.frame(data,NO2)
                                  print(paste(auxiliar,aux))
                                }
                              )
                            } else
                            {
                              url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                              if(Contaminantes[p]=="NOX")
                              {
                                contaminante_arana<-"/Cal/0NOX//0NOX"
                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                try(
                                  {
                                    NOX_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                    NOX_col1<-NOX_Bruto$Registros.validados
                                    NOX_col2<-NOX_Bruto$Registros.preliminares
                                    NOX_col3<-NOX_Bruto$Registros.no.validados
                                    NOX<-gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                                    data<-data.frame(data,NOX)
                                    print(paste(auxiliar,aux))
                                  }
                                  , silent = TRUE)
                              } else
                              {
                                url = NULL; contaminante_arana= NULL; temperatura_bruto =NULL;temperatura_col1 = NULL; temperatura = NULL
                                if(Contaminantes[p]=="Temperatura")
                                {
                                  contaminante_arana<-"/Met/TEMP//"
                                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                  try(
                                    {
                                      temperatura_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                      temperatura_col1<-temperatura_bruto$X
                                      Temperatura<-gsub("NA","",gsub(" ", "",temperatura_col1))
                                      data<-data.frame(data,Temperatura)
                                      print(paste(auxiliar,aux))
                                    }
                                    , silent = TRUE)
                                } else
                                {
                                  url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                                  if(Contaminantes[p]=="HR")
                                  {
                                    contaminante_arana<-"/Met/RHUM//"
                                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                    try(
                                      {
                                        HR_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                        HR_col1<-HR_bruto$X
                                        HR<-gsub("NA","",gsub(" ", "",HR_col1))
                                        data<-data.frame(data,HR)
                                        print(paste(auxiliar,aux))
                                      }
                                      , silent = TRUE)
                                  } else
                                  {
                                    url = NULL; contaminante_arana= NULL; WD_bruto =NULL;WD_col1 = NULL; WD = NULL
                                    if(Contaminantes[p]=="WD")
                                    {
                                      contaminante_arana<-"/Met/WDIR//"
                                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000_spec.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                      try(
                                        {
                                          WD_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                          WD_col1<-WD_bruto$X
                                          WD<-gsub("NA","",gsub(" ", "",WD_col1))
                                          data<-data.frame(data,WD)
                                          print(paste(auxiliar,aux))
                                        }
                                        , silent = TRUE)
                                    } else
                                    {
                                      url = NULL; contaminante_arana= NULL; WS_bruto =NULL;WS_col1 = NULL; WS = NULL
                                      if(Contaminantes[p]=="WS")
                                      {
                                        contaminante_arana<-"/Met/WSPD//"
                                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                        try(
                                          {
                                            WS_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                            WS_col1<-WS_bruto$X
                                            WS<-gsub("NA","",gsub(" ", "",WS_col1))
                                            data<-data.frame(data,WS)
                                            print(paste(auxiliar,aux))
                                          }
                                          , silent = TRUE)
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
                      }
                    }
                  }
                }
                data_Independencia<-data
                try(
                  {
                    data_total<-rbind(data_total, data_Independencia)
                  }
                  , silent = T)
              } else
              {
                auxiliar = NULL; data = NULL; p= NULL
                if(aux == "LF")
                {
                  ciudad_arana<-"RM/D12"
                  col_ciudad<-rep(aux,horas+1)
                  data<-data.frame(col_fecha,col_ciudad)
                  p= NULL; auxiliar = NULL
                  for(p in 1:length(Contaminantes))
                  {
                    auxiliar<- Contaminantes[p]
                    url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL; PM10 = NULL
                    if(auxiliar=="PM10")
                    {
                      contaminante_arana<-"/Cal/PM10//PM10"
                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                      try(
                        {
                          PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                          PM10_col1<-PM10_Bruto$Registros.validados
                          PM10_col2<-PM10_Bruto$Registros.preliminares
                          PM10_col3<-PM10_Bruto$Registros.no.validados
                          PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
                          data<-data.frame(data,PM10)
                          print(paste(auxiliar,aux))
                        }
                        ,silent = True)
                      
                    } else
                    {
                      url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL; PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL
                      if(Contaminantes[p]=="PM25")
                      {
                        contaminante_arana<-"/Cal/PM25//PM25"
                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                        try(
                          {
                            PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                            PM25_col1<-PM25_Bruto$Registros.validados
                            PM25_col2<-PM25_Bruto$Registros.preliminares
                            PM25_col3<-PM25_Bruto$Registros.no.validados
                            PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                            data<-data.frame(data,PM25)
                            print(paste(auxiliar,aux))
                          }
                          , silent = TRUE)
                      } else
                      {
                        url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL
                        if(Contaminantes[p]=="O3")
                        {
                          contaminante_arana<-"/Cal/0008//0008"
                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                          try(
                            {
                              O3_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                              O3_col1<-O3_Bruto$Registros.validados
                              O3_col2<-O3_Bruto$Registros.preliminares
                              O3_col3<-O3_Bruto$Registros.no.validados
                              O3<-gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                              data<-data.frame(data,O3)
                              print(paste(auxiliar,aux))
                            }
                            , silent = TRUE)
                        } else
                        {
                          url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL
                          if(Contaminantes[p]=="CO")
                          {
                            contaminante_arana<-"/Cal/0004//0004"
                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                            try(
                              {
                                CO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                CO_col1<-CO_Bruto$Registros.validados
                                CO_col2<-CO_Bruto$Registros.preliminares
                                CO_col3<-CO_Bruto$Registros.no.validados
                                CO<-gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                                data<-data.frame(data,CO)
                                print(paste(auxiliar,aux))
                              }
                              , silent = TRUE)
                          } else
                          {
                            url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL
                            if(Contaminantes[p]=="NO")
                            {
                              contaminante_arana<-"/Cal/0002//0002"
                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                              try(
                                {
                                  NO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                  NO_col1<-NO_Bruto$Registros.validados
                                  NO_col2<-NO_Bruto$Registros.preliminares
                                  NO_col3<-NO_Bruto$Registros.no.validados
                                  NO<-gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                                  data<-data.frame(data,NO)
                                  print(paste(auxiliar,aux))
                                }
                                , silent = TRUE)
                            } else
                            {
                              url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL
                              if(Contaminantes[p]=="NO2")
                              {
                                contaminante_arana<-"/Cal/0003//0003"
                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                try(
                                  {
                                    NO2_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                    NO2_col1<-NO2_Bruto$Registros.validados
                                    NO2_col2<-NO2_Bruto$Registros.preliminares
                                    NO2_col3<-NO2_Bruto$Registros.no.validados
                                    NO2<-gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                                    data<-data.frame(data,NO2)
                                    print(paste(auxiliar,aux))
                                  }
                                )
                              } else
                              {
                                url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                                if(Contaminantes[p]=="NOX")
                                {
                                  contaminante_arana<-"/Cal/0NOX//0NOX"
                                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                  try(
                                    {
                                      NOX_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                      NOX_col1<-NOX_Bruto$Registros.validados
                                      NOX_col2<-NOX_Bruto$Registros.preliminares
                                      NOX_col3<-NOX_Bruto$Registros.no.validados
                                      NOX<-gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                                      data<-data.frame(data,NOX)
                                      print(paste(auxiliar,aux))
                                    }
                                    , silent = TRUE)
                                } else
                                {
                                  url = NULL; contaminante_arana= NULL; temperatura_bruto =NULL;temperatura_col1 = NULL; temperatura = NULL
                                  if(Contaminantes[p]=="Temperatura")
                                  {
                                    contaminante_arana<-"/Met/TEMP//"
                                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                    try(
                                      {
                                        temperatura_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                        temperatura_col1<-temperatura_bruto$X
                                        Temperatura<-gsub("NA","",gsub(" ", "",temperatura_col1))
                                        data<-data.frame(data,Temperatura)
                                        print(paste(auxiliar,aux))
                                      }
                                      , silent = TRUE)
                                  } else
                                  {
                                    url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                                    if(Contaminantes[p]=="HR")
                                    {
                                      contaminante_arana<-"/Met/RHUM//"
                                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                      try(
                                        {
                                          HR_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                          HR_col1<-HR_bruto$X
                                          HR<-gsub("NA","",gsub(" ", "",HR_col1))
                                          data<-data.frame(data,HR)
                                          print(paste(auxiliar,aux))
                                        }
                                        , silent = TRUE)
                                    } else
                                    {
                                      url = NULL; contaminante_arana= NULL; WD_bruto =NULL;WD_col1 = NULL; WD = NULL
                                      if(Contaminantes[p]=="WD")
                                      {
                                        contaminante_arana<-"/Met/WDIR//"
                                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000_spec.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                        try(
                                          {
                                            WD_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                            WD_col1<-WD_bruto$X
                                            WD<-gsub("NA","",gsub(" ", "",WD_col1))
                                            data<-data.frame(data,WD)
                                            print(paste(auxiliar,aux))
                                          }
                                          , silent = TRUE)
                                      } else
                                      {
                                        url = NULL; contaminante_arana= NULL; WS_bruto =NULL;WS_col1 = NULL; WS = NULL
                                        if(Contaminantes[p]=="WS")
                                        {
                                          contaminante_arana<-"/Met/WSPD//"
                                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                          try(
                                            {
                                              WS_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                              WS_col1<-WS_bruto$X
                                              WS<-gsub("NA","",gsub(" ", "",WS_col1))
                                              data<-data.frame(data,WS)
                                              print(paste(auxiliar,aux))
                                            }
                                            , silent = TRUE)
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
                        }
                      }
                    }
                  }
                  data_La_Florida<-data
                  try(
                    {
                      data_total<-rbind(data_total, data_La_Florida)
                    }
                    , silent = T)
                } else
                {
                  auxiliar = NULL; data = NULL; p= NULL
                  if(aux == "LC")
                  {
                    ciudad_arana<-"RM/D13"
                    col_ciudad<-rep(aux,horas+1)
                    data<-data.frame(col_fecha,col_ciudad)
                    p= NULL; auxiliar = NULL
                    for(p in 1:length(Contaminantes))
                    {
                      auxiliar<- Contaminantes[p]
                      url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL; PM10 = NULL
                      if(auxiliar=="PM10")
                      {
                        contaminante_arana<-"/Cal/PM10//PM10"
                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                        try(
                          {
                            PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                            PM10_col1<-PM10_Bruto$Registros.validados
                            PM10_col2<-PM10_Bruto$Registros.preliminares
                            PM10_col3<-PM10_Bruto$Registros.no.validados
                            PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
                            data<-data.frame(data,PM10)
                            print(paste(auxiliar,aux))
                          }
                          ,silent = True)
                        
                      } else
                      {
                        url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL; PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL
                        if(Contaminantes[p]=="PM25")
                        {
                          contaminante_arana<-"/Cal/PM25//PM25"
                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                          try(
                            {
                              PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                              PM25_col1<-PM25_Bruto$Registros.validados
                              PM25_col2<-PM25_Bruto$Registros.preliminares
                              PM25_col3<-PM25_Bruto$Registros.no.validados
                              PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                              data<-data.frame(data,PM25)
                              print(paste(auxiliar,aux))
                            }
                            , silent = TRUE)
                        } else
                        {
                          url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL
                          if(Contaminantes[p]=="O3")
                          {
                            contaminante_arana<-"/Cal/0008//0008"
                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                            try(
                              {
                                O3_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                O3_col1<-O3_Bruto$Registros.validados
                                O3_col2<-O3_Bruto$Registros.preliminares
                                O3_col3<-O3_Bruto$Registros.no.validados
                                O3<-gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                                data<-data.frame(data,O3)
                                print(paste(auxiliar,aux))
                              }
                              , silent = TRUE)
                          } else
                          {
                            url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL
                            if(Contaminantes[p]=="CO")
                            {
                              contaminante_arana<-"/Cal/0004//0004"
                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                              try(
                                {
                                  CO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                  CO_col1<-CO_Bruto$Registros.validados
                                  CO_col2<-CO_Bruto$Registros.preliminares
                                  CO_col3<-CO_Bruto$Registros.no.validados
                                  CO<-gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                                  data<-data.frame(data,CO)
                                  print(paste(auxiliar,aux))
                                }
                                , silent = TRUE)
                            } else
                            {
                              url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL
                              if(Contaminantes[p]=="NO")
                              {
                                contaminante_arana<-"/Cal/0002//0002"
                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                try(
                                  {
                                    NO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                    NO_col1<-NO_Bruto$Registros.validados
                                    NO_col2<-NO_Bruto$Registros.preliminares
                                    NO_col3<-NO_Bruto$Registros.no.validados
                                    NO<-gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                                    data<-data.frame(data,NO)
                                    print(paste(auxiliar,aux))
                                  }
                                  , silent = TRUE)
                              } else
                              {
                                url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL
                                if(Contaminantes[p]=="NO2")
                                {
                                  contaminante_arana<-"/Cal/0003//0003"
                                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                  try(
                                    {
                                      NO2_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                      NO2_col1<-NO2_Bruto$Registros.validados
                                      NO2_col2<-NO2_Bruto$Registros.preliminares
                                      NO2_col3<-NO2_Bruto$Registros.no.validados
                                      NO2<-gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                                      data<-data.frame(data,NO2)
                                      print(paste(auxiliar,aux))
                                    }
                                  )
                                } else
                                {
                                  url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                                  if(Contaminantes[p]=="NOX")
                                  {
                                    contaminante_arana<-"/Cal/0NOX//0NOX"
                                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                    try(
                                      {
                                        NOX_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                        NOX_col1<-NOX_Bruto$Registros.validados
                                        NOX_col2<-NOX_Bruto$Registros.preliminares
                                        NOX_col3<-NOX_Bruto$Registros.no.validados
                                        NOX<-gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                                        data<-data.frame(data,NOX)
                                        print(paste(auxiliar,aux))
                                      }
                                      , silent = TRUE)
                                  } else
                                  {
                                    url = NULL; contaminante_arana= NULL; temperatura_bruto =NULL;temperatura_col1 = NULL; temperatura = NULL
                                    if(Contaminantes[p]=="Temperatura")
                                    {
                                      contaminante_arana<-"/Met/TEMP//"
                                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                      try(
                                        {
                                          temperatura_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                          temperatura_col1<-temperatura_bruto$X
                                          Temperatura<-gsub("NA","",gsub(" ", "",temperatura_col1))
                                          data<-data.frame(data,Temperatura)
                                          print(paste(auxiliar,aux))
                                        }
                                        , silent = TRUE)
                                    } else
                                    {
                                      url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                                      if(Contaminantes[p]=="HR")
                                      {
                                        contaminante_arana<-"/Met/RHUM//"
                                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                        try(
                                          {
                                            HR_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                            HR_col1<-HR_bruto$X
                                            HR<-gsub("NA","",gsub(" ", "",HR_col1))
                                            data<-data.frame(data,HR)
                                            print(paste(auxiliar,aux))
                                          }
                                          , silent = TRUE)
                                      } else
                                      {
                                        url = NULL; contaminante_arana= NULL; WD_bruto =NULL;WD_col1 = NULL; WD = NULL
                                        if(Contaminantes[p]=="WD")
                                        {
                                          contaminante_arana<-"/Met/WDIR//"
                                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000_spec.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                          try(
                                            {
                                              WD_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                              WD_col1<-WD_bruto$X
                                              WD<-gsub("NA","",gsub(" ", "",WD_col1))
                                              data<-data.frame(data,WD)
                                              print(paste(auxiliar,aux))
                                            }
                                            , silent = TRUE)
                                        } else
                                        {
                                          url = NULL; contaminante_arana= NULL; WS_bruto =NULL;WS_col1 = NULL; WS = NULL
                                          if(Contaminantes[p]=="WS")
                                          {
                                            contaminante_arana<-"/Met/WSPD//"
                                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                            try(
                                              {
                                                WS_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                WS_col1<-WS_bruto$X
                                                WS<-gsub("NA","",gsub(" ", "",WS_col1))
                                                data<-data.frame(data,WS)
                                                print(paste(auxiliar,aux))
                                              }
                                              , silent = TRUE)
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
                          }
                        }
                      }
                    }
                    data_Las_Condes<-data
                    try(
                      {
                        data_total<-rbind(data_total, data_Las_Condes)
                      }
                      , silent = T)
                  } else
                  {
                    auxiliar = NULL; data = NULL; p= NULL
                    if(aux == "PU")
                    {
                      ciudad_arana<-"RM/D15"
                      col_ciudad<-rep(aux,horas+1)
                      data<-data.frame(col_fecha,col_ciudad)
                      p= NULL; auxiliar = NULL
                      for(p in 1:length(Contaminantes))
                      {
                        auxiliar<- Contaminantes[p]
                        url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL; PM10 = NULL
                        if(auxiliar=="PM10")
                        {
                          contaminante_arana<-"/Cal/PM10//PM10"
                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                          try(
                            {
                              PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                              PM10_col1<-PM10_Bruto$Registros.validados
                              PM10_col2<-PM10_Bruto$Registros.preliminares
                              PM10_col3<-PM10_Bruto$Registros.no.validados
                              PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
                              data<-data.frame(data,PM10)
                              print(paste(auxiliar,aux))
                            }
                            ,silent = True)
                          
                        } else
                        {
                          url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL; PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL
                          if(Contaminantes[p]=="PM25")
                          {
                            contaminante_arana<-"/Cal/PM25//PM25"
                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                            try(
                              {
                                PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                PM25_col1<-PM25_Bruto$Registros.validados
                                PM25_col2<-PM25_Bruto$Registros.preliminares
                                PM25_col3<-PM25_Bruto$Registros.no.validados
                                PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                                data<-data.frame(data,PM25)
                                print(paste(auxiliar,aux))
                              }
                              , silent = TRUE)
                          } else
                          {
                            url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL
                            if(Contaminantes[p]=="O3")
                            {
                              contaminante_arana<-"/Cal/0008//0008"
                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                              try(
                                {
                                  O3_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                  O3_col1<-O3_Bruto$Registros.validados
                                  O3_col2<-O3_Bruto$Registros.preliminares
                                  O3_col3<-O3_Bruto$Registros.no.validados
                                  O3<-gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                                  data<-data.frame(data,O3)
                                  print(paste(auxiliar,aux))
                                }
                                , silent = TRUE)
                            } else
                            {
                              url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL
                              if(Contaminantes[p]=="CO")
                              {
                                contaminante_arana<-"/Cal/0004//0004"
                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                try(
                                  {
                                    CO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                    CO_col1<-CO_Bruto$Registros.validados
                                    CO_col2<-CO_Bruto$Registros.preliminares
                                    CO_col3<-CO_Bruto$Registros.no.validados
                                    CO<-gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                                    data<-data.frame(data,CO)
                                    print(paste(auxiliar,aux))
                                  }
                                  , silent = TRUE)
                              } else
                              {
                                url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL
                                if(Contaminantes[p]=="NO")
                                {
                                  contaminante_arana<-"/Cal/0002//0002"
                                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                  try(
                                    {
                                      NO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                      NO_col1<-NO_Bruto$Registros.validados
                                      NO_col2<-NO_Bruto$Registros.preliminares
                                      NO_col3<-NO_Bruto$Registros.no.validados
                                      NO<-gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                                      data<-data.frame(data,NO)
                                      print(paste(auxiliar,aux))
                                    }
                                    , silent = TRUE)
                                } else
                                {
                                  url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL
                                  if(Contaminantes[p]=="NO2")
                                  {
                                    contaminante_arana<-"/Cal/0003//0003"
                                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                    try(
                                      {
                                        NO2_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                        NO2_col1<-NO2_Bruto$Registros.validados
                                        NO2_col2<-NO2_Bruto$Registros.preliminares
                                        NO2_col3<-NO2_Bruto$Registros.no.validados
                                        NO2<-gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                                        data<-data.frame(data,NO2)
                                        print(paste(auxiliar,aux))
                                      }
                                    )
                                  } else
                                  {
                                    url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                                    if(Contaminantes[p]=="NOX")
                                    {
                                      contaminante_arana<-"/Cal/0NOX//0NOX"
                                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                      try(
                                        {
                                          NOX_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                          NOX_col1<-NOX_Bruto$Registros.validados
                                          NOX_col2<-NOX_Bruto$Registros.preliminares
                                          NOX_col3<-NOX_Bruto$Registros.no.validados
                                          NOX<-gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                                          data<-data.frame(data,NOX)
                                          print(paste(auxiliar,aux))
                                        }
                                        , silent = TRUE)
                                    } else
                                    {
                                      url = NULL; contaminante_arana= NULL; temperatura_bruto =NULL;temperatura_col1 = NULL; temperatura = NULL
                                      if(Contaminantes[p]=="Temperatura")
                                      {
                                        contaminante_arana<-"/Met/TEMP//"
                                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                        try(
                                          {
                                            temperatura_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                            temperatura_col1<-temperatura_bruto$X
                                            Temperatura<-gsub("NA","",gsub(" ", "",temperatura_col1))
                                            data<-data.frame(data,Temperatura)
                                            print(paste(auxiliar,aux))
                                          }
                                          , silent = TRUE)
                                      } else
                                      {
                                        url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                                        if(Contaminantes[p]=="HR")
                                        {
                                          contaminante_arana<-"/Met/RHUM//"
                                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                          try(
                                            {
                                              HR_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                              HR_col1<-HR_bruto$X
                                              HR<-gsub("NA","",gsub(" ", "",HR_col1))
                                              data<-data.frame(data,HR)
                                              print(paste(auxiliar,aux))
                                            }
                                            , silent = TRUE)
                                        } else
                                        {
                                          url = NULL; contaminante_arana= NULL; WD_bruto =NULL;WD_col1 = NULL; WD = NULL
                                          if(Contaminantes[p]=="WD")
                                          {
                                            contaminante_arana<-"/Met/WDIR//"
                                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000_spec.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                            try(
                                              {
                                                WD_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                WD_col1<-WD_bruto$X
                                                WD<-gsub("NA","",gsub(" ", "",WD_col1))
                                                data<-data.frame(data,WD)
                                                print(paste(auxiliar,aux))
                                              }
                                              , silent = TRUE)
                                          } else
                                          {
                                            url = NULL; contaminante_arana= NULL; WS_bruto =NULL;WS_col1 = NULL; WS = NULL
                                            if(Contaminantes[p]=="WS")
                                            {
                                              contaminante_arana<-"/Met/WSPD//"
                                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                              try(
                                                {
                                                  WS_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                  WS_col1<-WS_bruto$X
                                                  WS<-gsub("NA","",gsub(" ", "",WS_col1))
                                                  data<-data.frame(data,WS)
                                                  print(paste(auxiliar,aux))
                                                }
                                                , silent = TRUE)
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
                            }
                          }
                        }
                      }
                      data_Pudahuel<-data
                      try(
                        {
                          data_total<-rbind(data_total, data_Pudahuel)
                        }
                        , silent = T)
                    } else
                    {
                      auxiliar = NULL; data = NULL; p= NULL
                      if(aux == "PA")
                      {
                        ciudad_arana<-"RM/D27"
                        col_ciudad<-rep(aux,horas+1)
                        data<-data.frame(col_fecha,col_ciudad)
                        p= NULL; auxiliar = NULL
                        for(p in 1:length(Contaminantes))
                        {
                          auxiliar<- Contaminantes[p]
                          url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL; PM10 = NULL
                          if(auxiliar=="PM10")
                          {
                            contaminante_arana<-"/Cal/PM10//PM10"
                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                            try(
                              {
                                PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                PM10_col1<-PM10_Bruto$Registros.validados
                                PM10_col2<-PM10_Bruto$Registros.preliminares
                                PM10_col3<-PM10_Bruto$Registros.no.validados
                                PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
                                data<-data.frame(data,PM10)
                                print(paste(auxiliar,aux))
                              }
                              ,silent = True)
                            
                          } else
                          {
                            url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL; PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL
                            if(Contaminantes[p]=="PM25")
                            {
                              contaminante_arana<-"/Cal/PM25//PM25"
                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                              try(
                                {
                                  PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                  PM25_col1<-PM25_Bruto$Registros.validados
                                  PM25_col2<-PM25_Bruto$Registros.preliminares
                                  PM25_col3<-PM25_Bruto$Registros.no.validados
                                  PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                                  data<-data.frame(data,PM25)
                                  print(paste(auxiliar,aux))
                                }
                                , silent = TRUE)
                            } else
                            {
                              url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL
                              if(Contaminantes[p]=="O3")
                              {
                                contaminante_arana<-"/Cal/0008//0008"
                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                try(
                                  {
                                    O3_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                    O3_col1<-O3_Bruto$Registros.validados
                                    O3_col2<-O3_Bruto$Registros.preliminares
                                    O3_col3<-O3_Bruto$Registros.no.validados
                                    O3<-gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                                    data<-data.frame(data,O3)
                                    print(paste(auxiliar,aux))
                                  }
                                  , silent = TRUE)
                              } else
                              {
                                url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL
                                if(Contaminantes[p]=="CO")
                                {
                                  contaminante_arana<-"/Cal/0004//0004"
                                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                  try(
                                    {
                                      CO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                      CO_col1<-CO_Bruto$Registros.validados
                                      CO_col2<-CO_Bruto$Registros.preliminares
                                      CO_col3<-CO_Bruto$Registros.no.validados
                                      CO<-gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                                      data<-data.frame(data,CO)
                                      print(paste(auxiliar,aux))
                                    }
                                    , silent = TRUE)
                                } else
                                {
                                  url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL
                                  if(Contaminantes[p]=="NO")
                                  {
                                    contaminante_arana<-"/Cal/0002//0002"
                                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                    try(
                                      {
                                        NO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                        NO_col1<-NO_Bruto$Registros.validados
                                        NO_col2<-NO_Bruto$Registros.preliminares
                                        NO_col3<-NO_Bruto$Registros.no.validados
                                        NO<-gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                                        data<-data.frame(data,NO)
                                        print(paste(auxiliar,aux))
                                      }
                                      , silent = TRUE)
                                  } else
                                  {
                                    url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL
                                    if(Contaminantes[p]=="NO2")
                                    {
                                      contaminante_arana<-"/Cal/0003//0003"
                                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                      try(
                                        {
                                          NO2_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                          NO2_col1<-NO2_Bruto$Registros.validados
                                          NO2_col2<-NO2_Bruto$Registros.preliminares
                                          NO2_col3<-NO2_Bruto$Registros.no.validados
                                          NO2<-gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                                          data<-data.frame(data,NO2)
                                          print(paste(auxiliar,aux))
                                        }
                                      )
                                    } else
                                    {
                                      url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                                      if(Contaminantes[p]=="NOX")
                                      {
                                        contaminante_arana<-"/Cal/0NOX//0NOX"
                                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                        try(
                                          {
                                            NOX_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                            NOX_col1<-NOX_Bruto$Registros.validados
                                            NOX_col2<-NOX_Bruto$Registros.preliminares
                                            NOX_col3<-NOX_Bruto$Registros.no.validados
                                            NOX<-gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                                            data<-data.frame(data,NOX)
                                            print(paste(auxiliar,aux))
                                          }
                                          , silent = TRUE)
                                      } else
                                      {
                                        url = NULL; contaminante_arana= NULL; temperatura_bruto =NULL;temperatura_col1 = NULL; temperatura = NULL
                                        if(Contaminantes[p]=="Temperatura")
                                        {
                                          contaminante_arana<-"/Met/TEMP//"
                                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                          try(
                                            {
                                              temperatura_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                              temperatura_col1<-temperatura_bruto$X
                                              Temperatura<-gsub("NA","",gsub(" ", "",temperatura_col1))
                                              data<-data.frame(data,Temperatura)
                                              print(paste(auxiliar,aux))
                                            }
                                            , silent = TRUE)
                                        } else
                                        {
                                          url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                                          if(Contaminantes[p]=="HR")
                                          {
                                            contaminante_arana<-"/Met/RHUM//"
                                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                            try(
                                              {
                                                HR_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                HR_col1<-HR_bruto$X
                                                HR<-gsub("NA","",gsub(" ", "",HR_col1))
                                                data<-data.frame(data,HR)
                                                print(paste(auxiliar,aux))
                                              }
                                              , silent = TRUE)
                                          } else
                                          {
                                            url = NULL; contaminante_arana= NULL; WD_bruto =NULL;WD_col1 = NULL; WD = NULL
                                            if(Contaminantes[p]=="WD")
                                            {
                                              contaminante_arana<-"/Met/WDIR//"
                                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000_spec.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                              try(
                                                {
                                                  WD_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                  WD_col1<-WD_bruto$X
                                                  WD<-gsub("NA","",gsub(" ", "",WD_col1))
                                                  data<-data.frame(data,WD)
                                                  print(paste(auxiliar,aux))
                                                }
                                                , silent = TRUE)
                                            } else
                                            {
                                              url = NULL; contaminante_arana= NULL; WS_bruto =NULL;WS_col1 = NULL; WS = NULL
                                              if(Contaminantes[p]=="WS")
                                              {
                                                contaminante_arana<-"/Met/WSPD//"
                                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                                try(
                                                  {
                                                    WS_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                    WS_col1<-WS_bruto$X
                                                    WS<-gsub("NA","",gsub(" ", "",WS_col1))
                                                    data<-data.frame(data,WS)
                                                    print(paste(auxiliar,aux))
                                                  }
                                                  , silent = TRUE)
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
                              }
                            }
                          }
                        }
                        data_Puente_Alto<-data
                        try(
                          {
                            data_total<-rbind(data_total, data_Puente_Alto)
                          }
                          , silent = T)
                      } else
                      {
                        auxiliar = NULL;  data = NULL;  p= NULL
                        if(aux == "QU")
                        {
                          ciudad_arana<-"RM/D30"
                          col_ciudad<-rep(aux,horas+1)
                          data<-data.frame(col_fecha,col_ciudad)
                          p= NULL; auxiliar = NULL
                          for(p in 1:length(Contaminantes))
                          {
                            auxiliar<- Contaminantes[p]
                            url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL; PM10 = NULL
                            if(auxiliar=="PM10")
                            {
                              contaminante_arana<-"/Cal/PM10//PM10"
                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                              try(
                                {
                                  PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                  PM10_col1<-PM10_Bruto$Registros.validados
                                  PM10_col2<-PM10_Bruto$Registros.preliminares
                                  PM10_col3<-PM10_Bruto$Registros.no.validados
                                  PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
                                  data<-data.frame(data,PM10)
                                  print(paste(auxiliar,aux))
                                }
                                ,silent = True)
                              
                            } else
                            {
                              url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL; PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL
                              if(Contaminantes[p]=="PM25")
                              {
                                contaminante_arana<-"/Cal/PM25//PM25"
                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                try(
                                  {
                                    PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                    PM25_col1<-PM25_Bruto$Registros.validados
                                    PM25_col2<-PM25_Bruto$Registros.preliminares
                                    PM25_col3<-PM25_Bruto$Registros.no.validados
                                    PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                                    data<-data.frame(data,PM25)
                                    print(paste(auxiliar,aux))
                                  }
                                  , silent = TRUE)
                              } else
                              {
                                url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL
                                if(Contaminantes[p]=="O3")
                                {
                                  contaminante_arana<-"/Cal/0008//0008"
                                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                  try(
                                    {
                                      O3_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                      O3_col1<-O3_Bruto$Registros.validados
                                      O3_col2<-O3_Bruto$Registros.preliminares
                                      O3_col3<-O3_Bruto$Registros.no.validados
                                      O3<-gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                                      data<-data.frame(data,O3)
                                      print(paste(auxiliar,aux))
                                    }
                                    , silent = TRUE)
                                } else
                                {
                                  url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL
                                  if(Contaminantes[p]=="CO")
                                  {
                                    contaminante_arana<-"/Cal/0004//0004"
                                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                    try(
                                      {
                                        CO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                        CO_col1<-CO_Bruto$Registros.validados
                                        CO_col2<-CO_Bruto$Registros.preliminares
                                        CO_col3<-CO_Bruto$Registros.no.validados
                                        CO<-gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                                        data<-data.frame(data,CO)
                                        print(paste(auxiliar,aux))
                                      }
                                      , silent = TRUE)
                                  } else
                                  {
                                    url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL
                                    if(Contaminantes[p]=="NO")
                                    {
                                      contaminante_arana<-"/Cal/0002//0002"
                                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                      try(
                                        {
                                          NO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                          NO_col1<-NO_Bruto$Registros.validados
                                          NO_col2<-NO_Bruto$Registros.preliminares
                                          NO_col3<-NO_Bruto$Registros.no.validados
                                          NO<-gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                                          data<-data.frame(data,NO)
                                          print(paste(auxiliar,aux))
                                        }
                                        , silent = TRUE)
                                    } else
                                    {
                                      url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL
                                      if(Contaminantes[p]=="NO2")
                                      {
                                        contaminante_arana<-"/Cal/0003//0003"
                                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                        try(
                                          {
                                            NO2_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                            NO2_col1<-NO2_Bruto$Registros.validados
                                            NO2_col2<-NO2_Bruto$Registros.preliminares
                                            NO2_col3<-NO2_Bruto$Registros.no.validados
                                            NO2<-gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                                            data<-data.frame(data,NO2)
                                            print(paste(auxiliar,aux))
                                          }
                                        )
                                      } else
                                      {
                                        url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                                        if(Contaminantes[p]=="NOX")
                                        {
                                          contaminante_arana<-"/Cal/0NOX//0NOX"
                                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                          try(
                                            {
                                              NOX_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                              NOX_col1<-NOX_Bruto$Registros.validados
                                              NOX_col2<-NOX_Bruto$Registros.preliminares
                                              NOX_col3<-NOX_Bruto$Registros.no.validados
                                              NOX<-gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                                              data<-data.frame(data,NOX)
                                              print(paste(auxiliar,aux))
                                            }
                                            , silent = TRUE)
                                        } else
                                        {
                                          url = NULL; contaminante_arana= NULL; temperatura_bruto =NULL;temperatura_col1 = NULL; temperatura = NULL
                                          if(Contaminantes[p]=="Temperatura")
                                          {
                                            contaminante_arana<-"/Met/TEMP//"
                                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                            try(
                                              {
                                                temperatura_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                temperatura_col1<-temperatura_bruto$X
                                                Temperatura<-gsub("NA","",gsub(" ", "",temperatura_col1))
                                                data<-data.frame(data,Temperatura)
                                                print(paste(auxiliar,aux))
                                              }
                                              , silent = TRUE)
                                          } else
                                          {
                                            url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                                            if(Contaminantes[p]=="HR")
                                            {
                                              contaminante_arana<-"/Met/RHUM//"
                                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                              try(
                                                {
                                                  HR_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                  HR_col1<-HR_bruto$X
                                                  HR<-gsub("NA","",gsub(" ", "",HR_col1))
                                                  data<-data.frame(data,HR)
                                                  print(paste(auxiliar,aux))
                                                }
                                                , silent = TRUE)
                                            } else
                                            {
                                              url = NULL; contaminante_arana= NULL; WD_bruto =NULL;WD_col1 = NULL; WD = NULL
                                              if(Contaminantes[p]=="WD")
                                              {
                                                contaminante_arana<-"/Met/WDIR//"
                                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000_spec.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                                try(
                                                  {
                                                    WD_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                    WD_col1<-WD_bruto$X
                                                    WD<-gsub("NA","",gsub(" ", "",WD_col1))
                                                    data<-data.frame(data,WD)
                                                    print(paste(auxiliar,aux))
                                                  }
                                                  , silent = TRUE)
                                              } else
                                              {
                                                url = NULL; contaminante_arana= NULL; WS_bruto =NULL;WS_col1 = NULL; WS = NULL
                                                if(Contaminantes[p]=="WS")
                                                {
                                                  contaminante_arana<-"/Met/WSPD//"
                                                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                                  try(
                                                    {
                                                      WS_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                      WS_col1<-WS_bruto$X
                                                      WS<-gsub("NA","",gsub(" ", "",WS_col1))
                                                      data<-data.frame(data,WS)
                                                      print(paste(auxiliar,aux))
                                                    }
                                                    , silent = TRUE)
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
                                }
                              }
                            }
                          }
                          data_Quilicura<-data
                          try(
                            {
                              data_total<-rbind(data_total, data_Quilicura)
                            }
                            , silent = T)
                        } else
                        {
                          auxiliar = NULL; data = NULL; p= NULL
                          if(aux == "QUI")
                          {
                            ciudad_arana<-"RM/D19"
                            col_ciudad<-rep(aux,horas+1)
                            data<-data.frame(col_fecha,col_ciudad)
                            p= NULL; auxiliar = NULL
                            for(p in 1:length(Contaminantes))
                            {
                              auxiliar<- Contaminantes[p]
                              url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL; PM10 = NULL
                              if(auxiliar=="PM10")
                              {
                                contaminante_arana<-"/Cal/PM10//PM10"
                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                try(
                                  {
                                    PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                    PM10_col1<-PM10_Bruto$Registros.validados
                                    PM10_col2<-PM10_Bruto$Registros.preliminares
                                    PM10_col3<-PM10_Bruto$Registros.no.validados
                                    PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
                                    data<-data.frame(data,PM10)
                                    print(paste(auxiliar,aux))
                                  }
                                  ,silent = True)
                                
                              } else
                              {
                                url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL; PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL
                                if(Contaminantes[p]=="PM25")
                                {
                                  contaminante_arana<-"/Cal/PM25//PM25"
                                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                  try(
                                    {
                                      PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                      PM25_col1<-PM25_Bruto$Registros.validados
                                      PM25_col2<-PM25_Bruto$Registros.preliminares
                                      PM25_col3<-PM25_Bruto$Registros.no.validados
                                      PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                                      data<-data.frame(data,PM25)
                                      print(paste(auxiliar,aux))
                                    }
                                    , silent = TRUE)
                                } else
                                {
                                  url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL
                                  if(Contaminantes[p]=="O3")
                                  {
                                    contaminante_arana<-"/Cal/0008//0008"
                                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                    try(
                                      {
                                        O3_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                        O3_col1<-O3_Bruto$Registros.validados
                                        O3_col2<-O3_Bruto$Registros.preliminares
                                        O3_col3<-O3_Bruto$Registros.no.validados
                                        O3<-gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                                        data<-data.frame(data,O3)
                                        print(paste(auxiliar,aux))
                                      }
                                      , silent = TRUE)
                                  } else
                                  {
                                    url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL
                                    if(Contaminantes[p]=="CO")
                                    {
                                      contaminante_arana<-"/Cal/0004//0004"
                                      url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                      try(
                                        {
                                          CO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                          CO_col1<-CO_Bruto$Registros.validados
                                          CO_col2<-CO_Bruto$Registros.preliminares
                                          CO_col3<-CO_Bruto$Registros.no.validados
                                          CO<-gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                                          data<-data.frame(data,CO)
                                          print(paste(auxiliar,aux))
                                        }
                                        , silent = TRUE)
                                    } else
                                    {
                                      url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL
                                      if(Contaminantes[p]=="NO")
                                      {
                                        contaminante_arana<-"/Cal/0002//0002"
                                        url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                        try(
                                          {
                                            NO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                            NO_col1<-NO_Bruto$Registros.validados
                                            NO_col2<-NO_Bruto$Registros.preliminares
                                            NO_col3<-NO_Bruto$Registros.no.validados
                                            NO<-gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                                            data<-data.frame(data,NO)
                                            print(paste(auxiliar,aux))
                                          }
                                          , silent = TRUE)
                                      } else
                                      {
                                        url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL
                                        if(Contaminantes[p]=="NO2")
                                        {
                                          contaminante_arana<-"/Cal/0003//0003"
                                          url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                          try(
                                            {
                                              NO2_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                              NO2_col1<-NO2_Bruto$Registros.validados
                                              NO2_col2<-NO2_Bruto$Registros.preliminares
                                              NO2_col3<-NO2_Bruto$Registros.no.validados
                                              NO2<-gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                                              data<-data.frame(data,NO2)
                                              print(paste(auxiliar,aux))
                                            }
                                          )
                                        } else
                                        {
                                          url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                                          if(Contaminantes[p]=="NOX")
                                          {
                                            contaminante_arana<-"/Cal/0NOX//0NOX"
                                            url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,".horario.horario.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                            try(
                                              {
                                                NOX_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                NOX_col1<-NOX_Bruto$Registros.validados
                                                NOX_col2<-NOX_Bruto$Registros.preliminares
                                                NOX_col3<-NOX_Bruto$Registros.no.validados
                                                NOX<-gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                                                data<-data.frame(data,NOX)
                                                print(paste(auxiliar,aux))
                                              }
                                              , silent = TRUE)
                                          } else
                                          {
                                            url = NULL; contaminante_arana= NULL; temperatura_bruto =NULL;temperatura_col1 = NULL; temperatura = NULL
                                            if(Contaminantes[p]=="Temperatura")
                                            {
                                              contaminante_arana<-"/Met/TEMP//"
                                              url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                              try(
                                                {
                                                  temperatura_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                  temperatura_col1<-temperatura_bruto$X
                                                  Temperatura<-gsub("NA","",gsub(" ", "",temperatura_col1))
                                                  data<-data.frame(data,Temperatura)
                                                  print(paste(auxiliar,aux))
                                                }
                                                , silent = TRUE)
                                            } else
                                            {
                                              url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                                              if(Contaminantes[p]=="HR")
                                              {
                                                contaminante_arana<-"/Met/RHUM//"
                                                url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                                try(
                                                  {
                                                    HR_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                    HR_col1<-HR_bruto$X
                                                    HR<-gsub("NA","",gsub(" ", "",HR_col1))
                                                    data<-data.frame(data,HR)
                                                    print(paste(auxiliar,aux))
                                                  }
                                                  , silent = TRUE)
                                              } else
                                              {
                                                url = NULL; contaminante_arana= NULL; WD_bruto =NULL;WD_col1 = NULL; WD = NULL
                                                if(Contaminantes[p]=="WD")
                                                {
                                                  contaminante_arana<-"/Met/WDIR//"
                                                  url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000_spec.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                                  try(
                                                    {
                                                      WD_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                      WD_col1<-WD_bruto$X
                                                      WD<-gsub("NA","",gsub(" ", "",WD_col1))
                                                      data<-data.frame(data,WD)
                                                      print(paste(auxiliar,aux))
                                                    }
                                                    , silent = TRUE)
                                                } else
                                                {
                                                  url = NULL; contaminante_arana= NULL; WS_bruto =NULL;WS_col1 = NULL; WS = NULL
                                                  if(Contaminantes[p]=="WS")
                                                  {
                                                    contaminante_arana<-"/Met/WSPD//"
                                                    url<-gsub(" ", "",paste("https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./",ciudad_arana,contaminante_arana,"horario_000.ic&",id_fecha,"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="))
                                                    try(
                                                      {
                                                        WS_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                                        WS_col1<-WS_bruto$X
                                                        WS<-gsub("NA","",gsub(" ", "",WS_col1))
                                                        data<-data.frame(data,WS)
                                                        print(paste(auxiliar,aux))
                                                      }
                                                      , silent = TRUE)
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
                                  }
                                }
                              }
                            }
                            data_Quilicura_i<-data
                            try(
                              {
                                data_total<-rbind(data_total, data_Quilicura_i)
                              }
                              , silent = T)
                          } else
                          {
                            print(paste("Comuna",aux,"no soportada en el Software"))
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  print("Datos Capturados!")
  data_total
}
