ChileAirQuality<-function(Comunas = "INFO", Contaminantes = "PM10", input_fecha_inicio = "01/01/2020",
                          input_fecha_termino = "21/12/2020", val = TRUE, Site = FALSE){
  
  #Matriz de datos
  cod<-c("SA", "CE1", "CE", "CN","EB", "IN","LF","LC","PU","PA","QU","QU1", "COI", "COII")
  codeSINCA<-c("RM/D14","RM/D16","RM/D31","RM/D18","RM/D17","RM/D11","RM/D12","RM/D13","RM/D15","RM/D27","RM/D30","RM/D19", "RXI/B03", "RXI/B04")
  Latitud<-c("-33.4508","-33.4795","-33.4824","-33.4197","-33.5336","-33.4089",
                   "-33.5033","-33.3635","-33.4244","-33.5779","-33.3363","-33.3525","-45.57994", "-45.5790")
  Longitud<-c("-70.6604","-70.7191","-70.7039","-70.7318","-70.6659","-70.6509",
                    "-70.5879","-70.5230","-70.7499","-70.5941","-70.7236","-70.7480", "-72.0611", "-72.0500")
  Estacion<-c("P. O'Higgins","Cerrillos 1", "Cerrillos", "Cerro Navia", "El Bosque","Independecia","La Florida",
                 "Las Condes","Pudahuel","Puente Alto","Quilicura","Quilicura 1","Coyhaique I", "Coyhaique II")
  city_table<-data.frame(cod,codeSINCA,Latitud,Longitud, Estacion)
  
  fi<-paste(input_fecha_inicio,"1:00")
  ft<-paste(input_fecha_termino,"23:00")
  Fecha_inicio<- as.POSIXct(strptime(fi, format = "%d/%m/%Y %H:%M"))
  Fecha_termino<- as.POSIXct(strptime(ft, format = "%d/%m/%Y %H:%M"))
  
  #Fechas para arana#
  Fecha_inicio_para_arana<-as.character(Fecha_inicio, format("%y%m%d"))
  Hora_inicio_para_arana<- as.character(Fecha_inicio, format( "%H%M")) 
  Fecha_termino_para_arana<- as.character(Fecha_termino, format("%y%m%d"))
  Hora_termino_para_arana<-as.character(Fecha_termino, format( "%H%M"))
  id_fecha<-gsub(" ","",paste("from=",Fecha_inicio_para_arana,"&to=",Fecha_termino_para_arana))
  horas<-(as.numeric(Fecha_termino)/3600-as.numeric(Fecha_inicio)/3600) #horas entre fecha y fecha
  
  #URLs
  sinca<-"https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./"
  url_sinca3<-"&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath="
  
 
  date= NULL; n =NULL
  for(n in 0:horas) 
  {
    date<-c(date, as.character(Fecha_inicio+3600*n, "%d/%m/%Y %H:%M"))
  }
  data<-data.frame(date)#Columna Fecha
  data_total<-data.frame() #Data frame Vacio
  #Selector de aranas#
  
  
  j<-NULL; i<-NULL;aux_1<-NULL;aux_2<-NULL;aux_3<-NULL; aux_4<-NULL;aux_5<-NULL; aux_6<-NULL
  
  for (i in 1:length(Comunas)) {
    try({
      aux_1<-Comunas[i]
      for(j in 1:length(city)){
        aux_2<-city_table[j,1] #Site
        aux_3<-city_table[j,2] # code_city
        aux_4<-city_table[j,3] #Latitud
        aux_5<-city_table[j,4] # Longitud
        aux_7<-city_table[j,5] #Estacion de Monitoreo
        if(Site){
          aux_6 = aux_2
        }else{
          aux_6 = aux_7
        }
        if(aux_1==aux_6){
          try({
            
            site<-rep(aux_2,horas+1)
            longitude<-rep(aux_4,horas+1)
            latitude<-rep(aux_5,horas+1)
            data<-data.frame(date,site,longitude,latitude)
            {
              p= NULL; auxiliar = NULL
              for(p in 1:length(Contaminantes))
              {
                auxiliar<- Contaminantes[p]
                url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL; PM10 = NULL
                if(auxiliar=="PM10")
                {
                  contaminante_arana<-"/Cal/PM10//PM10.horario.horario.ic&"
                  url<-gsub(" ", "",paste(sinca,aux_3,contaminante_arana,id_fecha,url_sinca3))
                  try(
                    {
                      PM10_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                      PM10_col1<-PM10_Bruto$Registros.validados
                      PM10_col2<-PM10_Bruto$Registros.preliminares
                      PM10_col3<-PM10_Bruto$Registros.no.validados
                      PM10<-gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3)))
                      if(length(PM10)==0){PM10<-rep("",horas+1)}
                      data<-data.frame(data,PM10)
                      print(paste(auxiliar,aux_1))
                    }
                    ,silent = T)
                  
                } else
                {
                  url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL; PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL
                  if(Contaminantes[p]=="PM25")
                  {
                    contaminante_arana<-"/Cal/PM25//PM25.horario.horario.ic&"
                    url<-gsub(" ", "",paste(sinca,aux_3,contaminante_arana,id_fecha,url_sinca3))
                    try(
                      {
                        PM25_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                        PM25_col1<-PM25_Bruto$Registros.validados
                        PM25_col2<-PM25_Bruto$Registros.preliminares
                        PM25_col3<-PM25_Bruto$Registros.no.validados
                        PM25<-gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                        if(length(PM25)==0){PM25<-rep("",horas+1)}
                        data<-data.frame(data,PM25)
                        print(paste(auxiliar,aux_1))
                      }
                      , silent = TRUE)
                  } else
                  {
                    url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL
                    if(Contaminantes[p]=="O3")
                    {
                      contaminante_arana<-"/Cal/0008//0008.horario.horario.ic&"
                      url<-gsub(" ", "",paste(sinca,aux_3,contaminante_arana,id_fecha,url_sinca3))
                      try(
                        {
                          O3_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                          O3_col1<-O3_Bruto$Registros.validados
                          O3_col2<-O3_Bruto$Registros.preliminares
                          O3_col3<-O3_Bruto$Registros.no.validados
                          O3<-gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                          if(length(O3)==0){O3<-rep("",horas+1)}
                          data<-data.frame(data,O3)
                          print(paste(auxiliar,aux_1))
                        }
                        , silent = TRUE)
                    } else
                    {
                      url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL
                      if(Contaminantes[p]=="CO")
                      {
                        contaminante_arana<-"/Cal/0004//0004.horario.horario.ic&"
                        url<-gsub(" ", "",paste(sinca,aux_3,contaminante_arana,id_fecha,url_sinca3))
                        try(
                          {
                            CO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                            CO_col1<-CO_Bruto$Registros.validados
                            CO_col2<-CO_Bruto$Registros.preliminares
                            CO_col3<-CO_Bruto$Registros.no.validados
                            CO<-gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                            if(length(O3)==0){O3<-rep("",horas+1)}
                            data<-data.frame(data,CO)
                            print(paste(auxiliar,aux_1))
                          }
                          , silent = TRUE)
                      } else
                      {
                        url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL
                        if(Contaminantes[p]=="NO")
                        {
                          contaminante_arana<-"/Cal/0002//0002.horario.horario.ic&"
                          url<-gsub(" ", "",paste(sinca,aux_3,contaminante_arana,id_fecha,url_sinca3))
                          try(
                            {
                              NO_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                              NO_col1<-NO_Bruto$Registros.validados
                              NO_col2<-NO_Bruto$Registros.preliminares
                              NO_col3<-NO_Bruto$Registros.no.validados
                              NO<-gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                              if(length(NO)==0){NO<-rep("",horas+1)}
                              data<-data.frame(data,NO)
                              print(paste(auxiliar,aux_1))
                            }
                            ,silent = T)
                        } else
                        {
                          url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL
                          if(Contaminantes[p]=="NO2")
                          {
                            contaminante_arana<-"/Cal/0003//0003.horario.horario.ic&"
                            url<-gsub(" ", "",paste(sinca,aux_3,contaminante_arana,id_fecha,url_sinca3))
                            try(
                              {
                                NO2_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                NO2_col1<-NO2_Bruto$Registros.validados
                                NO2_col2<-NO2_Bruto$Registros.preliminares
                                NO2_col3<-NO2_Bruto$Registros.no.validados
                                NO2<-gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                                if(length(NO2)==0){NO2<-rep("",horas+1)}
                                data<-data.frame(data,NO2)
                                print(paste(auxiliar,aux_1))
                              }
                              , silent = TRUE)
                          } else
                          {
                            url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                            if(Contaminantes[p]=="NOX")
                            {
                              contaminante_arana<-"/Cal/0NOX//0NOX.horario.horario.ic&"
                              url<-gsub(" ", "",paste(sinca,aux_3,contaminante_arana,id_fecha,url_sinca3))
                              try(
                                {
                                  NOX_Bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                  NOX_col1<-NOX_Bruto$Registros.validados
                                  NOX_col2<-NOX_Bruto$Registros.preliminares
                                  NOX_col3<-NOX_Bruto$Registros.no.validados
                                  NOX<-gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                                  if(length(NOX)==0){NOX<-rep("",horas+1)}
                                  data<-data.frame(data,NOX)
                                  print(paste(auxiliar,aux_1))
                                }
                                , silent = TRUE)
                            } else
                            {
                              url = NULL; contaminante_arana= NULL; temp_bruto =NULL;temp_col1 = NULL; temp = NULL
                              if(Contaminantes[p]=="temp")
                              {
                                contaminante_arana<-"/Met/TEMP//horario_000.ic&"
                                url<-gsub(" ", "",paste(sinca,aux_3,contaminante_arana,id_fecha,url_sinca3))
                                try(
                                  {
                                    temp_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                    temp_col1<-temp_bruto$X
                                    temp<-gsub("NA","",gsub(" ", "",temp_col1))
                                    if(length(temp)==0){temp<-rep("",horas+1)}
                                    data<-data.frame(data,temp)
                                    print(paste(auxiliar,aux_1))
                                  }
                                  , silent = TRUE)
                              } else
                              {
                                url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                                if(Contaminantes[p]=="HR")
                                {
                                  contaminante_arana<-"/Met/RHUM//horario_000.ic&"
                                  url<-gsub(" ", "",paste(sinca,aux_3,contaminante_arana,id_fecha,url_sinca3))
                                  try(
                                    {
                                      HR_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                      HR_col1<-HR_bruto$X
                                      HR<-gsub("NA","",gsub(" ", "",HR_col1))
                                      if(length(HR)==0){HR<-rep("",horas+1)}
                                      data<-data.frame(data,HR)
                                      print(paste(auxiliar,aux_1))
                                    }
                                    , silent = TRUE)
                                } else
                                {
                                  url = NULL; contaminante_arana= NULL; wd_bruto =NULL;wd_col1 = NULL; wd = NULL
                                  if(Contaminantes[p]=="wd")
                                  {
                                    contaminante_arana<-"/Met/WDIR//horario_000_spec.ic&"
                                    url<-gsub(" ", "",paste(sinca,aux_3,contaminante_arana,id_fecha,url_sinca3))
                                    try(
                                      {
                                        wd_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                        wd_col1<-wd_bruto$X
                                        wd<-gsub("NA","",gsub(" ", "",wd_col1))
                                        if(length(wd)==0){wd<-rep("",horas+1)}
                                        data<-data.frame(data,wd)
                                        print(paste(auxiliar,aux_1))
                                      }
                                      , silent = TRUE)
                                  } else
                                  {
                                    url = NULL; contaminante_arana= NULL; ws_bruto =NULL;ws_col1 = NULL; ws = NULL
                                    if(Contaminantes[p]=="ws")
                                    {
                                      contaminante_arana<-"/Met/WSPD//horario_000.ic&"
                                      url<-gsub(" ", "",paste(sinca,aux_3,contaminante_arana,id_fecha,url_sinca3))
                                      try(
                                        {
                                          ws_bruto<-read.csv(url,dec =",", sep= ";",na.strings= "")
                                          ws_col1<-ws_bruto$X
                                          ws<-gsub("NA","",gsub(" ", "",ws_col1))
                                          if(length(ws)==0){ws<-rep("",horas+1)}
                                          data<-data.frame(data,ws)
                                          print(paste(auxiliar,aux_1))
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
              
              try(
                {
                  data_total<-rbind(data_total,data)
                }
                , silent = T)
            }
            
          }
          , silent = T)
        }
        else if(aux_1 =="INFO"){
          return((city_table))
        }
      }
      
      
    }, silent = T)
  }
  
  if(val){
    try({
      i =NULL
      for (i in 1:(horas+1)) 
      {
        try(
          {
            if((as.numeric(data_total$NO[i])+as.numeric(data_total$NO2[i]))>as.numeric(data_total$NOX[i])*1.001){
              data_total$NO[i] = ""
              data_total$NO2[i] = ""
              data_total$NOX[i] = ""
              
            }
          }
          , silent = T)
      }
    }, silent = T)
    
    try({
      i =NULL
      for (i in 1:length(data_total$PM10)) 
      {
        try(
          {
            if(as.numeric(data_total$PM25[i]) > as.numeric(data_total$PM10[i])*1.001){
              data_total$PM10[i] = ""
              data_total$PM25[i] = ""
            }
          }  
          ,silent = T)
      }
    }, silent = T)
    
    try({
      i =NULL
      for (i in 1:length(data_total$wd)) 
      {
        try({
          if(as.numeric(data_total$wd[i]) > 360||as.numeric(data_total$wd[i]) <0){
            data_total$wd[i] = ""
          }
        }, silent = T)
      }
      
    }, silent = T)
    
    try({
      i =NULL
      for (i in 1:length(data_total$HR)) 
      {
        try(
          {
            if(as.numeric(data_total$HR[i]) > 100||as.numeric(data_total$HR[i]) <0){
              data_total$HR[i] = ""
            }
            
          }, silent = T)
      }
      
    }, silent = T)
  }
  
  k= NULL
  for(k in 3:ncol(data_total)){
    data_total[[k]]<-as.numeric(data_total[[k]])
    
  }
  print("Datos Capturados!")
  return(data_total)
}
