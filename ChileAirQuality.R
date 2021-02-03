ChileAirQuality <- function(Comunas = "INFO", Contaminantes, fechadeInicio,
                          fechadeTermino, Site = FALSE, Curar = TRUE){
  Ciudad <- c("SA", "CE1", "CE", "CN","EB", "IN","LF","LC","PU","PA","QU","QU1", "COI", "COII")
  cod <- c("RM/D14", "RM/D16", "RM/D31", "RM/D18", "RM/D17", "RM/D11", "RM/D12",
         "RM/D13", "RM/D15", "RM/D27", "RM/D30", "RM/D19", "RXI/B03", "RXI/B04")
  Latitud <- c("-33.450819", "-33.479515", "-33.482411","-33.419725","-33.533626","-33.408920",
             "-33.503288", "-33.363453", "-33.424439", "-33.577948", "-33.33632",
             "-33.352539", "-45.57993636", "-45.57904645")
  Longitud <- c("-70.6604476", "-70.719064", "-70.703947"," -70.731790", "-70.665906", 
              "-70.650886", "-70.587916", "-70.523024", "-70.749876", "-70.594184",
              "-70.723583", "-70.747952", "-72.06108480", "-72.04996681")
  Estacion <- c("P. O'Higgins", "Cerrillos 1", "Cerrillos", "Cerro Navia", "El Bosque",
              "Independecia", "La Florida", "Las Condes", "Pudahuel", "Puente Alto",
              "Quilicura", "Quilicura 1", "Coyhaique I", "Coyhaique II")
  estationMatrix <- data.frame(Ciudad, cod, Latitud, Longitud, Estacion) #Matriz base de estaciones de monitoreo
  info = Comunas[1]
  if(info =="INFO"){ #"INFO" para solicitar informacion de estaciones de monitoreo
    return((estationMatrix)) #Retorna matriz de estaciones
  }else{
    
    fi <- paste(fechadeInicio,"1:00") #incluir hora en fecha de inicio
    ft <- paste(fechadeTermino,"23:00") # incluir hora en fecha de termino
    Fecha_inicio <-  as.POSIXct(strptime(fi, format = "%d/%m/%Y %H:%M")) #Asignar formato de fecha de termino
    Fecha_termino<-as.POSIXct(strptime(ft, format = "%d/%m/%Y %H:%M")) #Asignar formato de fecha de termino
    
    #Fechas para arana#
    Fecha_inicio_para_arana <- as.character(Fecha_inicio, format("%y%m%d")) #formato fecha inicio para el enrutador
    Fecha_termino_para_arana <-  as.character(Fecha_termino, format("%y%m%d")) #formato fecha termino para el enrutador
    id_fecha <- gsub(" ","",paste("from=", Fecha_inicio_para_arana, "&to=", Fecha_termino_para_arana)) #codigo de intervalo de fechas para enrutador
    horas <- (as.numeric(Fecha_termino)/3600-as.numeric(Fecha_inicio)/3600) #horas entre fechas
    
    
    
    urlSinca  <- "https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=./" #parte inicial url de extraccion
    urlSinca2 <- "&path=/usr/airviro/data/CONAMA/&lang=esp&rsrc=&macropath=" #parte final de ur de extraccion
    
    #Data frame vacio#
    date= NULL; n =NULL #Limpiar variables para evitar errores
    for(n in 0:horas) 
    {
      date <- c(date, as.character(Fecha_inicio+3600*n, "%d/%m/%Y %H:%M")) #Generar coluna fecha
    }
    data <- data.frame(date)#Columna Fecha
    data_total <- data.frame() #Data frame Vacio
    #Selector de aranas#
    
    
    j <- NULL; i <- NULL;inEstation <- NULL;mSite <- NULL; mCod <- NULL;
    mLat <- NULL;mLon <- NULL; mEstation <- NULL #Limpiar variables en caso de errores
    
    for (i in 1:length(Comunas)) {
      try({
        inEstation <- Comunas[i] # Asignar Comunas a variable
        for(j in 1:length(Ciudad)){
          mSite      <-  estationMatrix[j, 1] #Asignar site a variable
          mCod       <-  estationMatrix[j, 2] #Asignar code a variable
          mLat       <-  estationMatrix[j, 3] #Asignar latitud a variable
          mLon       <-  estationMatrix[j, 4] #Asignar longitud a variable
          mEstation  <-  estationMatrix[j, 5] #Asignar estacion a variable
          if(Site){                 # Si Site es verdadero 
            aux      <-  mSite      #aux, la variable de comparacion
          }else{                    #Se comparara con Site
            aux      <-  mEstation  #Si es falso se comparara con El nombre de la estacion
          }
          if(inEstation == aux){ 
            try({
              
              site <- rep(mSite, horas + 1) #Generar columna site
              longitude <- rep(mLat, horas + 1) #Generar columna longitud
              latitude <- rep(mLon, horas + 1) # Generar columna latitud
              data <- data.frame(date, site, longitude, latitude) #Unir columnas
              {
                p= NULL; inContaminante = NULL #Limpiar variable
                for(p in 1:length(Contaminantes))
                {
                  inContaminante <-  Contaminantes[p] #Asignar contaminante a variable
                  url = NULL; contaminante_arana= NULL; PM10_Bruto =NULL;
                  PM10_col1 = NULL; PM10_col2 = NULL; PM10_col3 = NULL;
                  PM10 = NULL #Limpiar variables
                  if(inContaminante=="PM10")
                  {
                    contaminante_arana <- "/Cal/PM10//PM10.horario.horario.ic&" #Codigo especifico para PM10
                    url <- gsub(" ", "",paste(urlSinca, mCod, contaminante_arana, id_fecha,
                                            urlSinca2)) #Generar URL
                    try(
                      {
                        PM10_Bruto <- read.csv(url,dec =",", sep= ";",na.strings= "") #Descargar csv
                        PM10_col1  <- PM10_Bruto$Registros.validados 
                        PM10_col2  <- PM10_Bruto$Registros.preliminares
                        PM10_col3  <- PM10_Bruto$Registros.no.validados
                        PM10 <- gsub("NA","",gsub(" ", "",paste(PM10_col1,PM10_col2,PM10_col3))) #unir columnas del csv
                        if(length(PM10)==0){PM10 <- rep("", horas + 1)}# Generar columna vacia en caso de que no exista informacion
                        data <- data.frame(data,PM10) #Incorporar al df de la comuna
                        print(paste(inContaminante,inEstation)) #Imprimir mnsje de exito
                      }
                      ,silent = T)
                    
                  } else
                  {
                    url = NULL; contaminante_arana= NULL; PM25_Bruto =NULL;PM25_col1 = NULL;
                    PM25_col2 = NULL; PM25_col3 = NULL; PM25 = NULL #Limpiar variables
                    if(Contaminantes[p] == "PM25")
                    {
                      contaminante_arana <- "/Cal/PM25//PM25.horario.horario.ic&" #Codigo especifico PM25
                      url <- gsub(" ", "",paste(urlSinca,
                                              mCod, contaminante_arana, id_fecha,
                                              urlSinca2)) #Generar URL
                      try(
                        {
                          PM25_Bruto <- read.csv(url,dec =",", sep= ";",na.strings= "")
                          PM25_col1 <- PM25_Bruto$Registros.validados
                          PM25_col2 <- PM25_Bruto$Registros.preliminares
                          PM25_col3 <- PM25_Bruto$Registros.no.validados
                          PM25 <- gsub("NA","",gsub(" ", "",paste(PM25_col1,PM25_col2,PM25_col3)))
                          if(length(PM25)==0){PM25 <- rep("",horas + 1)}
                          data <- data.frame(data,PM25) #Crear columna
                          print(paste(inContaminante, inEstation)) #Mensaje de exito
                        }
                        , silent = TRUE)
                    } else
                    {
                      url = NULL; contaminante_arana= NULL; O3_Bruto =NULL;
                      O3_col1 = NULL; O3_col2 = NULL; O3_col3 = NULL; O3 = NULL #Limpiar variables
                      if(Contaminantes[p]== "O3")
                      {
                        contaminante_arana <- "/Cal/0008//0008.horario.horario.ic&" #Codigo url Ozono
                        url <- gsub(" ", "",paste(urlSinca, mCod, contaminante_arana,
                                                id_fecha, urlSinca2)) #Generarurl
                        try(
                          {
                            O3_Bruto <- read.csv(url,dec =",", sep= ";",na.strings= "")
                            O3_col1 <- O3_Bruto$Registros.validados
                            O3_col2 <- O3_Bruto$Registros.preliminares
                            O3_col3 <- O3_Bruto$Registros.no.validados
                            O3 <- gsub("NA","",gsub(" ", "",paste(O3_col1,O3_col2,O3_col3)))
                            if(length(O3) == 0){O3 <- rep("",horas + 1)}
                            data <- data.frame(data,O3)
                            print(paste(inContaminante,inEstation))
                          }
                          , silent = TRUE)
                      } else
                      {
                        url = NULL; contaminante_arana= NULL; CO_Bruto =NULL;
                        CO_col1 = NULL; CO_col2 = NULL; CO_col3 = NULL; CO = NULL #Limpiar variables
                        if(Contaminantes[p]=="CO")
                        {
                          contaminante_arana <- "/Cal/0004//0004.horario.horario.ic&" #Codigo CO
                          url <- gsub(" ", "",paste(urlSinca, mCod, contaminante_arana,
                                                    id_fecha, urlSinca2)) #Generar url
                          try(
                            {
                              CO_Bruto <- read.csv(url,dec =",", sep= ";",na.strings= "")
                              CO_col1 <- CO_Bruto$Registros.validados
                              CO_col2 <- CO_Bruto$Registros.preliminares
                              CO_col3 <- CO_Bruto$Registros.no.validados
                              CO <- gsub("NA","",gsub(" ", "",paste(CO_col1,CO_col2,CO_col3)))
                              if(length(O3)==0){O3 <- rep("",horas + 1)}
                              data <- data.frame(data,CO)
                              print(paste(inContaminante, inEstation)) #mensaje de exito
                            }
                            , silent = TRUE)
                        } else
                        {
                          url = NULL; contaminante_arana= NULL; NO_Bruto =NULL;
                          NO_col1 = NULL; NO_col2 = NULL; NO_col3 = NULL; NO = NULL #limpiar url
                          if(Contaminantes[p]=="NO")
                          {
                            contaminante_arana <- "/Cal/0002//0002.horario.horario.ic&" #codigo monoxido de carbono
                            url <- gsub(" ", "",paste(urlSinca, mCod, contaminante_arana,
                                                      id_fecha, urlSinca2)) #generar url
                            try(
                              {
                                NO_Bruto <- read.csv(url,dec =",", sep= ";",na.strings= "")
                                NO_col1 <- NO_Bruto$Registros.validados
                                NO_col2 <- NO_Bruto$Registros.preliminares
                                NO_col3 <- NO_Bruto$Registros.no.validados
                                NO <- gsub("NA","",gsub(" ", "",paste(NO_col1,NO_col2,NO_col3)))
                                if(length(NO)==0){NO <- rep("",horas + 1)}
                                data <- data.frame(data,NO)
                                print(paste(inContaminante,inEstation)) #mensaje de exito
                              }
                              ,silent = T)
                          } else
                          {
                            url = NULL; contaminante_arana= NULL; NO2_Bruto =NULL;
                            NO2_col1 = NULL; NO2_col2 = NULL; NO2_col3 = NULL; NO2 = NULL #limpiar variable
                            if(Contaminantes[p]=="NO2")
                            {
                              contaminante_arana <- "/Cal/0003//0003.horario.horario.ic&" #codigo dioxido de carbono
                              url <- gsub(" ", "",paste(urlSinca, mCod, contaminante_arana,
                                                        id_fecha, urlSinca2))
                              try(
                                {
                                  NO2_Bruto <- read.csv(url,dec =",", sep= ";",na.strings= "")
                                  NO2_col1 <- NO2_Bruto$Registros.validados
                                  NO2_col2 <- NO2_Bruto$Registros.preliminares
                                  NO2_col3 <- NO2_Bruto$Registros.no.validados
                                  NO2 <- gsub("NA","",gsub(" ", "",paste(NO2_col1,NO2_col2,NO2_col3)))
                                  if(length(NO2)==0){NO2 <- rep("",horas + 1)}
                                  data <- data.frame(data,NO2)
                                  print(paste(inContaminante,inEstation))
                                }
                                , silent = TRUE)
                            } else
                            {
                              url = NULL; contaminante_arana= NULL; NOX_Bruto =NULL;NOX_col1 = NULL; NOX_col2 = NULL; NOX_col3 = NULL; NOX = NULL
                              if(Contaminantes[p]=="NOX")
                              {
                                contaminante_arana <- "/Cal/0NOX//0NOX.horario.horario.ic&"
                                url <- gsub(" ", "",paste(urlSinca,
                                                        mCod, contaminante_arana, id_fecha,
                                                        urlSinca2))
                                try(
                                  {
                                    NOX_Bruto <- read.csv(url,dec =",", sep= ";",na.strings= "")
                                    NOX_col1 <- NOX_Bruto$Registros.validados
                                    NOX_col2 <- NOX_Bruto$Registros.preliminares
                                    NOX_col3 <- NOX_Bruto$Registros.no.validados
                                    NOX <- gsub("NA","",gsub(" ", "",paste(NOX_col1,NOX_col2,NOX_col3)))
                                    if(length(NOX)==0){NOX <- rep("",horas + 1)}
                                    data <- data.frame(data,NOX)
                                    print(paste(inContaminante,inEstation))
                                  }
                                  , silent = TRUE)
                              } else
                              {
                                url = NULL; contaminante_arana= NULL; temp_bruto =NULL;temp_col1 = NULL; temp = NULL
                                if(Contaminantes[p]=="temp")
                                {
                                  contaminante_arana <- "/Met/TEMP//horario_000.ic&"
                                  url <- gsub(" ", "",paste(urlSinca,
                                                          mCod, contaminante_arana, id_fecha,
                                                          urlSinca2))
                                  try(
                                    {
                                      temp_bruto <- read.csv(url,dec =",", sep= ";",na.strings= "")
                                      temp_col1 <- temp_bruto$X
                                      temp <- gsub("NA","",gsub(" ", "",temp_col1))
                                      if(length(temp)==0){temp <- rep("",horas + 1)}
                                      data <- data.frame(data,temp)
                                      print(paste(inContaminante, inEstation))
                                    }
                                    , silent = TRUE)
                                } else
                                {
                                  url = NULL; contaminante_arana= NULL; HR_bruto =NULL;HR_col1 = NULL; HR = NULL
                                  if(Contaminantes[p]=="HR")
                                  {
                                    contaminante_arana <- "/Met/RHUM//horario_000.ic&"
                                    url <- gsub(" ", "",paste(urlSinca, 
                                                            mCod, contaminante_arana, id_fecha,
                                                            urlSinca2))
                                    try(
                                      {
                                        HR_bruto <- read.csv(url,dec =",", sep= ";",na.strings= "")
                                        HR_col1 <- HR_bruto$X
                                        HR <- gsub("NA","",gsub(" ", "",HR_col1))
                                        if(length(HR)==0){HR <- rep("",horas + 1)}
                                        data <- data.frame(data,HR)
                                        print(paste(inContaminante,inEstation))
                                      }
                                      , silent = TRUE)
                                  } else
                                  {
                                    url = NULL; contaminante_arana= NULL; wd_bruto =NULL;wd_col1 = NULL; wd = NULL
                                    if(Contaminantes[p]=="wd")
                                    {
                                      contaminante_arana <- "/Met/WDIR//horario_000_spec.ic&"
                                      url <- gsub(" ", "",paste(urlSinca, mCod,
                                                                contaminante_arana,
                                                                id_fecha, 
                                                                urlSinca2))
                                      try(
                                        {
                                          wd_bruto <- read.csv(url,dec =",", sep= ";",na.strings= "")
                                          wd_col1 <- wd_bruto$X
                                          wd <- gsub("NA","",gsub(" ", "",wd_col1))
                                          if(length(wd) ==0 ){wd  <-  rep("",horas + 1)}
                                          data <- data.frame(data,wd)
                                          print(paste(inContaminante,inEstation))
                                        }
                                        , silent = TRUE)
                                    } else
                                    {
                                      url = NULL; contaminante_arana= NULL; ws_bruto =NULL;ws_col1 = NULL; ws = NULL
                                      if(Contaminantes[p]=="ws")
                                      {
                                        contaminante_arana <- "/Met/WSPD//horario_000.ic&"
                                        url <- gsub(" ", "",paste(urlSinca,
                                                                mCod, contaminante_arana, id_fecha,
                                                                urlSinca2))
                                        try(
                                          {
                                            ws_bruto <- read.csv(url,dec =",", sep= ";",na.strings= "")
                                            ws_col1 <- ws_bruto$X
                                            ws <- gsub("NA","",gsub(" ", "",ws_col1))
                                            if(length(ws) == 0){ws <- rep("",horas + 1)}
                                            data <- data.frame(data,ws)
                                            print(paste(inContaminante,inEstation))
                                          }
                                          , silent = TRUE)
                                      } else
                                      {
                                        print(paste("Contaminante",inContaminante,"no soportado en el Software")) #Generar mensaje de fracaso
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
                    data_total <- rbind(data_total, data) #Unir el df de cada comuna al df total
                  }
                  , silent = T)
              }
              
            }
            , silent = T)
          }
        }
        
        
      }, silent = T)
    }
    
    if(Curar){
      try({
        i =NULL
        for (i in 1:(horas + 1)) 
        {
          try(
            {
              if((as.numeric(data_total$NO[i]) + as.numeric(data_total$NO2[i])) > as.numeric(data_total$NOX[i]) * 1.001){
                data_total$NO[i] = "" #Si la suma de NO y NO2 es mayor a NOX
                data_total$NO2[i] = "" #Eliminar el dato de NO, NO2 y NOX
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
                data_total$PM10[i] = "" #Si PM25 es mayor a PM10 borrar PM10
                data_total$PM25[i] = "" #Y PM25
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
            if(as.numeric(data_total$wd[i]) > 360||as.numeric(data_total$wd[i]) < 0){
              data_total$wd[i] = "" #Si la tireccion del viento es menor a 0 o mayor a 360 eliminar el dato
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
                data_total$HR[i] = "" #Si la humedad relativa es mayor al 100% borrar el dato
              }
              
            }, silent = T)
        }
        
      }, silent = T)
    }
    
    
    k= NULL
    for(k in 3:ncol(data_total)){
      data_total[[k]]  <-  as.numeric(data_total[[k]]) #transformar columnas en variables numericas
      
    }
    print("Datos Capturados!")
    return(data_total) #retornar df total
  }
}
