ChileClimateData <- function(Estaciones = "INFO", Parametros, inicio, fin, Region = FALSE){

  tablaEstaciones <- data.frame(
    "Código Nacional" = c("180005","200006","220002","230001","270001","270008","290004","320041","320051","330007","330019","330020","330021","330030","330031","330066","330077","330111","330112","330113","340031","360011","360019","360042","370033","380013","380029","390006","400009","410005","420004","420014","430002","430004","430009","450001","450004","450005","460001","470001","510005","520006","530005","550001","950001","950002","950003"),
    "Código OMM"      = c("85406","85418","85432","85442","85469","85467","85488","85556","85539","85560","85580","85577","85574","85586","85585","85584","85594","85571","85593","85569","85629","85672","85682","85671","85703","85743","85744","85766","85782","85799","85830","85824","85832","85836","85837","85862","85864","85874","85886","85892","85920","85934","85940","85968","89056","89057","89059"),
    "Código OACI"     = c("SCAR","SCDA","SCCF","SCFA","SCIP","SCAT","SCSE","SCVM","","SCRD","SCTB","SCQN","SCEL","SCSN","","SCIR","","","","","SCIC","SCCH","SCIE","","SCGE","SCTC","SCQP","SCVD","SCJO","SCTE","SCTN","SCPQ","SCFT","SCAP","SCMK","SCAS","SCCY","SCBA","SCCC","SCHR","SCNT","SCCI","SCFM","SCGZ","SCRM","SCBP","SCBO"),
    "Nombre"          = c("Chacalluta Arica Ap.","Diego Aracena Iquique Ap.","El Loa Calama Ad.","Cerro Moreno Antofagasta Ap.","Mataveri Isla de Pascua Ap.","Desierto de Atacama Caldera Ad.","La Florida La Serena Ad.","Viña del Mar Ad. (Torquemada)","Los Libertadores","Rodelillo Ad.","Eulogio Sánchez Tobalaba Ad.","Quinta Normal Santiago","Pudahuel Santiago","Santo Domingo Ad.","Juan Fernández Estación Meteorológica.","La Punta Juan Fernández Ad.","El Colorado","Lo Prado Cerro San Francisco","San José Guayacán","El Paico","General Freire Curicó Ad.","General Bernardo O'Higgins Chillán Ad.","Carriel Sur Concepción Ap.","Termas de Chillán","María Dolores Los Angeles Ad.","Maquehue Temuco Ad.","La Araucanía Ad.","Pichoy Valdivia Ad.","Cañal Bajo Osorno Ad.","El Tepual Puerto Montt Ap.","Chaitén Ad.","Mocopulli Ad.","Futaleufú Ad.","Alto Palena Ad.","Melinka Ad.","Puerto Aysén Ad.","Teniente Vidal Coyhaique Ad.","Balmaceda Ad.","Chile Chico Ad.","Lord Cochrane Ad.","Teniente Gallardo Puerto Natales Ad.","Carlos Ibañez Punta Arenas Ap.","Fuentes Martínez Porvenir Ad.","Guardiamarina Zañartu Pto Williams Ad.","C.M.A. Eduardo Frei Montalva Antártica","Base Antártica Arturo Prat","Base Antártica Bernardo O`Higgins"),
    "Latitud"         = c("-18.35555","-20.54917","-22.49806","-23.45361","-27.15889","-27.25444","-29.91444","-32.94944","-32.84555","-33.06528","-33.45528","-33.44500","-33.37833","-33.65611","-33.63583","-33.66639","-33.35000","-33.45806","-33.61528","-33.70639","-34.96944","-36.58583","-36.78055","-36.90361","-37.39694","-38.76778","-38.93444","-39.65667","-40.61444","-41.44750","-42.93028","-42.34667","-43.18889","-43.61167","-43.89778","-45.39944","-45.59083","-45.91833","-46.58500","-47.24389","-51.66722","-53.00167","-53.25361","-54.93167","-62.19194","-62.47861","-63.32083"),
    "Longitud"        = c("-70.33889","-70.16944","-68.89805","-70.44056","-109.42361","-70.77944","-71.20333","-71.47444","-70.11861","-71.55917","-70.54222","-70.67778","-70.79639","-71.61000","-78.83028","-78.93194","-70.28805","-70.94889","-70.35583","-71.00000","-71.22028","-72.03389","-73.05083","-71.40667","-72.42361","-72.62694","-72.66083","-73.08472","-73.05083","-73.08472","-72.71167","-73.71167","-71.86417","-71.81333","-73.74555","-72.67778","-72.10167","-71.67778","-71.69472","-72.57611","-72.52528","-70.84722","-70.32194","-67.61000","-58.98278","-59.66083","-57.89805"),
    "Region"          = c("XV","I","II","II","V","III","VI","V","V","V","RM","RM","RM","V","V","V","RM","RM","RM","RM","VII","XVI","VII","XVI","VIII","IX","IX","XIV","X","X","X","X","X","X","XI","XI","XI","XI","XI","XI","XII","XII","XII","XII","XII","XII","XII")
    
  )

  if(Estaciones[1] == "INFO"){
    return(tablaEstaciones)
  }
  if(fin < inicio){
    print()
    stop("Verificar fechas de inicio y fin")
  }

  url1 <- "https://climatologia.meteochile.gob.cl/application/productos/gethistoricos/"

  parametros_list <- c("Temperatura", "PuntoRocio", "Humedad",
                       "Viento", "PresionQFE", "PresionQFF")
  #temporal <- c("TMinima", "TMaxima", "Agua6Horas", "Agua24Horas")
  #parametros_list <- c(parametros_list, temporal)

  intervalo <- inicio:fin

  lenInEstaciones <- length(Estaciones)
  lenInParametros <- length(Parametros)
  lenEstaciones   <- nrow(tablaEstaciones)
  lenParametros   <- length(parametros_list)
  lendate         <- length(intervalo)

  start <- as.POSIXct(strptime(paste("01-01-", inicio, "00:00:00", sep =""), format = "%d-%m-%Y %H:%M:%S"))
  end <- as.POSIXct(strptime(paste("31-12-", fin, "23:00:00", sep =""), format = "%d-%m-%Y %H:%M:%S"))
  #horas<-(as.numeric(end)/3600-as.numeric(start)/3600)
  date = NULL

  date <- seq(start, end, by = "hour")
  date <- format(date, format = "%d-%m-%Y %H:%M:%S")

  df    <- NULL
  df2   <- NULL
  data_total <- data.frame()

  if(Region == TRUE){
    r <- 7
  }else{
    r <- 1
  }

  for(i in 1:lenInEstaciones){
    for(j in 1:lenEstaciones){
      if(Estaciones[i] == tablaEstaciones[j, r]){

        estacion_var <- tablaEstaciones[j, 1]
        Latitud     <-  tablaEstaciones[j, 5]
        Longitud    <-  tablaEstaciones[j, 6]
        Nombre      <-  rep(tablaEstaciones[j, 4], length(date))
        Latitud     <-  rep(tablaEstaciones[j, 5], length(date))
        Longitud    <-  rep(tablaEstaciones[j, 6], length(date))
        data        <-  data.frame(date, Nombre, Latitud, Longitud)
        setDT(data)

        for(k in 1:lenInParametros){
          for(l in 1:lenParametros){
            if(Parametros[k] == parametros_list[l]){

              for(m in 1:lendate){

                url3 <- paste(url1, estacion_var,"_",intervalo[m], "_", parametros_list[l], "_", sep = "")
                print(url3)
                filename <- paste(estacion_var,"_",intervalo[m],"_", parametros_list[l], ".zip", sep = "")
                csvname <- paste(estacion_var,"_",intervalo[m],"_", parametros_list[l], "_.csv", sep = "")
                CSV <- NULL
                download.file(url3, destfile = filename, method = "curl")
                suppressWarnings({
                  unzip(zipfile = filename)
                  try({
                    CSV <- read.csv(csvname, sep =  ";", dec = ".", encoding = "UTF-8")
                  }, silent = T)
                })

                if(is.null(CSV)| length(CSV) == 0){
                  momento1 <- as.POSIXct(strptime(paste("01-01-", intervalo[m], "00:00:00", sep =""), format = "%d-%m-%Y %H:%M:%S"))
                  momento2 <- as.POSIXct(strptime(paste("31-12-", intervalo[m], "23:00:00", sep =""), format = "%d-%m-%Y %H:%M:%S"))
                  momento <- seq(momento1, momento2, by = "hour")
                  CodigoNacional <-rep("", length(momento))
                  momento <- format(momento, format = "%d-%m-%Y %H:%M:%S")

                  if(parametros_list[l] == "Temperatura"){
                    Ts_Valor<- rep("", length(momento))
                    CSV <- data.frame(CodigoNacional, momento, Ts_Valor)
                  }else if(parametros_list[l] == "PuntoRocio"){
                    Td_Valor<- rep("", length(momento))
                    CSV <- data.frame(CodigoNacional, momento, Td_Valor)
                  }else if(parametros_list[l] == "Humedad"){
                    HR_Valor<- rep("", length(momento))
                    CSV <- data.frame(CodigoNacional, momento, HR_Valor)
                  }else if(parametros_list[l] == "Viento"){
                    dd_Valor<- rep("", length(momento))
                    ff_Valor<- rep("", length(momento))
                    VRB_Valor<- rep("", length(momento))
                    CSV <- data.frame(CodigoNacional, momento, dd_Valor,ff_Valor, VRB_Valor)
                  }else if(parametros_list[l] == "PresionQFE"){
                    QFE_Valor<- rep("", length(momento))
                    CSV <- data.frame(CodigoNacional, momento, QFE_Valor)
                  }else if(parametros_list[l] == "PresionQFF"){
                    QFF_Valor<- rep("", length(momento))
                    CSV <- data.frame(CodigoNacional, momento, QFF_Valor)
                  }
                }
                df<- rbind(df, CSV)
                suppressWarnings({
                  file.remove(filename)
                  file.remove(csvname)
                })
              }
              if(parametros_list[l] == "Viento"){
                df2 <- data.frame(df[2], df[3], df[4], df[5])
              }else{
                df2 <- data.frame(df[2], df[3])
              }
              setDT(df2)
              data <- data[df2, on = c("date" = "momento")]
              df   <- NULL
              df2  <- NULL
            }
          }
        }
        if(is.null(data_total)){
          data_total<-data
        }else{
          data_total<-rbind(data_total, data)
        }
      }
    }
  }
  return(data_total)
}
