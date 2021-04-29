#libraries
library(DT)
library(shiny)
library(openair)
library(dplyr)
library(plotly)
library(data.table)
library(lubridate)
library(shinycssloaders)


#functions
##ClimateData function
source("https://raw.githubusercontent.com/franciscoxaxo/ChileAirQualityProject/master/R/ChileClimateData.R")
##AirQuality function
source("https://raw.githubusercontent.com/franciscoxaxo/ChileAirQualityProject/master/R/ChileAirQuality.R")
##Complementary statistics functions
source("https://raw.githubusercontent.com/franciscoxaxo/ChileAirQualityProject/master/R/complementaryFunctions.R")


shinyServer(function(input, output) {
  
  
  #Reactive function for download air quality information from SINCA
  data_totalAQ<-reactive(
    ChileAirQuality(
      Comunas = c(c(input$Comunas1, input$Comunas2)),
      Parametros = c(input$Contaminantes, input$F_Climaticos),
      fechadeInicio = as.character(
        input$Fecha_inicio,
        format("%d/%m/%Y")
      ),
      fechadeTermino = as.character(
        input$Fecha_Termino,
        format("%d/%m/%Y")
      ),
      Curar = input$validacion
    ))
  
  #reactive function for download climate data from DMC
  data_totalDC <- reactive(
    
    ChileClimateData(
      Estaciones = c(input$region1, input$region2),
      Parametros = input$parametros,
      inicio = input$rango[1],
      fin = input$rango[2],
      Region = TRUE
    ))
  
  #IN DEVELOPMENT!!!
  #  Select dataset
  output$sData <- renderUI({
    selectInput("selectData","Seleccionar dataset",
                choices = c("--Seleccionar--","Data Climatica", "Calidad del aire")
    )
  })
  
  output$sData2 <- renderUI({
    selectInput("selectData2","Seleccionar dataset",
                choices = c("--Seleccionar--","Data Climatica", "Calidad del aire")
    )
    
    
    
  })
  output$selectorGraficos <- renderUI({
    
    suppressWarnings({
      if(input$selectData2 == "Calidad del aire"){
        selectInput("Select","Tipo de Grafico",
                    choices = c("--Seleccionar--","timeVariation","corPlot","timePlot",
                                "calendarPlot","polarPlot","scatterPlot","smoothTrend")
        )
      }
      else if(input$selectData2 == "Data Climatica"){
        selectInput("Select","Tipo de Grafico",
                    choices = c("--Seleccionar--","timeVariation","corPlot","timePlot",
                                "calendarPlot")
        )
      }
      
    })
    
    
    
  })
  
  #Transform controls for meteorological parameters
  parClimaticos <- reactive({comparFunction(input$parametros)})
  
  
  
  
  #Deploy data table for climate data
  output$table1 <- DT::renderDataTable(
    DT::datatable({data_totalDC()},
                  filter = "top",
                  selection = 'multiple',
                  style = 'bootstrap'
    )
  )
  
  #Download Button for climate data
  output$descargar1<-downloadHandler(
    filename = "data.csv",
    content = function(file){
      write.csv(data_totalDC(), file)
    }
  )
  
  
  #Store information of air quality stations
  site<-reactive(ChileAirQuality())
  
  #Map plot of airquality stations
  output$sitemap<-renderPlotly({
    siteplot(
      site()
    )
  })
  
  #IN DEVELOPMENT!!!
  
  #Calculate the stats summary
  stats<-reactive(
    if(input$selectData == "Data Climatica"){
      if(input$statsummary == "Promedio"){
        datamean2(data_totalDC())
      }else if(input$statsummary == "Mediana"){
        datamedian2(data_totalDC())
      }else if(input$statsummary == "Desviacion Estandar"){
        datasd2(data_totalDC())
      }else if(input$statsummary == "coeficiente de variacion"){
        datacv2(data_totalDC())
      }
    }else if(input$selectData == "Calidad del aire"){
      if(input$statsummary == "Promedio"){
        datamean(data_totalAQ())
      }else if(input$statsummary == "Mediana"){
        datamedian(data_totalAQ())
      }else if(input$statsummary == "Desviacion Estandar"){
        datasd(data_totalAQ())
      }else if(input$statsummary == "coeficiente de variacion"){
        datacv(data_totalAQ())
      }
    }
    
    
  )
  
  #Build the DF with  the stats summary
  output$statstable <- DT::renderDataTable(
    DT::datatable({stats()},
                  filter = "top",
                  selection = 'multiple',
                  style = 'bootstrap'
    )
  )
  
  #Download button for stats summary data
  output$descargarstats<-downloadHandler(
    filename = "stats.csv",
    content = function(file){
      write.csv(stats(), file)
    }
  )
  
  
  #Deploy reactive controls option for air quality graphics
  output$moreControls <- renderUI({
    suppressWarnings(
      {
        if(input$selectData2 == "Calidad del aire"){
          ##Options for calendarPlot
          if(input$Select == "calendarPlot"){
            flowLayout(
              sliderInput("rango_calendar",
                          "Rango:",
                          step = 1,
                          min = year(as.Date(input$Fecha_inicio, format = "%d/%m/%Y")),
                          max = year(as.Date(input$Fecha_Termino, format = "%d/%m/%Y")),
                          value = c(year(as.Date(input$Fecha_inicio, format = "%d/%m/%Y")),
                                    year(as.Date(input$Fecha_Termino, format = "%d/%m/%Y")))),
              radioButtons(inputId = "choices",
                           label = "Contaminantes",
                           choices = input$Contaminantes)
            )
          }else if(input$Select == "scatterPlot"){
            ##Options for scatterPlot
            flowLayout(
              splitLayout(radioButtons(inputId = "x",
                                       label = "Contaminantes",
                                       choices = input$Contaminantes),
                          radioButtons(inputId = "y",
                                       label = "Contaminantes",
                                       choices = input$Contaminantes)
              ),
              splitLayout(
                #Logaritm options in the X axis
                checkboxInput(inputId = "logx",label = "log(x)"),
                #Logaritm options in the Y axis
                checkboxInput(inputId ="logy",label = "log(y)"),
                #Trace lineal correlation
                checkboxInput(inputId ="lineal",label = "Lineal")
              )
              
            )
            
          }else if(input$Select == "smoothTrend"){
            flowLayout(checkboxInput("checkSites","Desagrupar por Ciudad"),
                       radioButtons(inputId = "choices",
                                    label = "Contaminantes",
                                    choices = input$Contaminantes)
            )
          }else if(input$Select == "timePlot"){
            #Deploy un-group for station option
            flowLayout(checkboxInput("checkSites","Desagrupar por Ciudad"),
                       selectInput("avgtime","Promedio de tiempo",
                                   choices = c("hour","year","month","week",
                                               "day")),
                       checkboxGroupInput(inputId = "choices",
                                          label = "Contaminantes",
                                          choices = input$Contaminantes,
                                          selected = input$Contaminantes)
                       
            )
            
          }else{
            #Deploy un-group for station option
            flowLayout(checkboxInput("checkSites","Desagrupar por Ciudad"),
                       checkboxGroupInput(inputId = "choices",
                                          label = "Contaminantes",
                                          choices = input$Contaminantes,
                                          selected = input$Contaminantes)
                       
            )
            
          }
        }
        else if(input$selectData2 == "Data Climatica"){
          if(input$Select == "calendarPlot"){
            flowLayout(
              sliderInput("rango_calendar",
                          "Rango:",
                          step = 1,
                          min = year(as.Date(input$Fecha_inicio, format = "%d/%m/%Y")),
                          max = year(as.Date(input$Fecha_Termino, format = "%d/%m/%Y")),
                          value = c(year(as.Date(input$Fecha_inicio, format = "%d/%m/%Y")),
                                    year(as.Date(input$Fecha_Termino, format = "%d/%m/%Y")))),
              radioButtons(inputId = "choices",
                           label = "Parametros",
                           choices = parClimaticos())
            )
          }else if(input$Select == "smoothTrend"){
            flowLayout(checkboxInput("checkSites","Desagrupar por Ciudad"),
                       radioButtons(inputId = "choices",
                                    label = "Parametros",
                                    choices = parClimaticos())
            )
          }else if(input$Select == "timePlot"){
            #Deploy un-group for station option
            flowLayout(checkboxInput("checkSites","Desagrupar por Ciudad"),
                       selectInput("avgtime","Promedio de tiempo",
                                   choices = c("hour","year","month","week",
                                               "day")),
                       checkboxGroupInput(inputId = "choices",
                                          label = "Parametros",
                                          choices = parClimaticos(),
                                          selected = parClimaticos())
                       
            )
            
          }else{
            #Deploy un-group for station option
            flowLayout(checkboxInput("checkSites","Desagrupar por Ciudad"),
                       checkboxGroupInput(inputId = "Parameterchoices",
                                          label = "Parametros",
                                          choices = parClimaticos(),
                                          selected = parClimaticos())
                       
            )
          }
        }
        
      }
    )
    
  })
  
  #Data Table for air quality information
  output$table <- DT::renderDataTable(
    DT::datatable({data_totalAQ()},
                  filter = "top",
                  selection = 'multiple',
                  style = 'bootstrap'
    )
  )
  #Download Botton for air quality data
  output$descargar<-downloadHandler(
    filename = "data.csv",
    content = function(file){
      write.csv(data_totalAQ(), file)
    }
  )
  
  output$grafico<-renderPlot({
    suppressWarnings(
      {
        if(input$selectData2 == "Calidad del aire"){
          if(input$checkSites){
            if(input$Select=="timeVariation")
            {
              #deploy timeVariation of air quality data un-group by site
              timeVariation(data_totalAQ(),
                            pollutant = input$choices,
                            type = "site")
            }
            else if(input$Select=="corPlot"){
              #deploy corPlot of air quality data un-group by site
              corPlot(data_totalAQ(),
                      pollutant = c(input$choices,input$F_Climaticos),
                      type = "site")
            }
            else if(input$Select=="timePlot"){
              #deploy timePlot of air quality data un-group by site
              timePlot(data_totalAQ(),
                       pollutant = input$choices,
                       type = "site",
                       avg.time = input$avgtime)
            }
            else if(input$Select=="polarPlot"){
              #deploy polarPlot of air quality data un-group by site
              polarPlot(data_totalAQ(),
                        pollutant = input$choices,
                        type = "site")
            }
            else if(input$Select=="calendarPlot"){
              #deploy timePlot of air quality data un-group by site
              calendarPlot(data_totalAQ(),
                           pollutant = input$choices,
                           type = "site",
                           year = as.numeric(input$rango_calendar[1]):as.numeric(input$rango_calendar[2]))
            }
            else if(input$Select=="scatterPlot"){
              #deploy scatterPlot of air quality data
              scatterPlot(data_totalAQ(),
                          x = input$x,
                          y= input$y,
                          x.log = input$logx,
                          y.log = input$logy,
                          linear = input$lineal)
            }
            else if(input$Select=="smoothTrend"){
              smoothTrend(data_totalAQ(),
                          pollutant = input$choices,
                          type = "site")
            }
          }else{
            if(input$Select=="timeVariation"){
              #deploy timeVariation of air quality data
              timeVariation(data_totalAQ(), pollutant = input$choices)
            }
            else if(input$Select=="corPlot"){
              #deploy corPlot of air quality data
              corPlot(data_totalAQ(), pollutant = c(input$choices,input$F_Climaticos))
            }
            else if(input$Select=="timePlot"){
              #deploy timePlot of air quality data un-group by site
              timePlot(data_totalAQ(), pollutant = input$choices, type = "site",
                       avg.time = input$avgtime)
            }
            else if(input$Select=="polarPlot"){
              #deploy polarPlot of air quality data un-group by site
              polarPlot(data_totalAQ(), pollutant = input$choices)
            }
            else if(input$Select=="calendarPlot"){
              #deploy calendarPlot of air quality data un-group by site
              calendarPlot(data_totalAQ(), pollutant = input$choices, year = as.numeric(input$rango_calendar[1]):as.numeric(input$rango_calendar[2]))
            }
            else if(input$Select=="scatterPlot"){
              #deploy scatterPlot of air quality data un-group by site
              scatterPlot(data_totalAQ(),
                          x = input$x,
                          y= input$y,
                          log.x = input$logx,
                          log.y = input$logy,
                          linear = input$lineal)
            }
            else if(input$Select=="smoothTrend"){
              smoothTrend(data_totalAQ(), pollutant = input$choices)
              
            }
          }
        }
        else if (input$selectData2 == "Data Climatica"){
          if(input$checkSites){
            if(input$Select=="timeVariation"){
              #deploy timeVariation of air quality data un-group by site
              timeVariation(data_totalDC(),
                            pollutant = input$choices, type = "Nombre")
            }
            else if(input$Select=="corPlot"){
              #deploy corPlot of air quality data un-group by site
              corPlot(data_totalDC(),
                      pollutant = input$choices
                      ,type = "Nombre")
            }
            else if(input$Select=="timePlot"){
              #deploy timePlot of air quality data un-group by site
              timePlot(data_totalDC(),
                       pollutant = input$choices
                       ,type = "Nombre",
                       avg.time = input$avgtime)
            }
            else if(input$Select=="calendarPlot"){
              #deploy timePlot of air quality data un-group by site
              calendarPlot(data_totalDC(),
                           pollutant = input$choices
                           ,type = "Nombre", year = as.numeric(input$rango_calendar[1]):as.numeric(input$rango_calendar[2]))
            }
            else if(input$Select=="smoothTrend"){
              smoothTrend(data_totalDC(),
                          pollutant = input$choices
                          ,type = "Nombre")
            }
            else{
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }")
            }
          }else{
            if(input$Select=="timeVariation"){
              #deploy timeVariation of air quality data
              timeVariation(data_totalDC(), pollutant = input$choices)
            }
            else if(input$Select=="corPlot"){
              #deploy corPlot of air quality data
              corPlot(data_totalDC(), pollutant = input$choices)
            }
            else if(input$Select=="timePlot"){
              #deploy timePlot of air quality data un-group by site
              timePlot(data_totalDC(), pollutant = input$choices, avg.time = input$avgtime)
            }
            else if(input$Select=="calendarPlot"){
              #deploy calendarPlot of air quality data un-group by site
              calendarPlot(data_totalDC(), pollutant = input$choices, year = as.numeric(input$rango_calendar[1]):as.numeric(input$rango_calendar[2]))
            }
            else if(input$Select=="smoothTrend"){
              smoothTrend(data_totalDC(), pollutant = input$choices)
            }
            
          }
        }
      }
    )
  })
  
  
  #Graphics options descriptions
  output$text<-renderUI({
    #Time Variation description
    suppressWarnings({
      if(input$Select=="timeVariation"){
        tags$body(
          tags$h2("timeVariation"),
          tags$p("La función timeVariation produce cuatro gráficos: variación del día
                de la semana, variación de la hora media del día y un gráfico combinado
                de hora del día - día de la semana y un gráfico mensual. También se muestra
                en los gráficos el intervalo de confianza del 95% en la media.")
        )
        
        
      }
      else if(input$Select=="corPlot"){
        #Corplot description
        tags$body(
          tags$h2("corPlot"),
          tags$p("El corPlot muestra la correlación codificada de tres
                       formas: por forma (elipses), color y valor numérico. Las
                       elipses se pueden considerar como representaciones
                       visuales de un diagrama de dispersión. Con una correlación
                       positiva perfecta se traza una línea a 45 grados de
                       pendiente positiva. Para una correlación cero, la forma
                       se convierte en un círculo.")
        )
      }
      else if(input$Select=="timePlot"){
        #timePlot description
        tags$body(
          tags$h2("timePlot"),
          tags$p("La función timePlot permite trazar rápidamente
                       series de tiempo de datos para varios contaminantes
                       o variables. Trazara series de tiempo de datos
                       de alta resolución por hora.")
        )
      }
      else if(input$Select=="polarPlot"){
        #polarPlot description
        tags$body(
          tags$h2("polarPlot"),
          tags$p("La función polarPlot traza una gráfica polar bivariada 
               de concentraciones. Se muestra como las concentraciones varían
               según la velocidad y la dirección del viento.")
        )
      }
      else if(input$Select=="calendarPlot"){
        #Calendarplot
        tags$body(
          tags$h2("calendarPlot"),
          tags$p("La función calendarPlot proporciona una forma eficaz de
                       visualizar los datos de esta manera al mostrar las
                       concentraciones diarias en formato de calendario. La
                       concentración de una especie se muestra por su color.")
        )
      }
      else if(input$Select=="scatterPlot"){
        #scatterPlot description
        tags$body(
          tags$h2("scatterPlot"),
          tags$p("Los gráficos de dispersión son extremadamente útiles y una
                       técnica de análisis muy utilizada para considerar como las
                       variables se relacionan entre sí."
          )
        )
        
      }
      else if(input$Select=="smoothTrend"){
        #Corplot description
        tags$body(
          tags$h2("smoothTrend"),
          tags$p("La función smoothTrend proporciona una forma flexible de estimar 
               la tendencia en la concentración de un contaminante u otra variable.
               Los valores medios mensuales se calculan a partir de una serie de 
               tiempo horaria (o de mayor resolución) o diaria. ")
        )
      }
      
    })
  })
  
  
  #Air quality variables
  Variable<-isolate(c("PM10", "PM25","NOX","NO","NO2","O3", "CO", "temp", "ws", "wd", "HR"))
  #Air quality variables full names
  Nombre<-isolate(c("Material Particulado Respirable menor a 10 micras",
                    "Material Particulado Respirable menor a 2,5 micras",
                    "Oxido de Nitrógeno",
                    "Monóxido de Nitrógeno",
                    "Dióxido de Nitrógeno",
                    "Ozono troposférico",
                    "Monóxido de carbono",
                    "Temperatura",
                    "Velocidad del Viento",
                    "Dirección del viento",
                    "Humedad Relativa"))
  #Units
  Unidades<-isolate(c("ug/m3N","ug/m3N","ppb","ppb","ppb","ppb","ppb","°C","m/s","°","%"))
  
  #data frame with the variables descriptions
  output$info_2 <- renderTable({
    data.frame(Variable, Nombre, Unidades)
  })
  
  Parametros <- isolate(c("Ts", "Td", "HR", "QFF", "QFE", "dd", "ff", "VRB")) 
  Descripcion <- isolate(c("Temperatura de aire seco",
                           "Temperatura de punto de rocio",
                           "Humedad relativa", 
                           "Presion en el entorno aeronáutico",
                           "Presion a nivel del mar", 
                           "Direccion del viento",
                           "Velocidad del viento", 
                           "Variabilidad del viento"))
  Unidad <- isolate(c("°C", "°C", "%", "Pa", "Pa", "°", "m/s", "Boolean"))
  output$info_3 <- renderTable({
    data.frame(Parametros, Descripcion, Unidad)
  })
})
