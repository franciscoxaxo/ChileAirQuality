library(DT)
library(shiny)
library(openair)
library(dplyr)
library(plotly)
source("https://raw.githubusercontent.com/franciscoxaxo/ChileAirQualityProject/master/ChileAirQuality.R")
source("https://raw.githubusercontent.com/franciscoxaxo/ChileAirQualityProject/master/siteplot.R")
source("https://raw.githubusercontent.com/franciscoxaxo/ChileAirQualityProject/master/datamean.R")

library(data.table)

shinyServer(function(input, output) {
  
  
  #Funcion de data, pilar del trabajo
  
  data_total<-reactive(ChileAirQuality(Comunas = c(c(input$Comunas1, input$Comunas2)),
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
  
  
  site<-reactive(ChileAirQuality())
  
  output$sitemap<-renderPlotly({
    siteplot(
      site()
    )
  })
  
  stats<-reactive(
    if(input$statsummary=="Promedio"){
      datamean(data_total())
    }else if(input$statsummary=="Mediana"){
      datamedian(data_total())
    }else if(input$statsummary=="Desviacion Estandar"){
      datasd(data_total())
    }else if(input$statsummary=="coeficiente de variacion"){
      datacv(data_total())
    }
  )
  
  output$statstable <- DT::renderDataTable(
    DT::datatable({stats()},
                  filter = "top",
                  selection = 'multiple',
                  style = 'bootstrap'
    )
  )
  #Boton de descarga
  output$descargarstats<-downloadHandler(
    filename = "stats.csv",
    content = function(file){
      write.csv(stats(), file)
    }
  )
  
  
  #Controles Reactivos#
  output$moreControls <- renderUI({
    if(input$Select == "calendarPlot"){
      flowLayout(
        #sliderInput(inputId = "year",min = substr(as.character(as.character(input$Fecha_inicio), 7, 10)), max = substr(as.character(as.character(input$Fecha_Termino), 7, 10))),
        radioButtons(inputId = "choices",
                     label = "Contaminantes",
                     choices = input$Contaminantes)
      )
    }else if(input$Select == "scatterPlot"){
      flowLayout(
        
        splitLayout(radioButtons(inputId = "x",
                                 label = "Contaminantes",
                                 choices = input$Contaminantes),
                    radioButtons(inputId = "y",
                                 label = "Contaminantes",
                                 choices = input$Contaminantes)
        ),
        splitLayout(checkboxInput(inputId = "logx",label = "log(x)"),
                    checkboxInput(inputId ="logy",label = "log(y)"),
                    checkboxInput(inputId ="lineal",label = "Lineal")
        )
        
      )
      
    }else{
      flowLayout(checkboxInput("checkSites","Desagrupar por Ciudad"),
                 checkboxGroupInput(inputId = "choices",
                                    label = "Contaminantes",
                                    choices = input$Contaminantes,
                                    selected = input$Contaminantes)
                 
      )
      
    }
  })
  
  #Tabla de exposicion de datos
  
  output$table <- DT::renderDataTable(
    DT::datatable({data_total()},
                  filter = "top",
                  selection = 'multiple',
                  style = 'bootstrap'
    )
  )
  #Boton de descarga
  output$descargar<-downloadHandler(
    filename = "data.csv",
    content = function(file){
      write.csv(data_total(), file)
    }
  )
  
  output$grafico<-renderPlot({
    if(input$checkSites){
      if(input$Select=="timeVariation")
      {
        timeVariation(data_total(),
                      pollutant = input$choices,
                      type = "site")
      }else if(input$Select=="corPlot")
      {
        corPlot(data_total(),
                pollutant = c(input$choices,input$F_Climaticos),
                type = "site")
      }else if(input$Select=="timePlot")
      {
        timePlot(data_total(),
                 pollutant = input$choices,
                 type = "site")
      }else if(input$Select=="polarPlot")
      {
        polarPlot(data_total(),
                  pollutant = input$choices,
                  type = "site")
      }else if(input$Select=="calendarPlot")
      {
        calendarPlot(data_total(),
                     pollutant = input$choices, 
                     type = "site")
      }else if(input$Select=="scatterPlot"){
        scatterPlot(data_total(),
                    x = input$x,
                    y= input$y,
                    x.log = input$logx,
                    y.log = input$logy,
                    linear = input$lineal)
      }
    }else{
      if(input$Select=="timeVariation")
      {
        timeVariation(data_total(), pollutant = input$choices)
      }else if(input$Select=="corPlot")
      {
        corPlot(data_total(), pollutant = c(input$choices,input$F_Climaticos))
      }else if(input$Select=="timePlot")
      {
        timePlot(data_total(), pollutant = input$choices, type = "site")
      }else if(input$Select=="polarPlot")
      {
        polarPlot(data_total(), pollutant = input$choices)
      }else if(input$Select=="calendarPlot")
      {
        calendarPlot(data_total(), pollutant = input$choices)
      }else if(input$Select=="scatterPlot"){
        scatterPlot(data_total(),
                    x = input$x, 
                    y= input$y,
                    log.x = input$logx,
                    log.y = input$logy,
                    linear = input$lineal)
      }
    }
  })
  
  output$text<-renderUI({
    if(input$Select=="timeVariation")
    {
      tags$body(
        tags$h2("timeVariation"),
        tags$p("La funcion timeVariation produce cuatro graficos: variacion del dia
                de la semana, variacion de la hora media del dia y un grafico combinado 
                de hora del dia - dia de la semana y un grafico mensual.Tambien se muestra
                en los graficos el intervalo de confianza del 95% en la media.")
      )
      
      
    }else if(input$Select=="corPlot")
    {
      tags$body(
        tags$h2("corPlot"),
        tags$p("El corPlot muestra la correlacion codificada de tres 
                       formas: por forma (elipses), color y valor numerico. Las
                       elipses se pueden considerar como representaciones 
                       visuales de un diagrama de dispersion. Con una correlacion
                       positiva perfecta se traza una linea a 45 grados de
                       pendiente positiva. Para una correlacion cero, la forma 
                       se convierte en un circulo.")
      )
    }else if(input$Select=="timePlot")
    {
      tags$body(
        tags$h2("timePlot"),
        tags$p("La funcion timePlot permite trazar rapidamente
                       series de tiempo de datos para varios contaminantes
                       o variables. Trazara series de tiempo de datos
                       de alta resolucion por hora.")
      )
    }else if(input$Select=="polarPlot")
    {
      tags$body(
        tags$h2("polarPlot"),
        tags$p("La funcion polarPlot traza una grafica polar bivariada de
                       concentraciones. Se muestra como las concentraciones varian 
                       segun la velocidad y la direccion del viento.")
      )
    }else if(input$Select=="calendarPlot")
    {
      tags$body(
        tags$h2("calendarPlot"),
        tags$p("La funcion calendarPlot proporciona una forma eficaz de 
                       visualizar los datos de esta manera al mostrar las 
                       concentraciones diarias en formato de calendario. La
                       concentracion de una especie se muestra por su color.")
      )
    }else if(input$Select=="scatterPlot"){
      tags$body(
        tags$h2("scatterPlot"),
        tags$p("Los graficos de dispersion son extremadamente utiles y una
                       tecnica de analisis muy utilizada para considerar como las 
                       variables se relacionan entre si.")
      )
      
    }
    
  })
  
  #Tabla de comunas
  
  output$info <- renderTable(
    {ChileAirQuality()}
  )
  
  #Tabla de variables: Contaminantes y factores meteorologicos
  
  Variable<-isolate(c("PM10", "PM25","NOX","NO","NO2","O3", "CO", "temp", "ws", "wd", "HR"))
  Nombre<-isolate(c("Material Particulado Respirable menor a 10 micras",
                    "Material Particulado Respirable menor a 2,5 micras",
                    "Oxido de Nitrogeno",
                    "Monoxido de Nitrogeno",
                    "Dioxido de Nitrogeno",
                    "Ozono troposferico",
                    "Monoxido de carbono",
                    "Temperatura",
                    "Velocidad del Viento",
                    "Direccion del viento",
                    "Humedad Relativa"
                    
  ))
  
  Unidades<-isolate(c("ug/m3N","ug/m3N","ppb","ppb","ppb","ppb","ppb","C","m/s","","%"))
  
  
  output$info_2 <- renderTable({
    data.frame(Variable, Nombre, Unidades)
    
  })
  
})
