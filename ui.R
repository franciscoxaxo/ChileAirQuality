library(DT)
library(shiny)
library(openair)
source("https://raw.githubusercontent.com/franciscoxaxo/SINCAspider/master/spider_sinca.R")
#source("https://raw.githubusercontent.com/franciscoxaxo/SINCAspider/master/Validar_NOX.R")
#source("https://raw.githubusercontent.com/franciscoxaxo/SINCAspider/master/Validar_PM.R")


# Define UI for application that draws a histogram
shinyUI(fluidPage(tabsetPanel(
    tabPanel("Capturar Datos",
             {
                 titlePanel("Captura de Datos")
                 sidebarLayout(
                     sidebarPanel(
                         dateInput("Fecha_inicio",
                                   label ="Fecha de inicio",
                                   value = Sys.Date(),
                                   min = "01-01-2005",
                                   max= Sys.Date(),
                                   format= "dd/mm/yyyy"),
                         dateInput("Fecha_Termino",
                                   label ="Fecha de Termino",
                                   value = Sys.Date(),
                                   min = ("01-01-2005"),
                                   max= Sys.Date(),
                                   format= "dd/mm/yyyy"),
                         splitLayout(checkboxGroupInput("F_Climaticos",
                                                        label ="F. Climaticos",
                                                        choices =c("Temperatura", "HR", "WD","WS")),
                                     checkboxGroupInput("Contaminantes",
                                                        label ="Contaminantes",
                                                        choices =c( "PM10", "PM25", "NO","NO2","NOX","O3","CO"))
                         ),
                         splitLayout(checkboxGroupInput("Comunas1",
                                                        label ="Estaciones",
                                                        choices =c("CE","CEi","CN","EB","IN","LF")),
                                     checkboxGroupInput("Comunas2",
                                                        label ="",
                                                        choices =c("LC","PU","PA","QU","QUi","SA"))
                         ),
                         
                         submitButton("Aplicar Cambios")
                     ),

                     mainPanel(
                         try(dataTableOutput("table"), silent = T),
                         downloadButton("descargar", label ="Descargar")
                         
                     )
                 )
            
        }
        
    ),
    tabPanel("Graficas",
             {
                 sidebarLayout(
                     sidebarPanel(
                         titlePanel("Graficos de Calidad del Aire"),
                         selectInput("OpenAir", label= "Escoja el tipo de grafica",
                         choices = c("timeplot", "Corplot", "TimeVariation", "WindRose","PolarPlot")),
                         splitLayout(checkboxGroupInput("F_Climaticos_Graficos",
                                                        label ="F. Climaticos",
                                                        choices =c("Temperatura", "HR", "WD","WS")),
                                     checkboxGroupInput("Contaminantes_Grafico",
                                                        label ="Contaminantes",
                                                        choices =c( "PM10", "PM25", "NO","NO2","NOX","O3","CO")),

                         submitButton("Aplicar Cambios")
                         )
                     ),
                     mainPanel()
                 )
             }
        
    )
    
    
    
)

))
