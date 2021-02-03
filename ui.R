library(DT)
library(shiny)
library(openair)
source("https://raw.githubusercontent.com/franciscoxaxo/ChileAirQualityProject/master/ChileAirQuality.R")
source("https://raw.githubusercontent.com/franciscoxaxo/ChileAirQualityProject/master/siteplot.R")



shinyUI(
    
    fluidPage(
        tags$head(HTML("<title>ChileAirQuality Proyect</title>")),
        tabsetPanel(tabPanel("Capturar Datos",
             {
                 titlePanel("Captura de Datos")
                 sidebarLayout(
                     sidebarPanel(
                         dateInput("Fecha_inicio",
                                   label ="Fecha de inicio",
                                   value = Sys.Date()-1,
                                   min = "2005-01-01",
                                   max= Sys.Date(),
                                   format= "dd/mm/yyyy"),
                         dateInput("Fecha_Termino",
                                   label ="Fecha de Termino",
                                   value = Sys.Date()-1,
                                   min = ("2005-01-01"),
                                   max= Sys.Date(),
                                   format= "dd/mm/yyyy"),
                         checkboxInput("validacion", 
                                       label = "Curar datos",
                                       value = TRUE),
                         splitLayout(checkboxGroupInput("F_Climaticos",
                                                        label ="F. Climaticos",
                                                        choices =c("temp", "HR", "wd","ws"), 
                                                        selected = c("temp", "HR", "wd","ws")
                                                        ),
                                     checkboxGroupInput("Contaminantes",
                                                        label ="Contaminantes",
                                                        choices =c( "PM10", "PM25", "NO","NO2",
                                                                    "NOX","O3","CO")
                                                        )
                         ),
                         splitLayout(checkboxGroupInput("Comunas1",
                                                        label ="Estaciones",
                                                        choices =c("P. O'Higgins","Cerrillos 1",
                                                                   "Cerrillos", "Cerro Navia",
                                                                   "El Bosque","Independecia")
                                                        ),
                                     checkboxGroupInput("Comunas2",
                                                        label ="",
                                                        choices =c("Las Condes","La Florida",
                                                                   "Pudahuel","Puente Alto","Quilicura",
                                                                   "Quilicura 1", "Coyhaique I",
                                                                   "Coyhaique II")
                                                        )
                                     ),
        
                         submitButton("Aplicar Cambios")
                         
                     ),

                     mainPanel(
                         
                         dataTableOutput("table"),
                         downloadButton("descargar",
                                        label ="Descargar"
                                        ),
                         
                         tags$style(type="text/css",
                                    ".shiny-output-error { visibility: hidden; }",
                                    ".shiny-output-error:before { visibility: hidden; }"
                         )
                     )
                 )
        }
    ),
    tabPanel("Graficas",
             {
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("Select","Tipo de Grafico",
                                     choices = c("--Seleccionar--","timeVariation",
                                                 "corPlot","timePlot", "calendarPlot",
                                                 "polarPlot","scatterPlot")
                                     ),
                         uiOutput("moreControls"),
                         submitButton("Aplicar Cambios")
                     ),
                     mainPanel(
                         verticalLayout(
                             plotOutput("grafico"),
                             uiOutput("text")
                         ),
                         
                         tags$style(type="text/css",
                                    ".shiny-output-error { visibility: hidden; }",
                                    ".shiny-output-error:before { visibility: hidden; }"
                         )
                         
                     )
                 )
             }
    ),
    tabPanel("Estaciones",
             {
                 flowLayout(plotlyOutput("sitemap",
                                         width = "800px",
                                         height = "800px"
                                         )
                            )
             }
    ),
    tabPanel("Variables",
             {
                 verticalLayout(tableOutput("info_2"),
                                actionLink("sinca", "sinca.mma.gob.cl")
                 )
             }
    )
)))
