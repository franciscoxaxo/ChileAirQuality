############################### LIBRARIES ######################################
library(DT)
library(shiny)
library(openair)
library(dplyr)
library(plotly)
library(data.table)
library(lubridate)
library(shinycssloaders)

############################ FUNCTIONS #########################################

##ClimateData function
source("https://raw.githubusercontent.com/franciscoxaxo/ChileAirQualityProject/master/R/ChileClimateData.R")
##AirQuality function
source("https://raw.githubusercontent.com/franciscoxaxo/ChileAirQualityProject/master/R/ChileAirQuality.R")

##Complementary statistics functions
source("https://raw.githubusercontent.com/franciscoxaxo/ChileAirQualityProject/master/R/complementaryFunctions.R")

########################### IU #################################################


shinyUI(
  ################################### HEAD #####################################
  fluidPage(
    tags$head(HTML("<title>ChileAirQuality Proyect</title>")),
    
    ################################# CALIDAD DEL AIRE TAB######################
    
    tabsetPanel(tabPanel("Data Calidad del Aire",
                         {
                           titlePanel("Captura de Datos")
                           sidebarLayout(
                             sidebarPanel(
                               #Input start date
                               dateInput("Fecha_inicio",
                                         label ="Fecha de inicio",
                                         value = Sys.Date()-1,
                                         min = "2005-01-01",
                                         max= Sys.Date(),
                                         format= "dd/mm/yyyy"),
                               #Input end date
                               dateInput("Fecha_Termino",
                                         label ="Fecha de Termino",
                                         value = Sys.Date()-1,
                                         min = ("2005-01-01"),
                                         max= Sys.Date(),
                                         format= "dd/mm/yyyy"),
                               #Control option: Curate data
                               checkboxInput("validacion",
                                             label = "Curar datos",
                                             value = TRUE),
                               #Air quality climate factors buttons
                               splitLayout(checkboxGroupInput("F_Climaticos",
                                                              label ="F. Climaticos",
                                                              choices =c("temp", "HR", "wd","ws"),
                                                              selected = c("temp", "HR", "wd","ws")
                               ),
                               #Air quality pollutants buttons
                               checkboxGroupInput("Contaminantes",
                                                  label ="Contaminantes",
                                                  choices =c( "PM10", "PM25", "NO","NO2",
                                                              "NOX","O3","CO")
                               )
                               ),
                               #Air quality stations buttons
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
                               #Action button
                               submitButton("Aplicar Cambios")
                               
                             ),
                             
                             mainPanel(
                               #Deploy air quality data table
                               withSpinner(DT::dataTableOutput("table")),
                               #Download air quality data
                               downloadButton("descargar", label ="Descargar"),
                               #Hide error
                               tags$style(type="text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"
                               )
                             )
                           )
                         }
    ),
    ############################### DATA CLIMATE TAB ###########################
    tabPanel("Data Climatica",
             {
               sidebarLayout(
                 sidebarPanel(
                   #Years range for the data collection
                   sliderInput("rango",
                               "Rango:",
                               min = 1940,
                               max = 2021,
                               value = c(2000,2020)),
                   
                   #Climate data parameters
                   checkboxGroupInput("parametros",
                                      label ="Parametros",
                                      choices =c("Temperatura", "PuntoRocio", "Humedad",
                                                 "Viento", "PresionQFE", "PresionQFF"),
                                      selected = c("Temperatura", "PuntoRocio", "Humedad",
                                                   "Viento", "PresionQFE", "PresionQFF")),
                   #Ad regions button
                   splitLayout(
                     checkboxGroupInput("region1",
                                        label ="Regiones",
                                        choices =c("I","II",
                                                   "III", "IV",
                                                   "V","VI", "VII", "VIII")
                     ),
                     checkboxGroupInput("region2",
                                        label ="",
                                        choices =c("IX","X",
                                                   "XI","XII","XIV",
                                                   "XV", "XVI",
                                                   "RM"))
                   ),
                   #Action button
                   submitButton("Aplicar Cambios")
                   
                   
                 ),
                 
                 #Deploy climate data table
                 mainPanel(
                  withSpinner( DT::dataTableOutput("table1")),
                   #Download button
                   downloadButton("descargar1",label ="Descargar"),
                   #Hide error
                   tags$style(type="text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }")
                 )
               )
             }
    ),
    ############################## GRAFICAS TAB ################################
    tabPanel("Gráficas",
             {
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("sData2"),
                   uiOutput("selectorGraficos"),##In development!!!
                   #Deploy graphic selection bar option
                   #Deploy complementary options
                   uiOutput("moreControls"),
                   #Action button
                   submitButton("Actualizar")
                 ),
                 mainPanel(
                   withSpinner(uiOutput("mainGraphics"))
                   ,
                   #Hide errors
                   tags$style(type="text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"
                   )
                   
                 )
               )
             }
    ),
    ############################# RESUMEN TAB ##################################
    tabPanel("Resumen",
             {
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("sData"), 
                   #Select statistic summary option
                   selectInput("statsummary","Resumen Estadistico",
                               choices = c("--Seleccionar--","Promedio",
                                           "Mediana","Desviacion Estandar",
                                           "coeficiente de variacion")
                   ),
                   
                   #Action bottom
                   submitButton("Aplicar Cambios")
                 ),
                 mainPanel(
                   # Stats table
                   withSpinner(DT::dataTableOutput("statstable")),
                   #Download stats table
                   downloadButton("descargarstats",
                                  label ="Descargar"
                   ),
                   #Hide errors
                   tags$style(type="text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"
                   )
                 )
               )
               
               
             }
    ),
    ############################ INFORMACION TAB ###############################
    tabPanel("Información",
             {
               #Table of variables
               verticalLayout(
                 
                 tags$body(HTML("
                   <h1><strong>Calidad del aire</strong></h1>
                   <h2><strong>Variables de Calidad del aire</strong></h2>
                   <p>Esta aplicación tiene disponible los siguientes
                      parámetros para el análisis de la calidad del
                      aire disponibles del Sistema de Información
                      Nacional de Calidad del Aire. </p>
                   ")),
                 withSpinner(tableOutput("info_2")),
                 tags$body(HTML("
                  <a href = 'https://sinca.mma.gob.cl/'> sinca.mma.gob.cl</a>
                  <h2><strong>Estaciones de monitoreo</strong></h2>    
                  <p>Los datos recopilados por esta aplicación son reportados 
                     por el sistema de información nacional de calidad
                     del aire a partir de la red de estaciones de monitoreo
                     MACAM distribuidas en las comunas de la región metropolitana. </p>"
                 )),

                 
                 withSpinner(plotlyOutput("sitemap",
                              width = "500px",
                              height = "500px"
                 )),
                 tags$body(HTML("
                  <br>
                  <br>
                  <h1><strong>Meteorología</strong></h1>
                  <h2><strong>Variables meteorológicas</strong></h2>
                  <p>Esta aplicación tiene disponible los siguientes
                      parámetros para el análisis meteorológicos 
                      disponibles en la Dirección meteorológica de 
                      Chile.</p>      
                                ")),
                 withSpinner(tableOutput("info_3")),
                 tags$body(HTML("
                  <a href = 'http://www.meteochile.gob.cl/'> www.meteochile.gob.cl</a>
                  <br>
                  <br>
                  <br>
                  <br>"))
                 
               )
             }
    )
    
    )))
