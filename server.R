library(DT)
library(shiny)
library(openair)
source("https://raw.githubusercontent.com/franciscoxaxo/SINCAspider/master/spider_sinca.R")
#source("https://raw.githubusercontent.com/franciscoxaxo/SINCAspider/master/Validar_NOX.R")
#source("https://raw.githubusercontent.com/franciscoxaxo/SINCAspider/master/Validar_PM.R")

shinyServer(function(input, output) {
    
    data_total<-reactive(spider_sinca(Comunas = c(input$Comunas1, input$Comunas2), Contaminantes = c(input$Contaminantes, input$F_Climaticos), input_fecha_inicio = as.character(input$Fecha_inicio, format("%d/%m/%Y")), input_fecha_termino = as.character(input$Fecha_Termino, format("%d/%m/%Y"))))
    
    try({
        output$table <- DT::renderDataTable(
            DT::datatable({
                data_total()
            },
            filter = "top",
            selection = 'multiple',
            style = 'bootstrap'
            
            )
        )
        output$descargar<-downloadHandler(
            filename = "data.csv",
            content = function(file){
                write.csv(data_total(), file)
            }
        )
    }
    , silent = T)
    
})
