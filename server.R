library(DT)
library(shiny)
library(openair)
source("https://raw.githubusercontent.com/franciscoxaxo/SINCAspider/master/ChileAirQuality.R")


shinyServer(function(input, output) {
    
    
    data_total<-reactive(ChileAirQuality(Comunas = c(c(input$Comunas1, input$Comunas2)), Contaminantes = c(input$Contaminantes, input$F_Climaticos), input_fecha_inicio = as.character(input$Fecha_inicio, format("%d/%m/%Y")), input_fecha_termino = as.character(input$Fecha_Termino, format("%d/%m/%Y"))))
    
    output$info <- DT::renderDataTable(
        DT::datatable({ChileAirQuality(Comunas ="INFO", Contaminantes = "PM10",input_fecha_inicio = "01/01/2020", input_fecha_termino = "01/01/2020")}
                      ))
    
    output$moreControls <- renderUI({
        if(input$Select == "calendarPlot"){
            radioButtons(inputId = "choices", label = "Contaminantes", choices = input$Contaminantes)
        }else if(input$Select == "scatterPlot"){
            radioButtons(inputId = "x", label = "Contaminantes", choices = input$Contaminantes)
            radioButtons(inputId = "y", label = "Contaminantes", choices = input$Contaminantes)
            
        
    }else{
            checkboxGroupInput(inputId = "choices", label = "Contaminantes", choices = input$Contaminantes, selected = input$Contaminantes)
        }
    })

    output$table <- DT::renderDataTable(
        DT::datatable({data_total()},
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

    
    
    output$grafico<-renderPlot({
        if(input$checkSites){
            if(input$Select=="timeVariation")
            {
                timeVariation(data_total(), pollutant = input$choices, type = "site")
            }else if(input$Select=="corPlot")
            {
                corPlot(data_total(), pollutant = c(input$choices,input$F_Climaticos), type = "site")
            }else if(input$Select=="timePlot")
            {
                timePlot(data_total(), pollutant = input$choices, type = "site")
            }else if(input$Select=="polarPlot")
            {
                polarPlot(data_total(), pollutant = input$choices, type = "site")
            }else if(input$Select=="calendarPlot")
            {
                calendarPlot(data_total(), pollutant = input$choices, type = "site")
            }else if(input$Select=="scatterPlot"){
                scatterPlot(x = input$x, y= input$y)
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
                scatterPlot(x = input$x, y= input$y)
            }
        }
    })
    
    

})
