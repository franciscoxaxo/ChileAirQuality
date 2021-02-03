siteplot<-function(data){
  library("ggplot2")
  library("plotly")
  library("dplyr")
  
  fig<-plot_ly(data,
               lat = data$Latitud,
               lon = data$Longitud,
               marker = list(color = "fuchsia"),
               hovertext = ~paste("Estacion:", data$Estacion,"<br />", "Site:", data$Ciudad), 
               type = 'scattermapbox'
  )
  fig<-fig %>%
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom =9,
        center = list(lon = -70.6, lat = -33.4)
      )
    ) 
  return(fig)
}


