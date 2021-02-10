siteplot<-function(data, latitud = data$Latitud, longitud = data$Longitud, centro = c(-70.6, -33.4)){
  library("ggplot2")
  library("plotly")
  library("dplyr")
  
  fig<-plot_ly(data,
               lat = latitud,
               lon = longitud,
               marker = list(color = "fuchsia"),
               hovertext = ~paste("Estacion:", data$Estacion,"<br />", "Site:", data$Ciudad), 
               type = 'scattermapbox'
  )
  fig<-fig %>%
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom =9,
        center = list(lon = centro[1], lat = centro[2])
      )
    ) 
  return(fig)
}
