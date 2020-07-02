Validar_NOX<-function(){
  i = NULL
  for (i in 1:length(dataset$NO)) 
  {
    if((dataset$NO[i]+dataset$NO2[i])>dataset$NOX[i]){
      dataset$NO[i] = ""
      dataset$NO2[i] = ""
      dataset$NOX[i] = ""
      
    }
  }
  return(dataset)
}
