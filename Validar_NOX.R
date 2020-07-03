Validar_NOX<-function(NO, NO2, NOX){
  i = NULL
  for (i in 1:length(NO)) 
  {
    if((NO[i]+NO2[i])>NOX[i]){
      NO[i] = ""
      NO2[i] = ""
      NOX[i] = ""
      
    }
  }
}
