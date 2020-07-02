Validar_PM<-function(){
  i = NULL
  for (i in 1:length(dataset$PM10)) 
  {
    if((dataset$PM25[i]>dataset$PM10[i]){
      dataset$PM10[i] = ""
      dataset$PM25[i] = ""
      
    }
  }
  return(dataset)
}
