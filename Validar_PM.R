Validar_PM<-function(dataset, dataset$PM10, dataset$PM25){
  i = NULL
  for (i in 1:length(dataset$PM10)) 
  {
    if(dataset$PM25[i]>dataset$PM10[i]){
      dataset$PM10[i] = ""
      dataset$PM25[i] = ""
      
    }
  }
  return(dataset)
}
