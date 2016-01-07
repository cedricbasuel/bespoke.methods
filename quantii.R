quantii=function(vecti){
  print(min(vecti,na.rm=T))
  print(mean(vecti,na.rm=T))
  print(max(vecti,na.rm=T))
  print(paste("min","mean","max"))
  print(quantile(vecti, c(0.001,0.005,0.01,.05,.1,.25,0.333,.5,0.6,.75,0.8,.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,.98,.99,0.995,0.998,0.999), na.rm = T))
}
