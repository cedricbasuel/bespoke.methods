binning = function(datatable,factor_column){
  if(is.factor(factor_column)){
    addtnl_columns = length(unique(factor_column))
    if(addtnl_columns<20){
      factories = as.character(unique(factor_column))
      prefix_col = readline("Enter desired column prefix: ")
      for(i in 1:addtnl_columns){
        binaries = ifelse(factor_column == factories[i],1,0)
        datatable = cbind(datatable,binaries)
        names(datatable)[ncol(datatable)] = paste(prefix_col,factories[i])
      }
      return(datatable)
    }
    else(return("There's more than 20 factors. Capping could feel too dizzy."))
  }
  else(return("Column is not of type factor"))
}

##
