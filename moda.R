moda<-function(base){ 
   base<-data.frame(base) 
  for(i in 1:length(base)){
    if(class(base[,i])=="factor"){
      base[,i][is.na(base[,i])] <- names(which.max(table(base[,i])))
    }
  }
  return(base)
}




df<-sapply(df,moda)

