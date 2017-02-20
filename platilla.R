library(dplyr)
df<-read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data')
summary(df)
dim(df)
str(df)
df[df$X40>10.0 & df$Never.married==" Divorced",]
rename(df,X..50K=X50k)
df[,sapply(df,is.numeric)]
slice(df,1:5)
noms<-df[,sapply(df,is.factor)]
noms[noms==' ?']<-NA
library(Amelia)
missmap(noms,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

table(noms$United.States)
noms[noms=="?"]

rep<-function(x){
  if (x==" ?"){
    b<-NaN
    
  }
  else{
    b<-x
  }
  return(b)
}


df[1,]
Z<-rbind(df[1,],df[2,])

a<-select(df,)

missmap(noms, main=" Missings Map", 
        , legend=FALSE)

ggplot(df,aes(X40))+geom_histogram(bins=30,fill='blue')

moda<-function(x){
  if (is.na(x)){
  w<-names(which.max(table(x))) 
  }
  else{
    w<-x
  }
  return(w)
}

k<-apply(noms, 2, function(x){ 
  x[is.na(x)] <- names(which.max(table(x)))
  return(x) })


missmap(data.frame(k),y.at=c(1),y.labels = c(''),col=c('yellow','black'))
