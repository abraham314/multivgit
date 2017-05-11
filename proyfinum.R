multiv<-read.csv('multivsamp.csv',sep=';') 
library(Amelia)
missmap(multiv, main="Missings", 
        col=c("yellow", "black"), legend=FALSE) 

cmult<-multiv[sapply(multiv, function(x) is.numeric(x))]
missmap(cmult, main="Missings", 
        col=c("yellow", "black"), legend=FALSE) 

ncero<-data.frame(colSums(is.na(cmult)))
cmult<-cmult[rownames(subset(ncero,subset=colSums.is.na.cmult..<100000))] 

missmap(cmult, main="Missings", 
        col=c("yellow", "black"), legend=FALSE) 

datana<-is.na(cmult)
cmult[datana]<-0 

missmap(cmult, main="Missings", 
        col=c("yellow", "black"), legend=FALSE) 

cmult$contrata<-as.factor(cmult$contrata) 

library(corrplot)

a<-c(1,72)
newmult<-cmult[-a]

ncero<-data.frame(colSums(newmult))
newmult<-newmult[rownames(subset(ncero,subset=colSums.newmult.!=0))] 
newmult<-data.frame(scale(newmult) )

missmap(newmult, main="Missings", 
        col=c("yellow", "black"), legend=FALSE) 

mat<-cor(newmult)

corrplot(mat,tl.cex=0.1,method='square')

res.pca = PCA(newmult, scale.unit=FALSE, ncp=5, graph=T)

res.pca
