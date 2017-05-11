library(FactoMineR)
tea <- read.table("ej1.csv",sep = ',',header = TRUE)
tea<-tea[-1]
tea$Años<-as.factor(tea$Años)
summary(tea)
teab<-burt(tea)
res.mca<-MCA(tea)
plot(res.mca,invisible=c("var","quali.sup"),cex=0.7)
plot(res.mca,invisible=c("ind","quali.sup"))
plot(res.mca,invisible="quali.sup")
plot(res.mca,invisible="ind")
plot(res.mca,invisible=c("ind","Alto"))

round(res.mca$eig,2)
lapply(dimdesc(res.mca),lapply,round,4)
lapply(dimdesc(res.mca),lapply,signif,3)

plotellipses(res.mca,keepvar='all')

res.mca <- MCA(tea, graph=FALSE)
new.data <- cbind.data.frame(tea[,1],res.mca$ind$coord)
res.pca <- PCA(new.data,quali.sup=1,scale=FALSE,graph=FALSE)
res.pca$eig[1:5,]=res.mca$eig[1:5,]
concat.data <- cbind.data.frame(tea[,11],res.mca$ind$coord)
ellipse.coord <- coord.ellipse(concat.data,bary=TRUE)
plot.PCA(res.pca, habillage=1, ellipse=ellipse.coord, cex=0.8,label="none")