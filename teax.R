library(FactoMineR)
tea <- read.table("http://factominer.free.fr/book/tea.csv",header=TRUE,sep=";")
summary(tea)
res.mca<-MCA(tea, quanti.sup=22, quali.sup=c(19:21,23:36))
plot(res.mca,invisible=c("var","quali.sup"),cex=0.7)
plot(res.mca,invisible=c("ind","quali.sup"))
plot(res.mca,invisible="quali.sup")
plot(res.mca,invisible="ind")
plot(res.mca,invisible=c("ind","var"))

round(res.mca$eig,2)
lapply(dimdesc(res.mca),lapply,round,4)
lapply(dimdesc(res.mca),lapply,signif,3)

plotellipses(res.mca,keepvar=c("relaxant","restaurant","profession","place.of.purchase"))

res.mca <- MCA(tea, quanti.sup=22, quali.sup=c(19:21,23:36), graph=FALSE)
new.data <- cbind.data.frame(tea[,11],res.mca$ind$coord)
res.pca <- PCA(new.data,quali.sup=1,scale=FALSE,graph=FALSE)
res.pca$eig[1:5,]=res.mca$eig[1:5,]
concat.data <- cbind.data.frame(tea[,11],res.mca$ind$coord)
ellipse.coord <- coord.ellipse(concat.data,bary=TRUE)
plot.PCA(res.pca, habillage=1, ellipse=ellipse.coord, cex=0.8,label="none")

catdes(tea, num.var = 18)