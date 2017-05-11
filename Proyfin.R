multiv<-read.csv('multivsamp.csv',sep=';') 
library(Amelia)
missmap(multiv, main="Missings", 
        col=c("yellow", "black"), legend=FALSE) 

cmult<-multiv[sapply(multiv, function(x) !is.numeric(x))]
missmap(cmult, main="Missings", 
        col=c("yellow", "black"), legend=FALSE) 

datana<-is.na(cmult)
cmult[datana]<-"" 

cmult<-cmult[,c('CD_ESTADO','CD_SEXO','TP_VIVIENDA_SEPO')]

library(FactoMineR) 
mca1<-MCA(cmult)
mca1$eig
cats = apply(cmult, 2, function(x) nlevels(as.factor(x)))
cats

# Graficamos con Data.Frame
mca1_vars_df = data.frame(mca1$var$coord,Variable = rep(names(cats), cats))
mca1_obs_df = data.frame(mca1$ind$coord)

library(ggplot2)
# Grafica de las categorias de las variables
ggplot(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, 
                                                             colour = "gray70") + geom_text(aes(colour = Variable)) + ggtitle("Grafica de ACM de variables de TEA en R con FactoMineR")


plot(mca1)