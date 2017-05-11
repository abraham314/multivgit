devtools::install_github("mauriciogtec/metodosMultivariados2017", build_vignettes= TRUE) 
library(metodosMultivariados2017)
load("/home/mb45296/R/x86_64-pc-linux-gnu-library/3.3/metodosMultivariados2017/data/senado_votacion.rda")
senado_votaciones <-senado_votacion 
load("/home/mb45296/R/x86_64-pc-linux-gnu-library/3.3/metodosMultivariados2017/data/senado_partidos.rda")

#eliminamos los renglones informativos
senado<-senado_votaciones[,-3:-1]
#guardamos el vector de asuntos para hacerlos columnas
colt<-senado_votaciones$ASUNTO
#genramos el dataframe transpuesto
trnew<-as.data.frame(t(senado))
#nombres de las columnas relacionados con asunto
colnames(trnew)<-colt



library(Amelia)
missmap(trnew, main="Votaciones", 
        col=c("yellow", "black"), legend=FALSE) 
datana<-is.na(trnew)
trnew[datana]<-0


## center and scale
X <- as.matrix(trnew)
X <- scale(X, center = TRUE, scale = TRUE)
X


## distance matrix
D <- as.matrix(dist(X))
## squared distance
D2 <- D ^2
D2

## mean center matrix
n <- nrow(D2)
ones <- rep(1, n)
C <- diag(1, n) - (1 / n) * ones %*% t(ones)
C

## double center the (squared) distance matrix
B <- - (1 / 2)*C %*% D2 %*% C
B

## find the eigen decomposition of B
eigen_decomposition <- eigen(B)
## eigenvectors
U <- eigen_decomposition$vectors
U
## and eigenvalues
G2 <- diag(eigen_decomposition$values) 
G2

## compose X with eigen values and vectors
Xstar <- U %*% (G2 ^ (0.5))
Xstar


## reduce dimensions
k <- 2

Xk <- U[, 1:k] %*% (G2 ^ 0.5)[1:k, 1:k]
Xk

plot(Xk, xlim = c(-7.5, 11.5), ylim = c(-6, 10))
text(x = Xk[, 1] -0.7, y = Xk[, 2], rownames(t(X)))

## alternativo: 
cmds <- cmdscale(D)
cmds
plot(cmds)

#varianza explicada
(G2[1,1]+G2[2,2])/sum(diag(G2))

mds<-as.data.frame(cmds)
mds$SENADOR<-rownames(cmds)
fin<-merge(x = mds, y = senado_partidos, by = "SENADOR", all.x = TRUE)

pl<-ggplot(fin, aes(x=V1, y=V2,text = paste(SENADOR))) + geom_point(aes(color=PARTIDO)) + 
  scale_color_manual(values=c("blue", "yellow", "green","red","orange","gray","black"))+
  geom_vline(xintercept = 0)+geom_hline(yintercept = 0)

print(pl)


