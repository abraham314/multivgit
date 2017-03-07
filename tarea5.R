devtools::install_github("mauriciogtec/metodosMultivariados2017", build_vignettes= TRUE) 
library(metodosMultivariados2017)
senado_votaciones <- readRDS("/home/mb45296/votacionEspacialMexico/source_rds/L63_A2_N1_PO1_RESULTS.RDS")

part<-readRDS('/home/mb45296/votacionEspacialMexico/source_rds/L63_A2_N1_PO1_ASISTENCIA_ASISTENCIA.RDS')

library(Amelia)
missmap(senado_votaciones, main="Votaciones", 
        col=c("yellow", "black"), legend=FALSE)


datana<-is.na(senado_votaciones)
senado_votaciones[datana]<-0

senadores_votaciones<-senado_votaciones[,-3:-1]
## center and scale
X <- as.matrix(senadores_votaciones)
X <- scale(X, center = TRUE, scale = TRUE)
X


## distance matrix
D <- as.matrix(dist(t(X)))
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

plot(Xk, xlim = c(-7, 7), ylim = c(-6, 6.5))
text(x = Xk[, 1] -0.7, y = Xk[, 2], rownames(t(X)))

## alternative: 
cmds <- cmdscale(D)
cmds
plot(cmds)



dist<-dist(t(senadores_votaciones))

dist<-as.matrix(dist)







mds<-cmdscale(dist)


ggplot(as.data.frame(mds), aes(V1, -V2, label = rownames(mds))) +     
  geom_text(check_overlap = TRUE) + theme_minimal() + xlab('') + ylab('') +
       scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)



www$SENADOR<-rownames(www)
firstname = sapply(strsplit(www$SENADOR, ' '), function(x) x[3]) 
lastname1 = sapply(strsplit(www$SENADOR, ' '), function(x) x[1]) 
lastname2 = sapply(strsplit(www$SENADOR, ' '), function(x) x[2])
www$SENADOR<-paste(firstname,lastname1,lastname2,sep=" ")

ggplot(fin,aes(V1,-V2,label=PARTIDO))+geom_point(aes(color=PARTIDO))


pl<-ggplot(fin, aes(x=V1, y=V2,text = paste(SENADOR))) + geom_point(aes(color=PARTIDO)) + 
  scale_color_manual(values=c("blue", "yellow", "green","red","orange","gray","black"))

print(pl)



fin[11,]$PARTIDO<-'PRI'
fin[15,]$PARTIDO<-'PAN'
fin[20,]$PARTIDO<-'PAN'
fin[57,]$PARTIDO<-'PRI'
fin[69,]$PARTIDO<-'PRI'
fin[104,]$PARTIDO<-'PAN'
fin[109,]$PARTIDO<-'PRD'
fin[120,]$PARTIDO<-'PT'
fin[131,]$PARTIDO<-'PRI'


firstname = sapply(strsplit(rownames(mds), ' '), function(x) {if (is.na(x[4])) {return (x[3])} else if(is.na(x[5])){return (paste(x[3],x[4]))}
  else if(is.na(x[6])){return(paste(x[3],x[4],x[5]))} else if(is.na(x[7])){return(paste(x[3],x[4],x[5],x[6]))}})

lastname1 = sapply(strsplit(rownames(mds), ' '), function(x) x[1]) 
lastname2 = sapply(strsplit(rownames(mds), ' '), function(x) x[2])
mds$SENADOR<-paste(firstname,lastname1,lastname2,sep=" ")




