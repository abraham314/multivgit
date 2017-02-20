library(dplyr)
df<-read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data')
summary(df)
df<-rename(df,age=X39,fnlwgt=X77516,education.num=X13,
           capital.gain=X2174,capital.loss=X0,sex=Male,
           hours.per.week=X40 ,
           Country=United.States,education=Bachelors,
           marital.status=Never.married,race=White,workclass=State.gov,
           occupation=Adm.clerical,relationship=Not.in.family,target=X..50K)


dim(df)
str(df)
library(Amelia)
missmap(df,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
df[df==' ?']<-NA #reemplazar los ?'s por missings

missmap(df,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

moda<-function(base){ 
  base<-data.frame(base) 
  for(i in 1:length(base)){
    if(class(base[,i])=="factor"){
      base[,i][is.na(base[,i])] <- names(which.max(table(base[,i])))
    }
  }
  return(base)
}

df<-data.frame(sapply(df,moda))

missmap(df,y.at=c(1),y.labels = c(''),col=c('yellow','black'))


#df<-data.frame(apply(df, 2, function(x){ if (class(x)=="factor"){
#  x[is.na(x)] <- names(which.max(table(x)))}
#  return(x) })) #reemplazar missings de variables nominales por la moda de cada una de ellas
#componentes principales
colnames(df) 

library(dummies)
varnums<-colnames(df[,sapply(df,is.numeric)])
varnoms<-colnames(df[,sapply(df,is.factor)])
pca.test<-dummy.data.frame(df,names=c(varnoms))
pca.train<-df[,sapply(df,is.numeric)]
newdf<-cbind(pca.train,pca.test)

#PCA
prin_comp <- prcomp(newdf, scale. = T) 

names(prin_comp) #variables de la base de componenetes pales.


#outputs the mean of variables

prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

#2. The rotation measure provides the principal component loading. 
#Each column of rotation matrix contains the principal component loading 
#vector. This is the most important measure we should be interested in.

prin_comp$rotation #matriz de rotación P

prin_comp$rotation[1:5,1:4] #primeras 4 componentes
#3. In order to compute the principal component score vector, 
#we don’t need to multiply the loading with data. Rather, 
#the matrix x has the principal component score vectors in a 
#8523 × 44 dimension.
dim(prin_comp$x)

biplot(prin_comp, scale = 0)

#compute variance
pr_var <- prin_comp$sdev^2


#To compute the proportion of variance explained by each component,
#we simply divide the variance by sum of total variance. This results in:
  
  #proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")




mat<-cov(newdf)#matriz de varianzas y covarianzas simétrica 

matx<-cov(newdf[1:2])
eigens <- eigen(matx)
evecs <- eigens$vectors
evecs
P=matrix(c(evecs),2,2)
P
P.t<-t(P)
P.t

eigens$values
D<-matrix(c(eigens$values[1],0,0,eigens$values[2]),2,2)
D
P%*%D%*%P.t #teorema espectral
matx 

x<-as.data.frame(newdf[c(1,6)])
print(ggplot(newdf,aes(x=age.base,y=hours.per.week.base))+geom_point(fill='red'))

x$age.base<-scale(x$age.base)
x$hours.per.week.base<-scale(x$hours.per.week.base)
print(ggplot(x,aes(x=age.base,y=hours.per.week.base))+geom_point(fill='red',color='blue'))


matz<-cov(x)
eigens<-eigen(matz)
evecs <- eigens$vectors
a <- sqrt(eigen(matz)$values)
ends <- evecs %*% diag(a)



plot(hours.per.week.base~age.base,x,bty='l',
     xlab='edad', ylab='horas_semana',
     pch=1,
     ylim=c(-3, 5),
     xlim=c(-2, 4),
     yaxs='i',
     xaxs='i')

library(shape)

Arrows(0,0, ends[1,], ends[2,],col='green')
text(ends[1,], ends[2,],
     c('Second Eigenvector', 'First Eigenvector'),
     pos=c(2,3),col='green')



x<-as.data.frame(newdf[c(1,6)])
x$age.base<-scale(x$age.base)
x$hours.per.week.base<-scale(x$hours.per.week.base)

g <- ggplot(data = x, mapping = aes(x =age.base, y = hours.per.week.base))
g <- g + geom_point(alpha = 1/3)  # alpha b/c of overplotting
g <- g + geom_smooth(method = "lm")  # just for comparison
g <- g + coord_fixed()  # otherwise, the angles of vectors are off
g

corre <- cor(x = x$age.base, y = x$hours.per.week.base, method = "spearman")  # calculate correlation, must be spearman b/c of measurement
matrix <- matrix(c(1, corre, corre, 1), nrow = 2)  # make this into a matrix
eigen <- eigen(matrix)  # calculate eigenvectors and values
eigen


g <- g + stat_ellipse(type='norm')
# add ellipse, though I am not sure which is the adequate type
# as per https://github.com/hadley/ggplot2/blob/master/R/stat-ellipse.R
eigen$slopes[1] <- eigen$vectors[1,1]/eigen$vectors[2,1]  # calc slopes as ratios
eigen$slopes[2] <- eigen$vectors[1,1]/eigen$vectors[1,2]  # calc slopes as ratios
g <- g + geom_abline(intercept = 0, slope = eigen$slopes[1], colour = "green")  # plot pc1
g <- g + geom_abline(intercept = 0, slope = eigen$slopes[2], colour = "red")  # plot pc2
g <- g + geom_segment(x = 0, y = 0, xend = eigen$values[1], yend = eigen$slopes[1] * eigen$values[1], colour = "green", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc1
g <- g + geom_segment(x = 0, y = 0, xend = eigen$values[2], yend = eigen$slopes[2] * eigen$values[2], colour = "red", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc2
# Here come the perpendiculars, from StackExchange answer http://stackoverflow.com/questions/30398908/how-to-drop-a-perpendicular-line-from-each-point-in-a-scatterplot-to-an-eigenv ===
