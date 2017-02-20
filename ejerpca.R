ej<-read.csv('ej.csv')
ej$Head.Length<-scale(ej$Head.Length)
ej$Head.Breadth<-scale(ej$Head.Breadth)

matx<-cov(ej)

eig<-eigen(matx)

g<-ggplot(ej,aes(x=Head.Length,y=Head.Breadth ))+geom_point(col='blue')
g



g <- g + stat_ellipse(type='norm')
# add ellipse, though I am not sure which is the adequate type
# as per https://github.com/hadley/ggplot2/blob/master/R/stat-ellipse.R
eig$slopes[1] <- eig$vectors[1,1]/eig$vectors[2,1]  # calc slopes as ratios
eig$slopes[2] <- eig$vectors[1,1]/eig$vectors[1,2]  # calc slopes as ratios
g <- g + geom_abline(intercept = 0, slope = eig$slopes[1], colour = "green")  # plot pc1
g <- g + geom_abline(intercept = 0, slope = eig$slopes[2], colour = "red")  # plot pc2
g <- g + geom_segment(x = 0, y = 0, xend = eig$values[1], yend = eig$slopes[1] * eig$values[1], colour = "green", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc1
g <- g + geom_segment(x = 0, y = 0, xend = eig$values[2], yend = eig$slopes[2] * eig$values[2], colour = "red", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc2


g <- g + geom_segment(x = 0, y = 0, xend = -eig$values[1], yend = -eig$slopes[1] * eig$values[1], colour = "green", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc1
g <- g + geom_segment(x = 0, y = 0, xend = -eig$values[2], yend = -eig$slopes[2] * eig$values[2], colour = "red", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc2
