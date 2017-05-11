Pz    <- Z / sum(Z)
cmz   <- apply(Pz, 2, sum)
rmz   <- apply(Pz, 1, sum)
ePz   <- rmz %*% t(cmz)
Sz    <- (Pz - ePz) / sqrt(ePz)
decz  <- svd(Sz)
lamz  <- decz$d[1:7]^2
explz <- 100*(lamz / sum(lamz))
rbind(round(lam[c(1:4,(J-Q))], 3),round(expl[c(1:4,(J-Q))], 1))


Drivr<-solve(diag(sqrt(rmz)))
Drivc<-solve(diag(sqrt(cmz)))

F<-Drivr%*%decz$u%*%decz$d  
G<-Drivc%*%decz$v%*%decz$d  


F1<-F[,1]
G2<-G[,2]

datt <- data.frame(Gc1, 
                   Gc2,
                   row_names)
ggplot(datt, aes(x = Gc1, y= Gc2, colour = row_names)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_text(aes(x = Gc1, y= Gc2, label = row_names))







B<-teab
P.2     <- B / sum(B)
cm.2    <- apply(P.2, 2, sum)
eP.2    <- cm.2 %*% t(cm.2)
S.2     <- (P.2 - eP.2) / sqrt(eP.2)
dec.2   <- eigen(S.2)
delt.2  <- dec.2$values[1:7]
expl.2  <- 100*(delt.2 / sum(delt.2))
lam.2   <- delt.2^2
expl.2b <- 100*(lam.2 / sum(lam.2))
rbind(round(lam.2, 3),round(expl.2b, 1))[,c(1:4,16)]

Driv<-solve(diag(sqrt(cm.2)))
Gc<-Driv%*%dec.2$vectors%*%diag(dec.2$values)

 Gc1<-Gc[,1]
 Gc2<-Gc[,2]

datt <- data.frame(Gc1, 
                   Gc2,
                   row_names)
ggplot(datt, aes(x = Gc1, y= Gc2, colour = row_names)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_text(aes(x = Gc1, y= Gc2, label = row_names))