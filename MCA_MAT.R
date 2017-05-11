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





P    <- Z / sum(Z)
cm   <- apply(P, 2, sum)
rm   <- apply(P, 1, sum)
eP   <- rm %*% t(cm)
S    <- (P - eP) / sqrt(eP)
dec  <- svd(S)
lam  <- dec$d[1:7]^2
expl <- 100*(lam / sum(lam))
rbind(round(lam[c(1:4,(J-Q))], 3),round(expl[c(1:4,(J-Q))], 1))



res.mca<-MCA(tea)
re.ca<-CA(Z)