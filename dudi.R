"as.dudi" <- function (df, col.w, row.w, scannf, nf, call, type, tol = 1e-07,
                       full = FALSE)
{
  if (!is.data.frame(df))
    stop("data.frame expected")
  lig <- nrow(df)
  col <- ncol(df)
  if (length(col.w) != col)
    stop("Non convenient col weights")
  if (length(row.w) != lig)
    stop("Non convenient row weights")
  if (any(col.w < 0))
    stop("col weight < 0")
  if (any(row.w < 0))
    stop("row weight < 0")
  if (full)
    scannf <- FALSE
  transpose <- FALSE
  if(lig<col)
    transpose <- TRUE
  res <- list(tab = df, cw = col.w, lw = row.w)
  df <- as.matrix(df)
  df.ori <- df
  df <- df * sqrt(row.w)
  df <- sweep(df, 2, sqrt(col.w), "*")
  if(!transpose){
    df <- crossprod(df,df) 
  }
  else{
    df <- tcrossprod(df,df)
  }
  eig1 <- eigen(df,symmetric=TRUE)
  eig <- eig1$values
  rank <- sum((eig/eig[1]) > tol)
  if (scannf) {
    if (exists("ade4TkGUIFlag")) {
      nf <- ade4TkGUI::chooseaxes(eig, rank)
    }
    else {
      barplot(eig[1:rank])
      cat("Select the number of axes: ")
      nf <- as.integer(readLines(n = 1))
    }
  }
  if (nf <= 0)
    nf <- 2
  if (nf > rank)
    nf <- rank
  if (full)
    nf <- rank
  res$eig <- eig[1:rank]
  res$rank <- rank
  res$nf <- nf
  col.w[which(col.w == 0)] <- 1
  row.w[which(row.w == 0)] <- 1
  dval <- sqrt(res$eig)[1:nf]
  if(!transpose){
    col.w <- 1/sqrt(col.w)
    auxi <- eig1$vectors[, 1:nf] * col.w
    auxi2 <- sweep(df.ori, 2, res$cw, "*")
    auxi2 <- data.frame(auxi2%*%auxi)
    auxi <- data.frame(auxi)
    
    names(auxi) <- paste("CS", (1:nf), sep = "")
    row.names(auxi) <- make.unique(names(res$tab))
    res$c1 <- auxi
    
    names(auxi2) <- paste("Axis", (1:nf), sep = "")
    row.names(auxi2) <- row.names(res$tab)
    res$li <- auxi2
    
    res$co <- sweep(res$c1,2,dval,"*")
    names(res$co) <- paste("Comp", (1:nf), sep = "")
    
    res$l1 <- sweep(res$li,2,dval,"/")
    names(res$l1) <- paste("RS", (1:nf), sep = "")
    
    
  } else {
    row.w <- 1/sqrt(row.w)
    auxi <- eig1$vectors[, 1:nf] * row.w
    auxi2 <- t(sweep(df.ori,1,res$lw,"*"))
    auxi2 <- data.frame(auxi2%*%auxi)
    auxi <- data.frame(auxi)
    
    names(auxi) <- paste("RS", (1:nf), sep = "")
    row.names(auxi) <- row.names(res$tab)
    res$l1 <- auxi
    
    names(auxi2) <- paste("Comp", (1:nf), sep = "")
    row.names(auxi2) <- make.unique(names(res$tab))
    res$co <- auxi2
    
    res$li <- sweep(res$l1,2,dval,"*")
    names(res$li) <- paste("Axis", (1:nf), sep = "")
    
    res$c1 <- sweep(res$co,2,dval,"/")
    names(res$c1) <- paste("CS", (1:nf), sep = "")
    
  }
  
  res$call <- call
  class(res) <- c(type, "dudi")
  return(res)
}


"is.dudi" <- function (x) {
  inherits(x, "dudi")
}

"print.dudi" <- function (x, ...) {
  cat("Duality diagramm\n")
  cat("class: ")
  cat(class(x))
  cat("\n$call: ")
  print(x$call)
  cat("\n$nf:", x$nf, "axis-components saved")
  cat("\n$rank: ")
  cat(x$rank)
  cat("\neigen values: ")
  l0 <- length(x$eig)
  cat(signif(x$eig, 4)[1:(min(5, l0))])
  if (l0 > 5) 
    cat(" ...\n")
  else cat("\n")
  sumry <- array("", c(3, 4), list(1:3, c("vector", "length", 
                                          "mode", "content")))
  sumry[1, ] <- c("$cw", length(x$cw), mode(x$cw), "column weights")
  sumry[2, ] <- c("$lw", length(x$lw), mode(x$lw), "row weights")
  sumry[3, ] <- c("$eig", length(x$eig), mode(x$eig), "eigen values")
  
  print(sumry, quote = FALSE)
  cat("\n")
  sumry <- array("", c(5, 4), list(1:5, c("data.frame", "nrow", 
                                          "ncol", "content")))
  sumry[1, ] <- c("$tab", nrow(x$tab), ncol(x$tab), "modified array")
  sumry[2, ] <- c("$li", nrow(x$li), ncol(x$li), "row coordinates")
  sumry[3, ] <- c("$l1", nrow(x$l1), ncol(x$l1), "row normed scores")
  sumry[4, ] <- c("$co", nrow(x$co), ncol(x$co), "column coordinates")
  sumry[5, ] <- c("$c1", nrow(x$c1), ncol(x$c1), "column normed scores")
  
  print(sumry, quote = FALSE)
  cat("other elements: ")
  if (length(names(x)) > 11) 
    cat(names(x)[12:(length(x))], "\n")
  else cat("NULL\n")
}

"t.dudi" <- function (x) {
  if (!inherits(x, "dudi")) 
    stop("Object of class 'dudi' expected")
  res <- list()
  res$tab <- data.frame(t(x$tab))
  res$cw <- x$lw
  res$lw <- x$cw
  res$eig <- x$eig
  res$rank <- x$rank
  res$nf <- x$nf
  res$c1 <- x$l1
  res$l1 <- x$c1
  res$co <- x$li
  res$li <- x$co
  res$call <- match.call()
  class(res) <- c("transpo", "dudi")
  return(res)
}

"dudi.acm" <- function (df, row.w = rep(1, nrow(df)), scannf = TRUE, nf = 2) {
  if (!all(unlist(lapply(df, is.factor)))) 
    stop("All variables must be factors")
  df <- as.data.frame(df)
  X <- acm.disjonctif(df)
  lig <- nrow(X)
  col <- ncol(X)
  var <- ncol(df)
  if (length(row.w) != lig) 
    stop("Non convenient row weights")
  if (any(row.w < 0)) 
    stop("row weight < 0")
  row.w <- row.w/sum(row.w)
  col.w <- apply(X, 2, function(x) sum(x*row.w))
  if (any(col.w == 0)) 
    stop("One category with null weight")
  X <- t(t(X)/col.w) - 1
  col.w <- col.w/var
  X <- as.dudi(data.frame(X), col.w, row.w, scannf = scannf, 
               nf = nf, call = match.call(), type = "acm")
  rcor <- matrix(0, ncol(df), X$nf)
  rcor <- row(rcor) + 0 + (0+1i) * col(rcor)
  floc <- function(x) {
    i <- Re(x)
    j <- Im(x)
    x <- X$l1[, j] * X$lw
    qual <- df[, i]
    poicla <- unlist(tapply(X$lw, qual, sum))
    z <- unlist(tapply(x, qual, sum))/poicla
    return(sum(poicla * z * z))
  }
  rcor <- apply(rcor, c(1, 2), floc)
  rcor <- data.frame(rcor)
  row.names(rcor) <- names(df)
  names(rcor) <- names(X$l1)
  X$cr <- rcor
  return(X)
}

"boxplot.acm" <- function (x, xax = 1, ...) {
  # correction d'un bug par P. Cornillon 29/10/2004
  if (!inherits(x, "acm")) 
    stop("Object of class 'acm' expected")
  if ((xax < 1) || (xax > x$nf)) 
    stop("non convenient axe number")
  def.par <- par(no.readonly = TRUE)
  on.exit(par(def.par))
  oritab <- eval.parent(as.list(x$call)[[2]])
  nvar <- ncol(oritab)
  if (nvar <= 7) 
    sco.boxplot(x$l1[, xax], oritab[, 1:nvar], clabel = 1)
  else if (nvar <= 14) {
    par(mfrow = c(1, 2))
    sco.boxplot(x$l1[, xax], oritab[, 1:(nvar%/%2)], clabel = 1.3)
    sco.boxplot(x$l1[, xax], oritab[, (nvar%/%2 + 1):nvar], 
                clabel = 1.3)
  }
  else {
    par(mfrow = c(1, 3))
    if ((a0 <- nvar%/%3) < nvar/3) 
      a0 <- a0 + 1
    sco.boxplot(x$l1[, xax], oritab[, 1:a0], clabel = 1.6)
    sco.boxplot(x$l1[, xax], oritab[, (a0 + 1):(2 * a0)], 
                clabel = 1.6)
    sco.boxplot(x$l1[, xax], oritab[, (2 * a0 + 1):nvar], 
                clabel = 1.6)
  }
}

"acm.burt" <- function (df1, df2, counts = rep(1, nrow(df1))) {
  if (!all(unlist(lapply(df1, is.factor)))) 
    stop("All variables must be factors")
  if (!all(unlist(lapply(df2, is.factor)))) 
    stop("All variables must be factors")
  if (nrow(df1) != nrow(df2)) 
    stop("non convenient row numbers")
  if (length(counts) != nrow(df2)) 
    stop("non convenient row numbers")
  g1 <- acm.disjonctif(df1)
  g1 <- g1 * counts
  g2 <- acm.disjonctif(df2)
  burt <- as.matrix(t(g1)) %*% as.matrix(g2)
  burt <- data.frame(burt)
  names(burt) <- names(g2)
  row.names(burt) <- names(g1)
  return(burt)
} 

"acm.disjonctif" <- function (df) {
  acm.util.df <- function(i) {
    cl <- df[,i]
    cha <- names(df)[i] 
    n <- length(cl)
    cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1:n) + n * (unclass(cl) - 1)] <- 1
    dimnames(x) <- list(row.names(df), paste(cha,levels(cl),sep="."))
    return(x)
  }
  G <- lapply(1:ncol(df), acm.util.df)
  G <- data.frame (G, check.names = FALSE)
  return(G)
}


fac2disj<- function(fac, drop = FALSE) {
  ## Returns the disjunctive table corrseponding to a factor
  n <- length(fac)
  fac <- as.factor(fac)
  if(drop)
    fac <- factor(fac)
  x <- matrix(0, n, nlevels(fac))
  x[(1:n) + n * (unclass(fac) - 1)] <- 1
  dimnames(x) <- list(names(fac), as.character(levels(fac)))
  return(data.frame(x, check.names = FALSE))
}