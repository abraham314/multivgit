## Multidimensional Scaling (MDS)


## the data matrix
X <- matrix(c(10.39, 1.81, 2.30, 
              10.31, 1.74, 2.14, 
              9.93, 1.73, 2.18, 
              10.82, 2.02, 2.70), nrow = 4, byrow = TRUE)

rownames(X) <- c("Argentina", "Australia", "USA", "W.Samoa")
colnames(X) <- c("X100m", "X800m", "marathon")


## center and scale
X <- as.matrix(adata)
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

plot(Xk, xlim = c(-2.5, 2.5), ylim = c(-0.7, 0.7))
text(x = Xk[, 1] -0.7, y = Xk[, 2], rownames(X))




## alternative: 
cmds <- cmdscale(D)
cmds
plot(cmds)