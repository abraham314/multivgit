mat <- matrix(c(2.2, 0.4, 0.4, 2.8), 2, 2)
eigens <- eigen(mat)
evs <- sqrt(eigens$values)
evecs <- eigens$vectors

a <- evs[1]
b <- evs[2]
x0 <- 0
y0 <- 0
alpha <- atan(evecs[ , 1][2] / evecs[ , 1][1])
theta <- seq(0, 2 * pi, length=(1000))

x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)


png("graph.png")
plot(x, y, type = "l", main = expression("x = a cos " * theta * " + " * x[0] * " and y = b sin " * theta * " + " * y[0]), asp = 1)
arrows(0, 0, a * evecs[ , 1][2], a * evecs[ , 1][2])
arrows(0, 0, b * evecs[ , 2][3], b * evecs[ , 2][2])
dev.off()