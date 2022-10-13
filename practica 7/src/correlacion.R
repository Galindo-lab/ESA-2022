
archivo <- read.csv("wdbc.data")

instante <- 1:100
x <- archivo[1:100, 3]
y <- archivo[1:100, 4]
n <- length(instante)

datos_x <- data.frame(instante, x)
datos_y <- data.frame(instante, y)

jpeg(file = "./practica 7/img/corelacion_inversa.jpeg")
par(mfrow = c(2, 1))
plot(datos_x, type = "b")
plot(datos_y, type = "b")
dev.off()

sum_xy <- 0
sum_x <- 0
sum_y <- 0
sum_xx <- 0
sum_yy <- 0


for (i in instante) {
  sum_xy <- sum_xy + x[i] * y[i]
  sum_x <- sum_x + x[i]
  sum_y <- sum_y + y[i]
  sum_xx <- sum_xx + x[i]^2
  sum_yy <- sum_yy + y[i]^2
}

Sxy <- sum_xy - (sum_x * sum_y / n)
Sxx <- sum_xx - (((sum_x)^2) / n)
Syy <- sum_yy - (((sum_y)^2) / n)
r <- Sxy / (sqrt(Sxx) * sqrt(Syy))
r

