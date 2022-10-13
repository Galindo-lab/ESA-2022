archivo <- read.csv("wdbc.data")

## se toman los primeros 300 renglones de las columnas 2 a 7
## como datos conocidos y la columna 8 como los datos para hacer
## ajuste de la recta con la función lm

renglon <- 1:300 # se genera una variable con rango del 1 a 300
x <- archivo[1:300, 3:7]
y <- archivo[1:300, 8]

## par(mfrow = c(3, 1))

datos_grafica <- data.frame(renglon, y) # datos del atributo 7
datos <- data.frame(x, y) # datos para la regresión
datos

## se grafican todos los datos de la columna 7
## plot(datos_grafica)

regresion <- lm(datos)


b <- regresion$coefficients[1]
m <- regresion$coefficients[2]
b
m

## se evalúa la ecuación de la recta ajustada a los primeros 300
## renglones de datos
yr <- m * renglon + b
yr
datos_r <- data.frame(renglon, yr)
datos_r

## se grafica la recta ajustada a los primeros 300 renglones de
## datos
## plot(datos_r, type = "l")

## se toman los renglones  301 a 500 (200 renglones) para hacer
## la predicción con la ecuación de la recta
xpred <- archivo[301:500, 2:6]
renglon <- 1:200

## se evalúa la ecuación de la recta ajustada a los datos de
## predicción
yp <- m * renglon + b
yp
datos_p <- data.frame(renglon, yp)
datos_p
## plot(datos_p, type = "l")




jpeg(file = "./practica 6/img/rp2.jpeg")
par(mfrow = c(2, 2))
plot(datos_grafica)
plot(datos_r, type = "l")
plot(datos_p, type = "l")
dev.off()
