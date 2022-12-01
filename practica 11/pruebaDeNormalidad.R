## AUTHOR: Luis Eduardo Galindo Amaya
##   DATE: 24-11-2022
##   DESC: Prueba de normalidad grafica y analitica

## ENTRADAS
n_clases <- 10
nombre_del_archivo <- "./bezdekIris.csv"

columna <- 3
filas <- 1:150

archivo_salida <- "./practica 11/img/i3.jpeg"

## OPERACIONES GRAFICA ####
archivo <- read.csv(nombre_del_archivo)
data <- archivo[filas, columna]

valor_minimo <- min(data)
valor_maximo <- max(data)
amplitud_de_clase <- (valor_maximo - valor_minimo) / n_clases

## numero de elementos dentro de cada clase
frecuencias <- array(0, dim = (n_clases))

for (i in 1:n_clases) {
  rango_min <- valor_minimo + amplitud_de_clase * (i - 1)
  rango_max <- valor_minimo + amplitud_de_clase * i 
  frecuencias[i] <- sum(data >= rango_min & data < rango_max)
}


## OPERACIONES ANALÍTICA ####
n <- length(data) # tamaño del arreglo

## Se calcula las fracciones las fracciones correspondientes a
## los acuartiles teoricos

fracciones <- c()
xdata <- c()

for (i in 1:n) {
  fraccion <- (i - 0.5) / n
  fracciones <- c(fracciones, fraccion)
  xdata <- c(xdata, data[i])
}

# se ordena el vector x y se asiga ordenado a la variable y
y <- sort(xdata)

# se calcula la media y la desviación estándar de y
media_y <- mean(y)
std_y <- sd(y)

## se calculan los cuartiles teóricos usando la distribució
## normal inversa qnorm

q <- c()
for (i in 1:n) {
  qi <- media_y + std_y * qnorm(fracciones[i])
  q <- c(q, qi)
}

datosQQ=data.frame(q,y)
rQ=cor(q,y)




## grafica
jpeg(file = archivo_salida)
par(mfrow = c(2, 1))
barplot(frecuencias)
plot(datosQQ)
dev.off()

## SALIDAS GRAFICAS ####
frecuencias
n_clases
valor_minimo
valor_maximo
amplitud_de_clase
length(data)
sum(frecuencias)
## SALIDAS ANALÍTICAS ####
rQ

