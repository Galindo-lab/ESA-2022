## AUTHOR: Luis Eduardo Galindo Amaya
## DATE:   21-10-2022

## ENTRADAS 
n_clases <- 30
nombre_del_archivo <- "./bezdekIris.csv"
## nombre_del_archivo <- "./practica 10/test.csv"
## salida <- "./practica 10/img/iris1.jpeg"
columna <- 1

## OPERACIONES
archivo <- read.csv(nombre_del_archivo)
data <- archivo[, columna]

valor_minimo <- min(data)
valor_maximo <- max(data)
amplitud_de_clase <- (valor_maximo - valor_minimo) / n_clases

## numero de elementos dentro de cada clase
frecuencias <- array(0, dim = (n_clases))

for (i in 1:n_clases) {    
  rango_min <- valor_minimo + amplitud_de_clase * (i - 1)
  rango_max <- valor_minimo + amplitud_de_clase * i + 0.00001
  frecuencias[i] <- sum(data >= rango_min & data < rango_max)
}

## SALIDAS
n_clases
valor_minimo
valor_maximo
amplitud_de_clase
length(data)
sum(frecuencias)
frecuencias

## grafica
## jpeg(file = salida)
barplot(frecuencias)
## dev.off()
