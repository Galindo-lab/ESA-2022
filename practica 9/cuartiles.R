
archivo <- read.csv("Real estate valuation data set.csv")
x <- archivo[,4]


## cuartil de interes
cuartil_de_interes <- 0.50

## ordenar los datos
x <- sort(x)

## cuartil
c <- cuartil_de_interes * length(x)

# check fractional part
Ccuartil <- if (c %% 1 == 0) {
  ## formula para enteros
  (x[c] + x[c + 1]) / 2
} else {
  ## formula para flotantes
  x[floor(c) + 1]
}

Ccuartil
