
archivo <- read.csv("bezdekIris.csv")

clase <- archivo[, 5]

cantidad_datos <- length(clase)

esperado <- c(1)
conteo_clases <- 1
for (i in 2:cantidad_datos) {
  if (clase[i] != clase[i - 1]) {
    conteo_clases <- conteo_clases + 1
    esperado <- c(esperado, 1)
  }
}


por_muestra <- 0.5 
cant_muestra <- trunc(cantidad_datos * por_muestra)

indice_muestra <- sample(1:cantidad_datos, cant_muestra)
clase_muestra <- clase[indice_muestra]
esperado <- esperado * trunc(cant_muestra / conteo_clases)
observado <- (1:conteo_clases) * 0

for (i in 1:cant_muestra) {
  clase_contada <- clase_muestra[i]
  observado[clase_contada] <- observado[clase_contada] + 1
}


tabla <- data.frame(esperado, observado)
tabla

chisq.test(tabla)
