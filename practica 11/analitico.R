
archivo <- read.csv("./bezdekIris.csv")
x <- archivo[, 1]
x
n=length(x)
n

n_clases=10
max_x=max(x)
min_x=min(x)

max_x
min_x

intervalo_clase=(max_x-min_x)/n_clases
intervalo_clase
intervalo_izquierda=c()
intervalo_derecha=c()
frecuencia_clases=c()
for (i in 1:n_clases){
    intervalo_izquierda=c(intervalo_izquierda,min_x+intervalo_clase*(i-1))
      intervalo_derecha=c(intervalo_derecha,min_x+intervalo_clase*i)
    frecuencia_clases=c(frecuencia_clases,0)
}  
intervalo_izquierda
intervalo_derecha
frecuencia_clases

for (j in 1:n)
    for (i in 1:n_clases){
         if ((x[j]>=intervalo_izquierda[i]) && (x[j]<intervalo_derecha[i])) {
            frecuencia_clases[i]=frecuencia_clases[i]+1}
             
    }

frecuencia_clases
barplot(frecuencia_clases)

# Se calculan las fraccones correspondientes a los cuartiles teoricos

fracciones=c()
xdata=c()
for (i in 1:n){
      fraccion=(i-0.5)/n
    fracciones=c(fracciones,fraccion)
      xdata=c(xdata,x[i])

}

fracciones

# se ordena el vector x y se asiga ordenado a la variable y
y=sort(xdata)

#se calcula la media y la desviación estándar de y
media_y=mean(y)
std_y=sd(y)

#se calculan los cuartiles teóricos usando la distribució normal inversa qnorm
q=c()
for (i in 1:n){
      qi=media_y+std_y*qnorm(fracciones[i])
    q=c(q,qi)
}

q
#se grafican los pares ordenados q,y, si es una linea casi recta
#se dice qua los datos originales sí proporcionan evidencias de normalidad
datosQQ=data.frame(q,y)
plot(datosQQ)

#Finalmente se calcula el coeficiente de correlación entre los datos y y los cuantiles q
rQ=cor(q,y)
rQ
#En este ejemplo, de acuerdo a la tabla de valores r*, para 60 datos, la prueba de normalidad
#da evidencia suficiente ya ua para un nivel de confianza de 0.01, rQ es mayor a 0.971
