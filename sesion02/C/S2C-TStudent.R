# Independientes 

EA = c(2, 3, 1, 4, 2, 3, 2, 1, 3, 2) 

EB = c(3, 2, 1, 3, 2, 2, 4, 2, 3, 1) 



# Cuando hay valores atipicos, como se muestra en el boxplot, se observan puntos, en nosotros no tenemos 

boxplot(EA, EB, names=c("EA", "EB")) 

medias = c(mean(EA),mean(EB)) 

points(medias,pch=18,col="purple") 



par(mar = c(2, 2, 2, 2)) 
par(mfrow = c(1, 2)) 
qqnorm(EA, xlab = "", ylab = "", main = "EA") 
qqline(EA) 
qqnorm(EB, xlab = "", ylab = "", main = "EB") 
qqline(EB) 



##### Prueba t 

t.test(EA, EB, paired = FALSE) 



# P VALOR SALE = 1 -> No hay diferencia significativa mayor a 0.05, las varianza son iguales 

#ejercicio 2 

#Dependientes 

PuntajeAntesTerapia = c(7, 6, 5, 6, 7) 

PuntajeDespuesTerapia = c(8, 7, 8, 8, 9) 



# Cuando hay valores atipicos, como se muestra en el boxplot, se observan puntos, en nosotros no tenemos 

boxplot(PuntajeAntesTerapia, PuntajeDespuesTerapia, names=c("Puntaje Antes Terapia", "Puntaje Despues Terapia")) 



mediasTerapia = c(mean(PuntajeAntesTerapia),mean(PuntajeDespuesTerapia)) 

points(mediasTerapia,pch=18,col="purple") 



par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 2))
qqnorm(PuntajeAntesTerapia, xlab = "", ylab = "", main = "Puntaje Antes Terapia") 
qqline(PuntajeAntesTerapia)
qqnorm(PuntajeDespuesTerapia, xlab = "", ylab = "", main = "Puntaje Despues Terapia") 
qqline(PuntajeDespuesTerapia)

##### Prueba t 

t.test(PuntajeAntesTerapia, PuntajeDespuesTerapia, paired = TRUE)
# P VALOR ES 0.008581, -> Si hay diferencia es menor a 0.05 y 
# la varianza no son iguales



