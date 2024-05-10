library(readxl)
beisbol <- read_excel("D:/Universidad/Ciclo7/AnalisisMultivariado/análisis-multivariado/unidad02/beisbol/beisbol.xlsx")
View(beisbol)

# Seleccionar solo las columnas numéricas
numeric_cols <- sapply(beisbol, is.numeric)
beisbol_numeric <- beisbol[, numeric_cols]
View(beisbol_numeric)
#Matriz de correlacion
round(cor(x = beisbol_numeric, method = "pearson"), 3)
#Si hay colinealidad, mayores a 0.7

library(psych)
multi.hist(x = beisbol_numeric, dcol = c("purple", "red"), dlty = c("dotted", "solid"), main = "")
# linea roja es la forma que tiene la distribucion normal
# morado es la forma que tiene la distribucion de datos

#Grafico 2
library(GGally) #defino que salgan las 8 primeras columnas
ggpairs(beisbol_numeric[, 1:8], lower = list(continuous ="smooth"), diag = list(continuous = "barDiag"), axisLabels ="none")

#pasos a seguir 2
modelox = lm(beisbol_numeric$Points ~ beisbol_numeric$Age + beisbol_numeric$`Minutes Played` + 
             beisbol_numeric$`Fields Goal` + beisbol_numeric$`3-points Field Goal` + beisbol_numeric$`3-points Field Goal` + 
             beisbol_numeric$Assists)
#es como ponerle en una bolsa e identificar que variable servira para nuestro modelo

step(object = modelox, direction = "both", trace=1)
#las variables ganadoras q seran utiles para el modelo es: imc , edad, grosor de piel, con el codigo de arriba paso 2

# paso 2 de la siguiente diapositiva
# vamos a ver si existe colinealidad, ya vimos pero porsiacaso
library(car)
vif(modelox)
# el valor que tenemos puede verse afectada por cierta colinialidad

#paso 3 
# para ver el grafico
library(rgl)
# 3 variables 1y y 2x
plot3d(beisbol_numeric$`Minutes Played`, beisbol_numeric$`Fields Goal`, 
       beisbol_numeric$Points, pch = ".", size = 0.5)
