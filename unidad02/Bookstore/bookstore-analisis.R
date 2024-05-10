library(readxl)
bookstore <- read_excel("D:/Universidad/Ciclo7/AnalisisMultivariado/análisis-multivariado/unidad02/bookstore/bookstore.xlsx")
View(bookstore)

# Seleccionar solo las columnas numéricas
numeric_cols <- sapply(bookstore, is.numeric)
bookstore_numeric <- bookstore[, numeric_cols]
View(bookstore_numeric)
#Matriz de correlacion
round(cor(x = bookstore_numeric, method = "pearson"), 3)

library(psych)
multi.hist(x = bookstore_numeric, dcol = c("purple", "red"), dlty = c("dotted", "solid"), main = "")
# linea roja es la forma que tiene la distribucion normal
# morado es la forma que tiene la distribucion de datos

#Grafico 2
library(GGally)
ggpairs(bookstore_numeric, lower = list(continuous ="smooth"), diag = list(continuous = "barDiag"), axisLabels ="none")

#pasos a seguir 2
modelox = lm(bookstore_numeric$price ~ bookstore_numeric$pages + bookstore_numeric$reviews + bookstore_numeric$n_reviews + bookstore_numeric$star5 + 
               bookstore_numeric$star4 + bookstore_numeric$star3 + bookstore_numeric$star2 + bookstore_numeric$star1 + bookstore_numeric$weight)
#es como ponerle en una bolsa e identificar que variable servira para nuestro modelo

step(object = modelox, direction = "both", trace=1)
#las variables ganadoras q seran utiles para el modelo es: imc , edad, grosor de piel, con el codigo de arriba paso 2

# paso 2 de la siguiente diapositiva
# vamos a ver si existe colinealidad, ya vimos pero porsiacaso
library(car)

vif(modelox)
# el valor que tenemos es no hay de que preocuparnos, esta normal, sale 1 ,  tantos es normal

#paso 3 
# para ver el grafico
library(rgl)
# 3 variables 1y y 2x
plot3d(bookstore_numeric$price, bookstore_numeric$pages, bookstore_numeric$reviews, pch = ".", size = 0.5)
