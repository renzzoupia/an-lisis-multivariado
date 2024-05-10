library(readxl)
diabetess <- read_excel("D:/Universidad/Ciclo7/AnalisisMultivariado/unidad02/diabetess.xlsx")
View(diabetess)

#Matriz de correlacion
round(cor(x = diabetess, method = "pearson"), 3)

library(psych)
multi.hist(x = diabetess, dcol = c("purple", "red"), dlty = c("dotted", "solid"), main = "")
# linea roja es la forma que tiene la distribucion normal
# morado es la forma que tiene la distribucion de datos

#Grafico 2
library(GGally)
ggpairs(diabetess, lower = list(continuous ="smooth"), diag = list(continuous = "barDiag"), axisLabels ="none")

#pasos a seguir 2
modelox = lm(diabetess$BloodPressure ~ diabetess$Pregnancies+diabetess$Glucose+diabetess$SkinThickness + diabetess$Insulin + 
               diabetess$BMI + diabetess$DiabetesPedigreeFunction + diabetess$Age)
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
plot3d(diabetess$BMI, diabetess$Age, diabetess$BloodPressure, pch = ".", size = 0.5)

#la tarea es:
# tenemos 3 datas, trabahar en grupo de articulo
# poner en el repositorio de git
# Realizar un analisis de regresion lineal multiple para cada uno de las datas
# con los pasos que vimos en clases, fecha de entrega viernes 10/05