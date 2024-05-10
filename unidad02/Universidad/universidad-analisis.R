library(readxl)
universidad <- read_excel("D:/Universidad/Ciclo7/AnalisisMultivariado/análisis-multivariado/unidad02/universidad/universidad.xlsx")
View(universidad)

# Seleccionar solo las columnas numéricas
numeric_cols <- sapply(universidad, is.numeric)
universidad_numeric <- universidad[, numeric_cols]
View(universidad_numeric)
#Matriz de correlacion
round(cor(x = universidad_numeric, method = "pearson"), 3)

library(psych)
multi.hist(x = universidad_numeric, dcol = c("purple", "red"), dlty = c("dotted", "solid"), main = "")
# linea roja es la forma que tiene la distribucion normal
# morado es la forma que tiene la distribucion de datos

#Grafico 2
library(GGally)
ggpairs(universidad_numeric, lower = list(continuous ="smooth"), diag = list(continuous = "barDiag"), axisLabels ="none")

#pasos a seguir 2
modelox = lm(universidad$Founded_year ~ universidad$UK_rank+universidad$World_rank+universidad$score
             +universidad$Minimum_IELTS_score+universidad$fees+universidad$Student_satisfaction+universidad$`Estimated_cost_of_living_per_year_(in_pounds)`+universidad$Latitude)
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
plot3d(universidad$score , universidad$World_rank, universidad$Founded_year, pch = ".", size = 0.5)
