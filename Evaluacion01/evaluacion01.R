library(beeswarm)
library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(ggplot2)
library(stringr)
library(corrplot)
library(RColorBrewer) #para definir colores
library(PerformanceAnalytics)

dataMurderRates = read_delim("D:/Universidad/Ciclo7/AnalisisMultivariado/Evaluacion01/MurderRates.csv", 
                             ",", escape_double = FALSE, trim_ws = TRUE) #insertar datos separados por ,
View(dataMurderRates)


dim(dataMurderRates) #mostrar filas y columnas
str(dataMurderRates) #tipo de variable del dato
names(dataMurderRates) #nombres de las variables
summary(dataMurderRates) #descripción de los datos

attach(dataMurderRates)

summary(rate)
mean(rate)

#2
library(PerformanceAnalytics)

pairs(rate~noncauc, col="blue")


df = data.frame(rate,noncauc)
chart.Correlation(df)

# Correlacion de Spearman

shapiro.test(rate)
shapiro.test(noncauc)

# Usamos R de person por que ambos tiene una distribucion normal

cor(rate, noncauc)#valor de correlacion
cor.test(rate,noncauc)   #FALTA IDENTIFICARLO

#3
str(dataMurderRates) #ver que tipo de variable son las columnas
murderrates_numeric <- dataMurderRates[, sapply(dataMurderRates, is.numeric)] #excluir caracteres y solo admitir
                                                                              #numeros
#Ejecutar matriz de correlaciones
my_colors <- colorRampPalette(c("red", "black", "violet"))(200) #definimos los colores
# Ejecutar Matriz de correlacion
M <- cor(murderrates_numeric)
corrplot(M, method = "ellipse", col = my_colors) # Con ellipses
corrplot(M, method = "number", col = my_colors) # Con números para entenderlo mejor

#4
# Cuando hay valores atipicos, como se muestra en el boxplot, se observan puntos, en nosotros no tenemos 

boxplot(rate, noncauc, names=c("tasa de asesinatos", "no caucasicas"))

medias = c(mean(rate),mean(noncauc)) 

points(medias,pch=18,col="purple") 

par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 2)) 
qqnorm(rate, xlab = "", ylab = "", main = "tasa de asesinatos") 
qqline(rate) 
qqnorm(noncauc, xlab = "", ylab = "", main = "no caucasicas") 
qqline(noncauc)


##### Prueba t para tasa de asesinatos y los estados del sur
t_test_result <- t.test(rate ~ southern, data = dataMurderRates, var.equal = FALSE)
print(t_test_result)
t.test(dataMurderRates$rate, dataMurderRates$noncauc, paired = TRUE)

