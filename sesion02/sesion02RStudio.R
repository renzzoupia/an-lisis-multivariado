library(PerformanceAnalytics)

peso=c(51,59,49,54,50,55,48,53,52,57)
largo =c(33.5,38,32,37.5,31.5,33,31,36.5,34,35)
pairs(largo~peso, col="blue")


df = data.frame(peso,largo)
chart.Correlation(df)

# Correlacion de Spearman

shapiro.test(peso)
shapiro.test(largo)

# Usamos R de person por que ambos tiene una distribucion normal

cor(peso, largo)#valor de correlacion
cor.test(peso,largo)      # correlacion positiva y fuerte saca 
                          # correlacion mas nivel de significancia

# R2                              o   r de pearson
# coeficiente de determinacion        coeficiente de correlacion
# 0.000049                            0.7794691^2

# PRACTICA EN CLASE
# edad y talla
edad = c(26,18,20,19,25,22,37,56,78)
talla = c(1.56,1.72,1.65,1.44,1.69,1.66,1.51,1.62,1.42)

pairs(edad~talla, col="blue")

dfedadtalla = data.frame(edad,talla)
chart.Correlation(dfedadtalla)