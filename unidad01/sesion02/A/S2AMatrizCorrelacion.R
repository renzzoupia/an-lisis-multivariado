library(readr)
library(corrplot)
library(RColorBrewer) #para definir colores
mtcars = read_delim("D:/Universidad/Ciclo7/AnalisisMultivariado/análisis-multivariado/sesion02/A/mtcars.csv", 
                            ",", escape_double = FALSE, trim_ws = TRUE) #insertar datos separados por ,
View(mtcars)
str(mtcars) #ver que tipo de variable son las columnas
mtcars_numeric <- mtcars[, sapply(mtcars, is.numeric)] #excluir caracteres y solo admitir
                                                        #numeros
#Ejecutar matriz de correlaciones
my_colors <- colorRampPalette(c("red", "white", "violet"))(200) #definimos los colores
# Ejecutar Matriz de correlacion
M <- cor(mtcars_numeric)
corrplot(M, method = "ellipse", col = my_colors) # Elipse
corrplot(M, method = "circle", col = my_colors) # Circulo
corrplot(M, method = "square", col = my_colors) #Cuadrado
corrplot(M, method = "number", col = my_colors) #Numerico
corrplot(M, method = "shade", col = my_colors) #Sombra
corrplot(M, method = "color", col = my_colors) # Color
corrplot(M, method = "pie", col = my_colors) #Pai o pie
