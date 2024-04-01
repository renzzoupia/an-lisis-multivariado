library(beeswarm)
library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(ggplot2)
library(stringr)

resultados2014 = read_delim("D:/Universidad/Ciclo7/AnalisisMultivariado/análisis-multivariado/sesion01/data/resultados2014.csv", 
                            ",", escape_double = FALSE, trim_ws = TRUE) #insertar datos separados por ,

head(resultados2014) #mostrar datos de resultados2014

resultados2018 = read_delim("D:/Universidad/Ciclo7/AnalisisMultivariado/análisis-multivariado/sesion01/data/resultados2018.csv", 
                            ",", escape_double = FALSE, trim_ws = TRUE) #insertar datos separados por ,
head(resultados2018) #mostrar datos de resultados2018

dim(resultados2014) #mostrar filas y columnas
str(resultados2014) #tipo de variable del dato
names(resultados2014) #nombres de las variables
summary(resultados2014) #descripción de los datos

#Actualizar los nombres de las columnas con el 
#nombre de los partidos politicos

# forma lenta
´pase18’ = names(resultados2018)[names(resultados2018) == 'votos1']

# forma efectiva
partidos_nombre = c('pase18', 'pac18', 'adc18', 'pt18', 'fa18', 'pin18',
                    'pln18', 'pml18', 'png18', 'prc18', 'prsc18', 'prn18', 'pusc18')

cam_nombre = function(dataframe){
  for (i in 1:length(partidos_nombre)){
    names(dataframe)[names(dataframe) == paste0('votos', i)] =
      partidos_nombre[i]
  }
  return(dataframe)
}
resultados2018 = cam_nombre(resultados2018) #ejecutar funcion
View(resultados2018) # ver el resultado del cambio de nombre

#calcular porcentaje

votos_porcentaje = function(dataframe){
  x = dataframe%>%
    group_by(codigo)%>%
    mutate_all(funs((. / votos_validos)*100))%>%
    select(-votos_validos)
  return(x)
}

resultados2014_porcen = votos_porcentaje(resultados2014)
resultados2018_porcen = votos_porcentaje(resultados2018)
head(resultados2014_porcen)

# funcion para sacar partido ganador por zona
winner = function(dataframe, periodo){
  x = dataframe%>%
    gather(partido, votos, -codigo) %>%
    group_by(codigo)%>%
    filter(votos==max(votos))%>%
    separate(partido, c(paste0("partido", periodo)),
             sep="1")%>%
    select(-votos)
  return(x)
  
}

winner2014=winner(resultados2014_porcen, 14)
winner2018 =winner(resultados2018_porcen, 18)

# ¿Como cambio la distribucion de los cantones ganados por 
#cada partido politico en comparación con las elecciones 2014

cambio = winner2018%>%
  left_join(winner2014, by="codigo")%>%
  mutate(cambio=ifelse(partido18==partido14,"sin
cambio", "cambio"),
         robo=ifelse(cambio=="cambio",
         paste(partido18, partido14, sep=" al "), "sin cambio"))

table(cambio$cambio)

# sacar grafico de acuerdo al partido politico
grafico_votos = function(partido, color){
  
  x = resultados2018_porcen%>%
    
    select(codigo, paste0(partido,18))%>%
    
    left_join(
      
      (resultados2014_porcen%>%
         
         select(codigo, paste0(partido,14))),
      
      by="codigo")%>%
    
    gather(anio, votos, - codigo)%>%
    
    mutate(anio=ifelse(anio==paste0(partido,14), 2014, 2018))
  
  par(las=1, bty="l", family="mono", font=1, bg="transparent")
  
  return(
    
    beeswarm(votos ~ anio, data=x, col=color, pch=16, method="hex",
             
             cex=0.8, horizontal=TRUE, ylab="", xlab=paste("Porcentaje de
votos del", toupper(partido)),
             
             main=paste("Porcentaje de votos del", toupper(partido)),
             xlim=c(0, 60))
    
  )
  
}
 #mostrar gráfico y definir color
grafico_votos("pac", "#BE0000")
