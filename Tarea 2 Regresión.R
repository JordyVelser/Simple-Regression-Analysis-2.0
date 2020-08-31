############################
# Tarea 2 Regresión Lineal #
############################

#Cargar paquete readxl
library(readxl)

# buscar la ruta del archivo de excel
file.choose()
Datos_Ej2<-"C:\\Users\\marye\\Desktop\\Tareas\\Ejemplo2.xlsx"

# Copiar ruta de la consola y guardar en variable
excel_regresion2<-read_excel(Datos_Ej2)

#Variables de Excel
excel_regresion2
attach(excel_regresion2)
names(excel_regresion2)

#Cargar librerías

library(boot)
library(ggplot2)
library(car)
library(tidyverse)
library(QuantPsyc)

#Graficar los datos

Grafica2 <- ggplot(excel_regresion2,
                   aes(x1,yi))

Grafica2 + geom_point()

#Seleccion del modelo 

Modelo2 <- lm(yi~x1+x2,data = excel_regresion2)

#Resumen del modelo 

summary(Modelo2)

#Podemos ver que la ecuación de nuesto modelo es: yg = 20.2508 - 0.8.3702x + 1.5625x^2 #
#Donde beta 0 = 20.2508, beta 1 = -8.3702 y beta 2 = 1.5625 #
#Con una R^2 = 0.9472 y una F de Fisher con 80.76 con 2 y 9 grados de libertad #

#Agregando la línea de regresión

Grafica2 + geom_point(size =2, shape=1, colour = "blue") + geom_smooth(colour = "Red")

#Encontramos los valores de la tabla ANOVA

aov(yi~x1+x2)
ANOVA2 <-aov(yi~x1+x2)

summary(ANOVA2)


