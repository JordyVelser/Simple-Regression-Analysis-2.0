####################
#                  #
# Tarea 1 Regresión #
#                  #
####################

# instalar paquete readxl
install.packages("readxl")

# cargar paquete readxl
library(readxl)

# buscar la ruta del archivo de excel
file.choose()
datos_excel<-"C:\\Users\\marye\\Desktop\\Tareas\\Datosmrls.xlsx"

# Copiar ruta de la consola y guardar en variable
excel_sheets(datos_excel)

# como mirar las hojas de tu excel
excel_regresion <-read_excel(datos_excel)

#Variables de Excel
excel_regresion
attach(excel_regresion)
names(excel_regresion)

#Instalar paquetes

install.packages(c("tidyverse", "Quantpsyc"))
install.packages(c("QuantPsyc"))

#Cargar librerías

library(boot)
library(ggplot2)
library(car)
library(tidyverse)
library(QuantPsyc)

#Observar datos

excel_regresion

#Graficar los datos

Grafica1 <- ggplot(excel_regresion,
                   aes(xi,yi))

Grafica1 + geom_point()

#Seleccion del modelo 

Modelo <- lm(yi~xi,data = excel_regresion)

#Resumen del modelo 

summary(Modelo)

#Podemos ver que la ecuación de nuesto modelo es: yg = 13.62299 - 0.07983x #
#Donde beta 0 = 13.62299 y beta 1 = -0.07983 #
#Con una R^2 = 0.7144 y una F de Fisher con 57.54 con 1 y 23 grados de libertad #

#Agregando la línea de regresión

Grafica1 + geom_point(size=2, shape= 1, colour= "blue") + geom_smooth(method = "lm", colour = "Red") + labs(x= "Grados Centígrados", y = "Días")

#Encontramos los valores ajustados  "fits" de yg

yg = 13.62299 - 0.07983*xi
yg

#Obtenemos valores de la tabla ANOVA

aov(yi~xi)
ANOVA<-aov(yi~xi)

summary(ANOVA)
