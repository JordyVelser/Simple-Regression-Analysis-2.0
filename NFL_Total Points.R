install.packages(c("flexdashboard","plotly"))
install.packages(c("nortest","manhattanly", "DT", "MASS", "knitr"))
install.packages("ResourceSelection")

library(flexdashboard)
library(plotly)
library(ggplot2)
library(readxl)
library(car)
library(nortest)
library(plotly)
library(manhattanly)
library(DT)
library(MASS)
library(ResourceSelection)
library(knitr)
library(dplyr)

# Sacar Datos
choose.files()
datosNFL <- read_excel("C:\\Users\\marye\\Desktop\\Qarentena\\Análisis de Regresión\\Trabajo NFL\\NFL_Total Points.xlsx")
attach(datosNFL)

# Gráficas Y vs Xi
datosNFL2 <-select(datosNFL,-Team)
pairs(datosNFL2)

# Gráfica Histograma
require(psych)
multi.hist(x = datosNFL2, dcol = c("blue", "red"), 
           dlty = c("dotted", "solid"), main = "" )

# Matriz de correlación
require(GGally)
ggpairs(datosNFL2, lower = list(continuous = "smooth"), 
        diag = list(continuous = "bar"), axisLabels = "none")

# Regresión Lineal
modelo_original <- lm(`Total Points` ~ `Average passing yards per game` + `Average Rushing yards per game` + `Average Rushing Attemps per game` + Turnovers, data = datosNFL2)
summary(modelo_original)
vif(modelo_original) #multicolineidad < 10

#ANOVA

Anova(modelo_original)

#Scatter plot para checar homosedasticidad
residuales1 <- rstandard(modelo_original)

plot_ly(data = data.frame(datosNFL2), x=~`Average passing yards per game`, y=~residuales1)
plot_ly(data = data.frame(datosNFL2), x=~ `Average Rushing yards per game`, y=~residuales1)
plot_ly(data = data.frame(datosNFL2), x=~`Average Rushing Attemps per game`, y=~residuales1)
plot_ly(data = data.frame(datosNFL2), x=~Turnovers, y=~residuales1)

#Grafico de probabilidades
qqnorm(residuales1); qqline(residuales1)
qqPlot(residuales1)

#pruebas, para ver si mis residuales se distribuyen normal
ad.test(residuales1)

#inicio de correccion de modelo
#Backward
back <- step(modelo_original, direction = "backward")
Anova(back)
vif(back)

#Forward
forward <- step(modelo_original, direction = "forward")
vif(forward)
Anova(forward)

#Both
wise <- step(modelo_original, direction = "both")
Anova(wise)
vif(wise)

