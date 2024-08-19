rm(list = ls())

install.packages("glmmTMB")
install.packages("tidyverse")
install.packages("matrix")
install.packages("DHARMa")
install.packages("psych")
install.packages("car")
install.packages("emmeans")


library(lme4)
library(glmmTMB)
library(tidyverse)
library(ggplot2)
library(DHARMa)
library(psych)
library(car)
library(emmeans)
#---
# 1) git add .
# 2) git commit -m "Descripción de los cambios"
# 3) git push

#---

#_______________________________________________________________________________

ENFR_temporal <- read.csv("C:/Tesis/Datos/EsNsFR.csv", header = T, sep = ",", dec = ".")

NBI_CNA <- read.csv("C:/Tesis/Datos/CNA + NBI/NBI_Prov_Total_Y_CNA.csv", header = T, sep = ",", dec = ".")


#_______________________________________________________________________________

### Algunos arreglos en el dataset:

## Pasamos a factor todas las variables:

str(ENFR_temporal)
ENFR_temporal$Año_Edicion <- as.factor(ENFR_temporal$Año_Edicion)

ENFR_temporal$Provincia <- as.factor(ENFR_temporal$Provincia)

ENFR_temporal$Genero <- factor(ENFR_temporal$Genero)

ENFR_temporal$Nivel_de_instrucción <- factor(ENFR_temporal$Nivel_de_instrucción)

ENFR_temporal$Sit_laboral <- factor(ENFR_temporal$Sit_laboral)

ENFR_temporal$Cobertura_salud <- factor(ENFR_temporal$Cobertura_salud)

ENFR_temporal$Indice_NBI_hogar_dic <- factor(ENFR_temporal$Indice_NBI_hogar_dic)

ENFR_temporal$Quintil_ingresos <- factor(ENFR_temporal$Quintil_ingresos)

ENFR_temporal$Promedio_fv_Diario_Dic <- factor(ENFR_temporal$Promedio_fv_Diario_Dic)

ENFR_temporal$Cumple_No_Cumple_FyV<- factor(ENFR_temporal$Cumple_No_Cumple_FyV)

#---
## Transformamos Edad a numerico:

ENFR_temporal$Edad <- as.numeric(ENFR_temporal$Edad)

#---
## Ver que onda los Na`s:

sum(is.na(ENFR_temporal)) # La base de datos tiene 484 NA`s (RARO PORQUE NO HABIA ELIMINADO LOS NA´s DE LAS BASES INDIVIDUALES Y DEBERIAN HABER MAS)
colSums(is.na(ENFR_temporal)) # Vienen de Ingreso_mensual_pesos_hogar + Quintil_ingresos

#---
## La variable Provincia esta dando problemas en los modelos, vamos a ver que onda:

table(ENFR_temporal$Provincia)
any(is.na(ENFR_temporal$Provincia)) # Provincia no tiene datos faltantes

# Creamos variable provincia sin los datos de mayor pero y otro sin los datos de menor peso(datos registrados) para ver si esto gebera algo

ENFR_temporal <-ENFR_temporal %>% 
  filter( !(Provincia %in% c("Buenos Aires", )))

# Filtrar provincias con menos datos
provincias_pocas_datos <- c("Tierra del Fuego", "Santiago del Estero")  # Ajusta según el análisis

# Subconjunto de datos con estas provincias
ENFR_pocas_datos <- ENFR_temporal %>% 
  filter(Provincia %in% provincias_pocas_datos)

str(ENFR_pocas_datos)
sum(is.na(ENFR_pocas_datos))

ENFR_pocas_datos <- na.omit(ENFR_pocas_datos)


###_____________________________________________________________________________

# Exploratorio/ descriptiva de la ENFR: (No elimine los NA´s a la hora de unir las 3 ediciones, para ver que voy hacer en esta parte)

summary(ENFR_temporal)

# Vamos a separar pór años y aplicar summary para cada año asi es mas claro que pasa cada año

# Separamos los años (Para que los juntes si los volvi a separar? jajaj):
df_Años <- split(ENFR_temporal, ENFR_temporal$Año_Edicion)

# Aplicamos summary para cada año:
lapply(df_Años, summary)

#---
names(ENFR_temporal)
str(ENFR_temporal)


#---

# La variable "Edad" da problemas, hacemos una exploracion de ella:

# Resumen y detección de outliers
summary(ENFR_temporal$Edad) # Hay un maximo de 104 años que por ahi esta generando problemas.
boxplot(ENFR_temporal$Edad) # En el boxplot se ve que ese valor es un outlier.
hist(ENFR_temporal$Edad)
# Cuantas personas con edad > 100 años hay?

ENFR_temporal %>% 
  select(Edad) %>% 
  filter( Edad >= 100) %>% 
  summarise(count = n()) # Hay una sola persona >= 100 años

ENFR_temporal %>% 
  select(Edad) %>% 
  filter( Edad == 100) %>% 
  summarise(count = n()) # No hay personas con 100 años

ENFR_temporal %>% 
  select(Edad) %>% 
  filter( Edad >= 90 & Edad <= 100 ) %>% 
  summarise(count = n()) # Hay unas 357 personas con edad entre los 90 y 100 años.

ENFR_temporal <- ENFR_temporal %>% 
  filter( Edad != 104) #Elimina a esa persona de de 104 años
# No hubo resultados con lo hecho hasta aca, voy a reescalar la variable para ver si ahora me da:
ENFR_temporal$Edad_escalada <- scale(ENFR_temporal$Edad)


#---

# Graficos para las variables cuantis: Scatter plot (grafico de dispersion), Histograma, box plot


# Graficos para las variables cualis: Bar plot

#Grafico de barras de "cumple o No" con el minimo recomendado, agrupando todas las ediciones:

ggplot( data = ENFR_temporal, mapping = aes( x = Promedio_fv_Diario_Dic)) + 
          geom_bar(fill = "blue") #pinta toda la barra


ggplot( data = ENFR_temporal, mapping = aes( x = Promedio_fv_Diario_Dic)) + 
  geom_bar(color = "blue") #Pinta el contorno de la barra


#Grafico de barras de "cumple o No" con el minimo recomendado separado por años:

ggplot( data = ENFR_temporal, mapping = aes( x = Promedio_fv_Diario_Dic, fill = Año_Edicion ))+ 
  geom_bar(position = "dodge")

#Grafico de barras de "cumple o No" con el minimo recomendado separado por Genero:

ggplot( data = na.omit(ENFR_temporal), mapping = aes( x = Promedio_fv_Diario_Dic, fill = Año_Edicion))+
  geom_bar(position = "dodge")+
  labs( title = "Consumo de frutas y verduras en poblacion adulta Argentina",
        subtitle = "Consumo munimo de tres porciones diarias recomendado por edicion",
        x = "Consumo minimo recomendado diario",
        y = "Frecuencia",
        fill = "Edicion",
        caption = "Fuente: Encuesta Nacional de Factores de Riesgo (Indec)")+
  theme(axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 8),
        plot.caption = element_text(hjust = 0.5),
  )

#Grafico de barras de "cumple o No" con el minimo recomendado separado por Genero y por año:

ggplot(data = na.omit(ENFR_temporal), mapping = aes( x = Promedio_fv_Diario_Dic, fill = interaction(Sexo, Año_Edicion )))+
  geom_bar(position = "dodge")

# Separados por sexo:
ggplot(data = ENFR_temporal, mapping = aes(x = Promedio_fv_Diario_Dic, fill = Año_Edicion)) + 
  geom_bar(position = "dodge") +
  facet_wrap(~ Sexo)

# Separados por año:
ggplot(data = na.omit(ENFR_temporal), mapping = aes(x = Promedio_fv_Diario_Dic, fill = Sexo)) + 
  geom_bar(position = "dodge") +
  facet_wrap(~ Año_Edicion)+
  labs( title = "Consumo de frutas y verduras en poblacion adulta Argentina",
        subtitle = "Consumo munimo de tres porciones diarias recomendado por año y segun genero",
        x = "Consumo minimo recomendado diario",
        y = "Frecuencia",
        fill = "Genero",
        caption = "Fuente: Encuesta Nacional de Factores de Riesgo (Indec)")+
  theme(axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 8),
        plot.caption = element_text(hjust = 0.5),
        )

# Grafico de barras de "cumple o No" con el minimo recomendado separado por año y quintil de ingreso
# otras variables

#---
# Como la variable "Provincia" va a ingresar al modelo estadistico como unba Variable de Efectos Aleatorios, vamos a ver que cantidad de personas encuenstadas hubo por provincia:

table(ENFR_temporal$Provincia) # Variable no balanceada

ggplot( ENFR_temporal, aes(x = Provincia))+
  geom_bar( fill = "skyblue", color = "blue")+
  theme( axis.text.x = element_text(angle = 90, hjust = 0.5))

#---
# Usamos la función describeBy()(para variables numericas) del paquete psych, para generar estadísticas descriptivas agrupadas según una o más variables. Es muy útil cuando quieres obtener resúmenes estadísticos de un conjunto de datos dividido por grupos.

sum(is.na(ENFR_temporal$Cumple_No_Cumple_FyV))

describeBy(ENFR_temporal$Promedio_fyv_dia, group = ENFR_temporal$Año_Edicion)

#_______________________________________________________________________________


                         ### MODELOS SIMPLES ###
 str(ENFR_temporal)

 
 
### Modelos simple con Año como V E F:
sum(is.na(ENFR_temporal$Año_Edicion))
sum(is.na(ENFR_temporal$Cumple_No_Cumple_FyV))

m1a <- glmmTMB( Cumple_No_Cumple_FyV ~ Año_Edicion , ENFR_temporal, family = binomial)
 
## Supuestos: 
simulationOutput <- simulateResiduals(fittedModel = m1a, plot = T)

## Salidas estadisticas: 
summary(m1a)
Anova(m1a) # Anova con "A" mayuscula para modelos lineales generalizados, con "a" en minuscula para mlgenerales

## Comparaciones : emmeans
# Comparasiones en escala de logit:
emmeans(m1a, pairswise ~ Año_Edicion) # Es el que usan en clase y no funciona

pairs(emmeans(m1a,~ Año_Edicion )) # Funciona
# Comparacion en escala de la VR:
pairs(emmeans(m1a, ~ Año_Edicion, type = "response"))

# Intervalos de confianza: pasamos del estudio a la poblacion.
confint(m1a)

## Modelos simple con Año como V E F mas Provincia como V E A:

m1a1 <- glmmTMB( Cumple_No_Cumple_FyV ~ Año_Edicion + (1 | Provincia )  , ENFR_pocas_datos, family = binomial)


## Supuestos: 
simulationOutput <- simulateResiduals(fittedModel = m1a1, plot = T)
# Residuales en funcion de cada variable predictora: "plotresidual( simulado, var predic)"

#-------------------------------------------------------------------------------
 
### Modelos simple con Nivel_de_instrucción como V E F mas Provincia como V E A: 
table(ENFR_temporal$Nivel_de_instrucción) 

m1b1 <- glmer( Cumple_No_Cumple_FyV ~ Nivel_de_instrucción + (1 | Provincia ), ENFR_temporal, family = binomial)

m1b <- glmmTMB( Cumple_No_Cumple_FyV ~ Nivel_de_instrucción, ENFR_temporal, family = binomial)

## Supuestos: 

simulationOutput <- simulateResiduals(fittedModel = m1b, plot = T)

## Salidas estadisticas: 
summary(m1b)
Anova(m1b) 

## Contrastes: 

# Escala de PL:
pairs(emmeans(m1b, ~ Nivel_de_instrucción ))

# Escala de la VR:
pairs(emmeans(m1b, ~ Nivel_de_instrucción, type = "response"))

#-------------------------------------------------------------------------------
### Modelos simple con Edad como V E F mas Provincia como V E A:
m1c <- glmer( Cumple_No_Cumple_FyV ~  Edad + (1 | Provincia ), ENFR_temporal , family = binomial)

m1c <- glmmTMB( Cumple_No_Cumple_FyV ~  Edad, ENFR_temporal , family = binomial)

##  Supuestos: 

simulationOutput <- simulateResiduals(fittedModel = m1c, plot = T)
windows()

# Linealidad de VR con el logit: tiene que guardar una relacion lineal con el log de odd
# residuos <- residuals(m1c, type = "pearson")
# plot(ENFR_temporal$Edad, residuos,
#      xlab = "Edad",
#      ylab = "Residuos de Pearson",
#      main = "Residuos vs. Edad")
# abline(h = 0, col = "red", lty = 2)


## Salida de analisis estadistico
summary(m1c)

# Grafico predictivo:
Prediccion_m1c <- predict(m1c, ENFR_temporal, type="response")

ggplot( data = ENFR_temporal, mapping = aes( x = Edad, y = Prediccion_m1c ))+
  geom_point() +
  labs(
    title = "Probabilidad estimada segun edad",
    x = "Edad",
    y = "Probabilidad, pedicha, Cumple/No_cumple"
  )


#-------------------------------------------------------------------------------
### Modelos simple con Sexo como V E F mas Provincia como V E A:
m1d <- glmer( Cumple_No_Cumple_FyV ~  Genero + (1|Provincia ), ENFR_temporal , family = binomial)

m1d <- glmmTMB( Cumple_No_Cumple_FyV ~  Genero, ENFR_temporal , family = binomial)

## Supuestos:
simulationOutput <- simulateResiduals(fittedModel = m1d, plot = T)
 
## Salidas de analisis estadistico:
summary(m1d)
Anova(m1d)

## Comparaciones:

# En escala del PL:
pairs(emmeans(m1d, ~ Genero))

# En escala de VR:
pairs(emmeans(m1d, ~ Genero, type = "response"))

#-------------------------------------------------------------------------------
 # Modelos simple con Sit_laboral como V E F mas Provincia como V E A:
m1e <- glmmTMB( Cumple_No_Cumple_FyV ~  Sit_laboral + (1|Provincia ), ENFR_temporal, family = binomial)

m1e <- glmmTMB( Cumple_No_Cumple_FyV ~  Sit_laboral, ENFR_temporal, family = binomial)

## Supuestos:
simulationOutput <- simulateResiduals(fittedModel = m1e, plot = T)

## Salidas de analisis estadisticos:
summary(m1e)
Anova(m1e)

## Comparaciones:

# Escala del PL:
pairs(emmeans(m1e, ~ Sit_laboral))

# Escala de VR:
pairs(emmeans(m1e, ~ Sit_laboral, type = "response"))

#------------------------------------------------------------------------------- 
### Modelos simple con Quintil_ingresos como V E F mas Provincia como V E A:
m1g <- glmmTMB( Cumple_No_Cumple_FyV ~  Quintil_ingresos + (1|Provincia ), ENFR_temporal, family = binomial)

m1g <- glmmTMB( Cumple_No_Cumple_FyV ~  Quintil_ingresos, ENFR_temporal, family = binomial)

## Supuestos:
simulationOutput <- simulateResiduals(fittedModel = m1g, plot = T)

## Salidas de analisis estadisticos:
Anova(m1g)

## Comparaciones:
# Escala del PL:
pairs(emmeans(m1g, ~ Quintil_ingresos))

# Escala de VR:
pairs(emmeans(m1g, ~ Quintil_ingresos, type = "response"))
#------------------------------------------------------------------------------- 

### Modelos simple con Cobertura_salud como V E F mas Provincia como V E A: ESTE NO LO IBAMOS A USAR
m1f <- glmer( Cumple_No_Cumple_FyV ~  Cobertura_salud + (1|Provincia ), ENFR_temporal, family = binomial)

m1f <- glmmTMB( Cumple_No_Cumple_FyV ~  Cobertura_salud, ENFR_temporal, family = binomial)

## Supuestos:
simulationOutput <- simulateResiduals(fittedModel = m1f, plot = T)
 
## Salidas de analisis estadisticos:
summary(m1f)
Anova(m1f)
 
## Comparaciones:
 
# Escala del PL:
pairs(emmeans(m1f, ~Cobertura_salud))
 
# Escala de VR:
pairs(emmeans(m1f, ~Cobertura_salud, type = "response"))

#------------------------------------------------------------------------------

### Modelos simple con como V E F mas Provincia como V E A:
m1h <- glmer( Cumple_No_Cumple_FyV ~  + (1|Provincia ), ENFR_temporal, family = binomial)
 
## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = m1h, plot = T)
 ## Salidas de analisis estadisticos:
 
 ## Comparaciones:
 
 # Escala del PL:
 
 # Escala de VR:
 
#_______________________________________________________________________________ 
 

                ### MODELOS MULTIPLES (mas de una variable) ###

# M1a <-CFV ~ Genero + rango etario + año
 M1a <- glmmTMB(Cumple_No_Cumple_FyV ~ Genero + Edad + Año_Edicion, ENFR_temporal, family = binomial())

 
## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = M1a, plot = T)
 
## Salidas de analisis estadisticos:
 
## Comparaciones:
 
# Escala del PL:
 
# Escala de VR:
 
 
 #------------------------------------------------------------------------------
 # M1b<-CFV ~ Genero + rango etario + año + de a una sumar las variables de NSE (max nivel alcanza) 
 
 M1b <- glmmTMB(Cumple_No_Cumple_FyV ~ Genero + Edad + Año_Edicion + , ENFR_temporal, family = binomial())
 
 ## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = m1a1, plot = T)
 
 
 ## Salidas de analisis estadisticos:
 
 ## Comparaciones:
 
 # Escala del PL:
 
 # Escala de VR:
 
 
 
 
 
 
 #------------------------------------------------------------------------------
 # M1c <-CFV ~ Genero + rango etario + año + Nivel educ
 
 M1c <- glmmTMB(Cumple_No_Cumple_FyV ~ Genero + Edad + Año_Edicion +  + , ENFR_temporal, family = binomial())
 ## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = m1a1, plot = T)
 
 ## Salidas de analisis estadisticos:
 
 ## Comparaciones:
 
 # Escala del PL:
 
 # Escala de VR:
 
 
 #------------------------------------------------------------------------------
 # M1d <-CFV ~ Genero + rango etario + año + Nivel educ + Quintil ingreso
 M1c <- glmmTMB(Cumple_No_Cumple_FyV ~ Genero + Edad + Año_Edicion +  +  + , ENFR_temporal, family = binomial())
 
 
 ## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = m1a1, plot = T)
 
 ## Salidas de analisis estadisticos:
 
 ## Comparaciones:
 
 # Escala del PL:
 
 # Escala de VR:
 
 
 #------------------------------------------------------------------------------
 # M1e<-CFV ~ Genero + rango etario + año + Nivel educ + Quintil ingreso +CMV
 ## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = m1a1, plot = T)
 
 ## Salidas de analisis estadisticos:
 
 ## Comparaciones:
 
 # Escala del PL:
 
 # Escala de VR:
 
 
 #------------------------------------------------------------------------------
 # M1f <-CFV ~ Genero + rango etario + año + Nivel educ + Quintil ingreso +CMV + NBI M1 <Provincial
 
 ## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = m1a1, plot = T)
 
 ## Salidas de analisis estadisticos:
 
 ## Comparaciones:
 
 # Escala del PL:
 
 # Escala de VR:
 
 #------------------------------------------------------------------------------
 # M1g <-CFV ~ año + Género + Quintil de Ingreso + Nivel Educativo Alcanzado + CMV + Rango etario + NBI Provincial 
 
 ## Supuestos:
 
 ## Salidas de analisis estadisticos:
 
 ## Comparaciones:
 
 # Escala del PL:
 
 # Escala de VR:
 