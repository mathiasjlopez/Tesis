rm(list = ls())

install.packages("glmmTMB")
install.packages("tidyverse")
install.packages("matrix")
install.packages("DHARMa")
install.packages("psych")
install.packages("car")

library(lme4)
library(glmmTMB)
library(tidyverse)
library(ggplot2)
library(DHARMa)
library(psych)
library(car)

ENFR_temporal <- read.csv("C:/Tesis/Datos/EsNsFR.csv", header = T, sep = ",", dec = ".")

NBI_CNA <- read.csv("C:/Tesis/Datos/CNA + NBI/NBI_Prov_Total_Y_CNA.csv", header = T, sep = ",", dec = ".")


#_______________________________________________________________________________

# Algunos arreglos en el dataset:

# Pasamos a factor todas las variables:

str(ENFR_temporal)
ENFR_temporal$Año_Edicion <- factor(ENFR_temporal$Año_Edicion)

ENFR_temporal$Provincia <- factor(ENFR_temporal$Provincia)

ENFR_temporal$Genero <- factor(ENFR_temporal$Genero)

ENFR_temporal$Nivel_de_instrucción <- factor(ENFR_temporal$Nivel_de_instrucción)

ENFR_temporal$Sit_laboral <- factor(ENFR_temporal$Sit_laboral)

ENFR_temporal$Cobertura_salud <- factor(ENFR_temporal$Cobertura_salud)

ENFR_temporal$Indice_NBI_hogar_dic <- factor(ENFR_temporal$Indice_NBI_hogar_dic)

ENFR_temporal$Quintil_ingresos <- factor(ENFR_temporal$Quintil_ingresos)

ENFR_temporal$Promedio_fv_Diario_Dic <- factor(ENFR_temporal$Promedio_fv_Diario_Dic)

ENFR_temporal$Cumple_No_Cumple_FyV<- factor(ENFR_temporal$Cumple_No_Cumple_FyV)

# Transformamos Edad a numerico:

ENFR_temporal$Edad <- as.numeric(ENFR_temporal$Edad)


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
  filter( Edad != 104) #Elimine a esa persona de de 104 años
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


### Modelos simples ###
 str(ENFR_temporal)

# Modelos simple con Año como V E F mas Provincia como V E A:
 m1a <- glmer( Cumple_No_Cumple_FyV ~ Año_Edicion + (1|Provincia ) , ENFR_temporal, family = binomial)
 summary(m1a)
 anova(m1a)
 
simulationOutput <- simulateResiduals(fittedModel = m1a, plot = T)
 
 
# Modelos simple con Nivel_de_instrucción como V E F mas Provincia como V E A: 
table(ENFR_temporal$Nivel_de_instrucción) 

m1b <- glmer( Cumple_No_Cumple_FyV ~ Nivel_de_instrucción + (1 | Provincia ), ENFR_temporal, family = binomial)
summary(m1b)
anova(m1b) 
 
simulationOutput <- simulateResiduals(fittedModel = m1b, plot = T)
 
# Modelos simple con Edad como V E F mas Provincia como V E A:
m1c <- glmer( Cumple_No_Cumple_FyV ~  Edad_escalada + (1 | Provincia ), ENFR_temporal , family = binomial)

# Ver colinealidad de edad:
vif_modelo <- vif(lm(Edad ~ ., data = ENFR_temporal))
print(vif_modelo)

summary(m1b)

 
simulationOutput <- simulateResiduals(fittedModel = m1c, plot = T)
windows()
# Modelos simple con Sexo como V E F mas Provincia como V E A:
m1d <- glmer( Cumple_No_Cumple_FyV ~  Sexo + (1|Provincia ), ENFR_temporal , family = binomial)

summary(m1d)
anova(m1d)
 
simulationOutput <- simulateResiduals(fittedModel = m1d, plot = T)
 
 # Modelos simple con Sit_laboral como V E F mas Provincia como V E A:
m1e <- glmer( Cumple_No_Cumple_FyV ~  Sit_laboral + (1|Provincia ), ENFR_temporal, family = binomial)
summary(m1d)
anova(m1d)
 
simulationOutput <- simulateResiduals(fittedModel = m1e, plot = T)
 
 # Modelos simple con Cobertura_salud como V E F mas Provincia como V E A:
 m1f <- glmer( Cumple_No_Cumple_FyV ~  Cobertura_salud + (1|Provincia ), ENFR_temporal, family = binomial)
 summary(m1f)
 anova(m1f)
 
 simulationOutput <- simulateResiduals(fittedModel = m1f, plot = T)
 
 # Modelos simple con Quintil_ingresos como V E F mas Provincia como V E A:
 m1g <- glmer( Cumple_No_Cumple_FyV ~  Quintil_ingresos + (1|Provincia ), ENFR_temporal, family = binomial)
 summary(m1g)
 anova(m1g)
 
 simulationOutput <- simulateResiduals(fittedModel = m1g, plot = T)
 
 # Modelos simple con Año como V E F mas Provincia como V E A:
 m1h <- glmer( Cumple_No_Cumple_FyV ~  + (1|Provincia ), ENFR_temporal, family = binomial)
 
 
 simulationOutput <- simulateResiduals(fittedModel = m1h, plot = T)
 