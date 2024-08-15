rm(list = ls())

install.packages("glmmTMB")
install.packages("tidyverse")
install.packages("matrix")

library(lme4)
library(glmmTMB)
library(tidyverse)
library(ggplot2)


ENFR_temporal <- read.csv("C:/Tesis/Datos/EsNsFR.csv", header = T, sep = ",", dec = ".")

NBI_CNA <- read.csv("C:/Tesis/Datos/CNA + NBI/NBI_Prov_Total_Y_CNA.csv", header = T, sep = ",", dec = ".")


# Pasamos a factor la variable Año:
ENFR_temporal$Año_Edicion <- factor(ENFR_temporal$Año_Edicion)

# Pasamos a factor la variable Quintil de ingreso:

ENFR_temporal$Quintil_ingresos <- factor(ENFR_temporal$Quintil_ingresos)

# Pasamos los casos de "No_cumple" -> 0 y "Cumple" -> 1 de la variable "Promedio_fv_Diario_Dic":
table(ENFR_temporal$Promedio_fv_Diario_Dic)

ENFR_temporal <-ENFR_temporal %>%
  filter( Promedio_fv_Diario_Dic != "<NA>") %>% 
  mutate( Consumo_FyV = ifelse(Promedio_fv_Diario_Dic == "Cumple", 1, 0))


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

#_______________________________________________________________________________
names(ENFR_temporal)

# Modelos simples:


# Modelos simple con Año como V E F mas Provincia como V E A:
 m1a <- glmer( Consumo_FyV ~ Año_Edicion + (1|Provincia ) , ENFR_temporal, family = binomial)
 summary(m1a)
 anova(m1a)
 
 # Modelos simple con Nivel_de_instrucción como V E F mas Provincia como V E A: 
 m1b <- glm( Consumo_FyV ~ Nivel_de_instrucción + (1|Provincia ), ENFR_temporal, family = binomial)
 
 # Modelos simple con Edad como V E F mas Provincia como V E A:
 m1c <- glm( Consumo_FyV ~  Edad, ENFR_temporal + (1|Provincia ), family = binomial)
 
 # Modelos simple con Sexo como V E F mas Provincia como V E A:
 m1d <- glm( Consumo_FyV ~  Sexo, ENFR_temporal + (1|Provincia ), family = binomial)
 
 # Modelos simple con Sit_laboral como V E F mas Provincia como V E A:
 m1e <- glm( Consumo_FyV ~  Sit_laboral + (1|Provincia ), ENFR_temporal, family = binomial)
 
 # Modelos simple con Cobertura_salud como V E F mas Provincia como V E A:
 m1f <- glm( Consumo_FyV ~  Cobertura_salud + (1|Provincia ), ENFR_temporal, family = binomial)
 
 # Modelos simple con Quintil_ingresos como V E F mas Provincia como V E A:
 m1g <- glm( Consumo_FyV ~  Quintil_ingresos + (1|Provincia ), ENFR_temporal, family = binomial)
 
 # Modelos simple con Año como V E F mas Provincia como V E A:
 m1h <- glm( Consumo_FyV ~  + (1|Provincia ), ENFR_temporal, family = binomial)