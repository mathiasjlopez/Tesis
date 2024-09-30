rm(list = ls())


#Paquetes:
install.packages("glmmTMB")
#install.packages("TMB")
install.packages("TMB", type = "source")
install.packages("tidyverse")
install.packages("Matrix")
install.packages("DHARMa")
install.packages("psych")
install.packages("car")
install.packages("emmeans")
install.packages("lme4")
install.packages("GGally")
install.packages("effects")


# Librerias:
library(lme4)
library(glmmTMB)
library(tidyverse)
library(emmeans)
library(ggplot2)
library(DHARMa)
library(psych)
library(car)
library(GGally)
library(effects)

#---
# 1) git add .
# 2) git commit -m "Rorganizacion de modelos"
# 3) git push

#---


#______________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________

ENFR_temporal <- read.csv("C:/Tesis/Datos/EsNsFR.csv", header = T, sep = ",", dec = ".")

 
NBI_CNA <- read.csv("C:/Tesis/Datos/CNA + NBI/NBI_Prov_Total_Y_CNA.csv", header = T, sep = ",", dec = ".")


#______________________________________________________________________________________________________________
#______________________________________________________________________________________________________________


### Algunos arreglos en el dataset:

## Pasamos a factor todas las variables:

# ENFR:
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

ENFR_temporal$Rango_edad <- factor(ENFR_temporal$Rango_edad)


#NBI_CNA:

NBI_CNA$Año_Edicion <- factor(NBI_CNA$Año_Edicion)
NBI_CNA$Provincia <- factor(NBI_CNA$Provincia)
NBI_CNA$Porcentaje_hogares_NBI <- as.numeric(NBI_CNA$Porcentaje_hogares_NBI)

#---
## Transformamos Edad a numerico:


ENFR_temporal$Edad <- as.numeric(ENFR_temporal$Edad)

#---
## Ver que onda los Na`s:

sum(is.na(ENFR_temporal)) # La base de datos tiene 484 NA`s (RARO PORQUE NO HABIA ELIMINADO LOS NA´s DE LAS BASES INDIVIDUALES Y DEBERIAN HABER MAS)
colSums(is.na(ENFR_temporal)) # Vienen de Ingreso_mensual_pesos_hogar + Quintil_ingresos

ENFR_temporal <- na.omit(ENFR_temporal)

#---
summary(ENFR_temporal)
table(ENFR_temporal)

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


#-----
##Grafico de barras de "cumple o No" con el minimo recomendado separado por Genero:

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
 
 # Calculamos la proporcion de Promedio_fv_Diario_Dic por cada año para graficarlo:

Proporcion_CFyV <- ENFR_temporal %>%
  group_by(Año_Edicion, Promedio_fv_Diario_Dic) %>%
  summarise(count = n()) %>%
  mutate(Proporcion = count / sum(count))

ggplot( data = na.omit(Proporcion_CFyV), mapping = aes( x = Promedio_fv_Diario_Dic, y = Proporcion ,fill = Año_Edicion))+
  geom_bar(stat = "identity", position = "dodge")+
  labs( title = "Consumo de frutas y verduras en poblacion adulta Argentina",
        subtitle = "Consumo munimo de tres porciones diarias recomendado por edicion",
        x = "Consumo minimo recomendado diario",
        y = "Proporcion",
        fill = "Edicion",
        caption = "Fuente: Encuesta Nacional de Factores de Riesgo (Indec)")+
  theme(axis.text = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 8),
        plot.caption = element_text(hjust = 0.5),
  )

#-----

#Grafico de barras de "cumple o No" con el minimo recomendado separado por Genero y por año:

ggplot(data = na.omit(ENFR_temporal), mapping = aes( x = Promedio_fv_Diario_Dic, fill = interaction(Sexo, Año_Edicion )))+
  geom_bar(position = "dodge")

# Separados por sexo:
ggplot(data = ENFR_temporal, mapping = aes(x = Promedio_fv_Diario_Dic, fill = Año_Edicion)) + 
  geom_bar(position = "dodge") +
  facet_wrap(~ Sexo)

# Separados por año:

genero_año_prop <- ENFR_temporal %>% 
  group_by(Genero,Año_Edicion, Promedio_fv_Diario_Dic) %>% 
  summarise(count = n())%>%
  mutate(Proporcion = count / sum(count))
              

ggplot(data = na.omit(genero_año_prop), mapping = aes(x = Promedio_fv_Diario_Dic, y = Proporcion, fill = Genero)) + 
  geom_bar(stat = "identity",position = "dodge") +
  facet_wrap(~ Año_Edicion)+
  labs( title = "Consumo de frutas y verduras en poblacion adulta Argentina",
        subtitle = "Consumo munimo de tres porciones diarias recomendado por año y segun genero",
        x = "Consumo minimo recomendado diario",
        y = "Proporcion",
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

#-----

# Como la variable "Provincia" va a ingresar al modelo estadistico como una Variable de Efectos Aleatorios, vamos a ver que cantidad de personas encuenstadas hubo por provincia:

table(ENFR_temporal$Provincia) # Variable no balanceada

ggplot( ENFR_temporal, aes(x = Provincia))+
  geom_bar( fill = "skyblue", color = "blue")+
  theme( axis.text.x = element_text(angle = 90, hjust = 0.5))

#---
# Usamos la función describeBy()(para variables numericas) del paquete psych, para generar estadísticas descriptivas agrupadas según una o más variables. Es muy útil cuando quieres obtener resúmenes estadísticos de un conjunto de datos dividido por grupos.

sum(is.na(ENFR_temporal$Cumple_No_Cumple_FyV))

describeBy(ENFR_temporal$Promedio_fyv_dia, group = ENFR_temporal$Año_Edicion)

#_______________________________________________________________________________

### Creando un nuevo dataset con ambas bases para laburar en modelos con interaccion

ENFR_t_NBI_CNA <- ENFR_temporal %>% 
  left_join(NBI_CNA %>% select(Año_Edicion, Provincia, Porcentaje_hogares_NBI),
            by = c("Año_Edicion", "Provincia") )
            
str(ENFR_t_NBI_CNA)
sum(is.na(ENFR_t_NBI_CNA))
# Eliminamos los NA:

ENFR_t_NBI_CNA <- na.omit(ENFR_t_NBI_CNA)

boxplot(ENFR_t_NBI_CNA$porcentaje_hogares_NBI)


# Explorando un poquito los datos de esta nueva base:

# ENFR_t_NBI_CNA %>% 
#   select(Provincia, Año_Edicion, Cumple_No_Cumple_FyV, Porcentaje_hogares_NBI) %>% 
#   filter( Provincia == "Tierra del Fuego", Año_Edicion == 2009) # LOS VALORES DE PORCENTAJE NBI SE VAN A REPETIR TODAS  LAS VECES QUE EN UN AÑO SE REPITAN LOS ENCUESTADOS EN UNA PROVINCIA.



### Modificar la variable Procentaje de hogares nbi:
# Calcular los puntos de corte para los terciles

cortes <- quantile(ENFR_t_NBI_CNA$Porcentaje_hogares_NBI, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
print(cortes)
# Crear la variable categórica de terciles

ENFR_t_NBI_CNA$Terciles_NBI_provincial <- cut(ENFR_t_NBI_CNA$Porcentaje_hogares_NBI, 
                                   breaks = cortes, 
                                   labels = c("Bajo", "Medio", "Alto"), 
                                   include.lowest = TRUE)

# Verificar la nueva variable
table(ENFR_t_NBI_CNA$Terciles_NBI_provincial)
str(ENFR_t_NBI_CNA$Terciles_NBI_provincial)


#---
ENFR_t_NBI_CNA %>% 
  select(Rango_edad) %>% 
  filter(Rango_edad == 6)




#_________________________________________________________________________________________________________________________________________________________________________________
#_________________________________________________________________________________________________________________________________________________________________________________



                                                                 ### PRIMERA PARTE: TEMPORAL ###


#_______________________________________________________________________________

                                                                     ### MODELOS SIMPLES ###

 
 
### Modelos de AÑO EDICION:
 
sum(is.na(ENFR_temporal$Año_Edicion))
sum(is.na(ENFR_temporal$Cumple_No_Cumple_FyV))

m1a <- glmer( Cumple_No_Cumple_FyV ~ Año_Edicion + (1 | Provincia )  , ENFR_temporal, family = binomial)
 
## Supuestos:

simulationOutput <- simulateResiduals(fittedModel = m1a, plot = T) # HACER UN RESAMPLEO, EL DHARMa NO SE BANCA TANTOS DATOS 
  # Variable de efectos aleatorios:

## Salidas estadisticas: 
summary(m1a)
# Anova con "A" mayuscula para modelos lineales generalizados, con "a" en minuscula para mlgenerales
Anova(m1a) #  Realiza una prueba estadística (en este caso, el test de Chi-cuadrado) para evaluar si existen diferencias significativas entre los niveles de la variable predictora. Pero no desglosa los efectos individuales o las medias para cada nivel.

## Comparaciones : emmeans
  # Comparasiones en escala de logit:
emmeans(m1a, pairwise ~ Año_Edicion) # Escala del PL

  # Comparacion en escala de la VR:
emmeans(m1a, pairwise ~ Año_Edicion, type = "response") #escala de la VR. Con emmeans se pueden calcular Las medias estimadas (o probabilidades predichas) para cada nivel de la variable cualitativa, con los límites inferior y superior del intervalo de confianza para la probabilidad estimada.
print(emm)

  # Intervalos de confianza: pasamos del estudio a la poblacion.
confint(m1a)


# Residuales en funcion de cada variable predictora: "plotresidual( simulado, var predic)"

#-------------------------------------------------------------------------------
 
### Modelos con NIVEL DE INSTRUCCION:

table(ENFR_temporal$Nivel_de_instrucción) 

m1b1 <- glmer( Cumple_No_Cumple_FyV ~ Nivel_de_instrucción + (1 | Provincia ), ENFR_temporal, family = binomial)

## Supuestos: 

simulationOutput <- simulateResiduals(fittedModel = m1b, plot = T)

## Salidas estadisticas: 
summary(m1b)
Anova(m1b) 

## Contrastes: 

# Escala de PL:
emmeans(m1b, pairwise ~ Nivel_de_instrucción )

# Escala de la VR:
emmeans(m1b, pairwise ~ Nivel_de_instrucción, type = "response")

#-------------------------------------------------------------------------------

### Modelos de RANGO EDAD:

m1c <- glmer( Cumple_No_Cumple_FyV ~  Rango_edad + (1 | Provincia ), ENFR_temporal , family = binomial)


##  Supuestos: 

simulationOutput <- simulateResiduals(fittedModel = m1c, plot = T)
windows()

# # Linealidad de VR con el logit: tiene que guardar una relacion lineal con el log de odd
# Logit_vs_edad <- glmmTMB(Cumple_No_Cumple_FyV ~ Edad, data = ENFR_temporal, family = binomial)
# 
# logit_grafico <- predict(Logit_vs_edad, type = "link")  # Esto te da el logit (log-odds)
#   #grafico logit vs Edad:
# 
# plot(ENFR_temporal$Edad, logit_grafico, xlab = "Edad", ylab = "Logit", main = "Relación entre Edad y Logit")
# abline(lm(logit_grafico ~ ENFR_temporal$Edad), col = "blue")

## Salida de analisis estadistico
summary(m1c)
Anova(m1c)


## Contrastes:

  # Escala PL:
emmeans(m1c, pairwise ~ Rango_edad )

  # Escala de VR:
emmeans(m1c, pairwise ~ Rango_edad, type = "response" )

#-------------------------------------------------------------------------------

### Modelos GENERO:

m1d <- glmer( Cumple_No_Cumple_FyV ~  Genero + (1|Provincia ), ENFR_temporal , family = binomial)


## Supuestos:
simulationOutput <- simulateResiduals(fittedModel = m1d, plot = T)
 
## Salidas de analisis estadistico:
summary(m1d)
Anova(m1d)

## Comparaciones:

# En escala del PL:
emmeans(m1d, pairwise ~ Genero )

# En escala de VR:
emmeans(m1d, pairwise ~ Genero, type = "response")
#-------------------------------------------------------------------------------

## Modelos SITUACION LABORAL:

m1e <- glmer( Cumple_No_Cumple_FyV ~  Sit_laboral + (1|Provincia ), ENFR_temporal, family = binomial)

## Supuestos:
simulationOutput <- simulateResiduals(fittedModel = m1e, plot = T)

## Salidas de analisis estadisticos:
summary(m1e)
Anova(m1e)

## Comparaciones:

# Escala del PL:
emmeans(m1e, pairwise  ~ Sit_laboral)
# Escala de VR:
emmeans( m1e, pairwise  ~ Sit_laboral, type = "response")

#------------------------------------------------------------------------------- 

### Modelos QUINTIL INGRESO:

m1g <- glmer( Cumple_No_Cumple_FyV ~  Quintil_ingresos + (1|Provincia ), ENFR_temporal, family = binomial)



## Supuestos:
simulationOutput <- simulateResiduals(fittedModel = m1g, plot = T)

## Salidas de analisis estadisticos:
Anova(m1g)

## Comparaciones:
# Escala del PL:
emmeans(m1g, pairwise ~ Quintil_ingresos)

# Escala de VR:
emmeans(m1g, pairwise ~ Quintil_ingresos, type = "response")
#------------------------------------------------------------------------------- 

### Modelos COBERTURA SALUD: ESTE NO LO IBAMOS A USAR
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
m1h <- glmer( Cumple_No_Cumple_FyV ~ Indice_NBI_hogar_dic + (1|Provincia ), ENFR_temporal, family = binomial)
 
## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = m1h, plot = T)
 ## Salidas de analisis estadisticos:
 
 Anova(m1h)
 
 ## Comparaciones:
 
 emmeans(m1h, pairwise ~ Indice_NBI_hogar_dic, type = "response")
 
 
 # Escala del PL:
 
 # Escala de VR:
 
#-------------------------------------------------------------------------------
 
# Modelo simple de  Porcentaje_hogares_NBI en ENFR_t_NBI_CNA:
 
m1i <- glmer(Cumple_No_Cumple_FyV ~ Porcentaje_hogares_NBI + (1|Provincia ), ENFR_t_NBI_CNA, family = binomial ) 

 ## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = m1i, plot = T)
 ## Salidas de analisis estadisticos:
 summary(m1i)
 Anova(m1i)
 
 ## Comparaciones: 

 emmeans(m1i, pairwise ~ Porcentaje_hogares_NBI, type = "response")
 
 
 
#__________________________________________________________________________________________________________________________________________________________________________________ 
 
 
                                                       ### MODELOS MULTIPLES (mas de una variable) ###

 
 
# M1a <-CFV ~ Genero + rango etario + año
M1a <- glmer(Cumple_No_Cumple_FyV ~ Genero + Rango_edad + Año_Edicion + (1|Provincia ), ENFR_temporal, family = binomial())

 
## Supuestos:
 
 simulationOutput <- simulateResiduals(fittedModel = M1a, plot = T)
 
 # supuesto para variable cuanti: linealidad de edad con logit
# Logit_vs_edad <- glmmTMB(Cumple_No_Cumple_FyV ~ Edad, data = ENFR_temporal, family = binomial)
# 
# logit_grafico <- predict(Logit_vs_edad, type = "link")  # Esto te da el logit (log-odds)
#  #grafico logit vs Edad:
# 
# plot(ENFR_temporal$Edad, logit_grafico, xlab = "Edad", ylab = "Logit", main = "Relación entre Edad y Logit")
# abline(lm(logit_grafico ~ ENFR_temporal$Edad), col = "blue")

 
## Colinealidad?
 car::vif(M1a)
 
 
 # Variable de efectos aleatorios:
 
## Salidas de modelo:

summary(M1a)
Anova(M1a)
 
## Comparaciones 
emmeans(M1a, pairwise ~ Genero, type = "response")
emmeans(M1a, pairwise ~ Rango_edad, type = "response")
emmeans(M1a, pairwise ~ Año_Edicion, type = "response")

## Ajuste del modelo: AIC/BIC / Devianza explicada(simil Rcuadrado)
 
 
 #------------------------------------------------------------------------------
 # M1b<-CFV ~ Genero + rango etario + año + de a una sumar las variables de NSE (max nivel alcanza) 
 
 M1b <- glmer(Cumple_No_Cumple_FyV ~ Genero + Rango_edad + Año_Edicion + Nivel_de_instrucción + (1|Provincia ), ENFR_temporal, family = binomial())

## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = m1a1, plot = T)
 # Colinealidad?
 car :: vif(M1b)
 
 # Variable de efectos aleatorios: 
 
## Salidas de modelo:
Anova(M1b) 

## Ajuste del modelo: AIC/BIC
 
## Comparaciones:
 emmeans(M1b, pairwise ~ Genero, type = "response")
 emmeans(M1b, pairwise ~ Rango_edad, type = "response")
 emmeans(M1b, pairwise ~ Año_Edicion, type = "response")
 emmeans(M1b, pairwise ~ Nivel_de_instrucción, type = "response")
 
 
 #------------------------------------------------------------------------------
 # M1c: AGREGA COB SALUD PERO DIJIMOS QUE NO VAMOS A USARLO, NO LO VOY A TENER EN CUENTA EN LAS QUE SIGUE
 
 M1c <- glmer(Cumple_No_Cumple_FyV ~ Genero + Rango_edad + Año_Edicion + Nivel_de_instrucción + Cobertura_salud + (1|Provincia ), ENFR_temporal, family = binomial())

 ## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = , plot = T)
 
 # Variable de efectos aleatorios:
 # Colinealidad?
 car :: vif(M1c)
## Salidas de modelo:
Anova(M1c)
 
## Comparaciones:
 emmeans(M1c, pairwise ~ Genero, type = "response")
 emmeans(M1c, pairwise ~ Rango_edad, type = "response")
 emmeans(M1c, pairwise ~ Año_Edicion, type = "response")
 emmeans(M1c, pairwise ~ Nivel_de_instrucción, type = "response")
 emmeans(M1c, pairwise ~ Cobertura_salud, type = "response")
 
 
## Ajuste del modelo: AIC/BIC 
 
 #------------------------------------------------------------------------------
 # M1d <-CFV ~ Genero + rango etario + año + Nivel educ + Quintil ingreso
 M1d <- glmer(Cumple_No_Cumple_FyV ~ Genero + Rango_edad + Año_Edicion + Nivel_de_instrucción + Sit_laboral + (1|Provincia ), ENFR_temporal, family = binomial())
 
 
## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = , plot = T)
 
## Variable de efectos aleatorios: 

## Colinealidad?
car :: vif(M1d)

## Salidas de modelo:

Anova(M1d)

## Comparaciones:

emmeans(M1d, pairwise ~ Sit_laboral, type = "response" )


## Ajuste del modelo: AIC/BIC
 
 
 #------------------------------------------------------------------------------
 # M1e<-CFV ~ Genero + rango etario + año + Nivel educ + Quintil ingreso +CMV
 
 M1e <- glmer(Cumple_No_Cumple_FyV ~ Genero + Rango_edad + Año_Edicion + Nivel_de_instrucción + Sit_laboral + Quintil_ingresos + (1|Provincia ), ENFR_temporal, family = binomial())
 
## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = , plot = T)

 # Variable de efectos aleatorios:
 
 # Colinealidad?
 
## Salidas de modelo:
 Anova(M1e)
 
## Comparaciones:

emmeans(M1e, pairwise ~ Quintil_ingresos ) 
## Ajuste del modelo: AIC/BIC
 
 #------------------------------------------------------------------------------
 # M1f <-CFV ~ Genero + rango etario + año + Nivel educ + Quintil ingreso +CMV + NBI M1 <Provincial
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)) #parámetros de control para la optimización
 
M1f <- glmer(Cumple_No_Cumple_FyV ~ Genero + Rango_edad + Año_Edicion + Nivel_de_instrucción + Sit_laboral + Quintil_ingresos + Indice_NBI_hogar_dic + (1|Provincia ), ENFR_temporal, family = binomial(), control = control)
 
## Supuestos:
 simulationOutput <- simulateResiduals(fittedModel = M1f, plot = T)
 
 # Variable de efectos aleatorios:
 
 # Colinealidad?
car :: vif(M1f)
 
## Salidas de modelo:

Anova(M1f)

## Comparaciones:

emmeans(M1f, pairwise ~ Indice_NBI_hogar_dic, type = "response" )
 
## Ajuste del modelo: AIC/BIC
 
 
 #------------------------------------------------------------------------------
# Modelos que se le agrega una variable cuanti (hasta el momento todas cualis)

control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)) #parámetros de control para la optimización

M1g <- glmer(Cumple_No_Cumple_FyV ~ Genero + Rango_edad + Año_Edicion + Nivel_de_instrucción + Sit_laboral + Quintil_ingresos + Indice_NBI_hogar_dic + Terciles_NBI_provincial + (1|Provincia ), ENFR_t_NBI_CNA, family = binomial())
 
## Supuestos:
 
 # Variable de efectos aleatorios:

## Correlacion:

ggpairs(ENFR_t_NBI_CNA, cardinality_threshold = 25)

cor(ENFR_t_NBI_CNA, use = "complete.obs")

## Colinealidad?
car :: vif(M1g) # problemas cuando es > 5


 
## Salidas de modelo:
summary(M1g)

## Comparaciones:
emmeans(M1g, pairwise ~ Porcentaje_hogar_NBI  , type = "response" )
 



 ## Ajuste del modelo: AIC/BIC
 
 #_________________________________________________________________________________________________________________________________________________________________________________
 
 
                                                                 ### MODELOS CON INTERACCION ###



#--- 

# Porcentaje_hogares_NBI es lineal al logit?
# Logit_vs_Porcentaje_hogares_NBI <- glmer(Cumple_No_Cumple_FyV ~ Porcentaje_hogares_NBI, data = ENFR_temporal, family = binomial)
# # 
# logit_grafico <- predict(Logit_vs_edad, type = "link")  # Esto te da el logit (log-odds)

##grafico logit vs Porcentaje_hogares_NBI:
# 
# plot(ENFR_temporal$Porcentaje_hogares_NBI, logit_grafico, xlab = "Porcentaje_hogares_NBI", ylab = "Logit", main = "Relación entre Porcentaje_hogares_NBI y Logit")
# abline(lm(logit_grafico ~ ENFR_temporal$Porcentaje_hogares_NBI), col = "blue")


#-------------------------------------------------------------------------------



                                                          ### MODELO A: TRIPLE INTERACCION ###

ModeloA <- glmmTMB(Cumple_No_Cumple_FyV ~ Genero*Año_Edicion*Quintil_ingresos + Rango_edad  + Nivel_de_instrucción  + Indice_NBI_hogar_dic + Terciles_NBI_provincial +(1|Provincia ), ENFR_t_NBI_CNA, family = binomial)

## Salidas de modelo:

summary(ModeloA)

drop1(ModeloA, test = "Chisq")

AIC(modeloA)
## Comparaciones
# Como no es significativa la interaccion, no hacemos comparaciones de la interaccion

# Variables aisladas:
emmeans(ModeloA, pairwise ~ Rango_edad  , type = "response")
emmeans(ModeloA, pairwise ~ Año_Edicion , type = "response")
emmeans(ModeloA, pairwise ~ Nivel_de_instrucción , type = "response")
emmeans(ModeloA, pairwise ~ Indice_NBI_hogar_dic , type = "response")
emmeans(ModeloA, pairwise ~ Genero , type = "response")
emmeans(ModeloA, pairwise ~ Quintil_ingresos, type = "response" )



#Graficos de efectos marginales:

# Efectos marginales para la triple interacción Genero:Quintil_ingresos:Año_Edicion
efecto_triple <- allEffects(Mod5_interaccion, focus = "Genero:Quintil_ingresos:Año_Edicion")

# Crear un dataframe con las variables correctas
df_triple <- data.frame(
  Año_Edicion = factor(rep(c("2009", "2013", "2018"), each = 8 * 4)),  # Ajusta según tus años y niveles
  Genero = factor(rep(c("Masculino", "Femenino"), each = 8 * 3)),  # Ajusta según tus géneros
  Quintil_ingresos = factor(rep(c("Bajo", "Medio", "Alto"), times = 8 * 2)),  # Ajusta según tus quintiles
  fit = runif(7),  # Aquí pondrías los valores reales de fit
  lower = runif(7),  # Aquí pondrías los valores reales de lower
  upper = runif(7)  # Aquí pondrías los valores reales de upper
)


##------------------------------------------------------------------------------
modeloB <- glmmTMB(Cumple_No_Cumple_FyV ~ Genero*Quintil_ingresos + Indice_NBI_hogar_dic + Quintil_ingresos*Año_Edicion + Edad + Nivel_de_instrucción   +(1|Provincia ), ENFR_temporal, family = binomial())
summary(modeloB)
drop1(modeloB, test="Chisq")
AIC(modeloB)
emmeans(modeloB, pairwise ~ Genero, type = "response" )


#-------------------------------------------------------------------------------



                                   ### MODELO B: con interaccion  doble Genero*Quintil de ingreso y Genero*Año de edicion ### 


#(LO CORRI SIN  "Terciles_NBI_provincial" YA QUE EL MODELO COMPLETO ESTABA TENIENDO PROBLEMAS DE CONVERGENCIA)
# LO VOLVI A CORRER Y LA VARIABLE "Terciles_NBI_provincial" DIO SIGNIFICATIVA!!!!! LA DEJAMOS, FIUF

ModeloB <- glmmTMB(Cumple_No_Cumple_FyV ~ Genero*Quintil_ingresos + Quintil_ingresos*Año_Edicion + Rango_edad + Nivel_de_instrucción + Indice_NBI_hogar_dic  + (1|Provincia ), ENFR_t_NBI_CNA, family = binomial)
# + Terciles_NBI_provincial

## Salidas de modelo:

summary(ModeloB)

drop1(ModeloB, test="Chisq")

AIC(ModeloB)


  ###Interacciones###

# Genero | Quintil_ingresos: Comparaciones dentro de cada nivel de Quintil_ingresos: COMPARACIONES A PRIORI.
# Genero * Quintil_ingresos: Todas las comparaciones cruzadas entre los niveles de Genero y Quintil_ingresos: COMPARACIONES A POSTERIORI.


# Genero y Quintil
emmeans(ModeloB, pairwise ~ Genero|Quintil_ingresos, type = "response" )
plot(emmeans(ModeloB, pairwise ~ Genero|Quintil_ingresos), comparison = TRUE, type = "response")

# emmeans(ModeloB, pairwise ~ Quintil_ingresos|Genero, type = "response" )
# plot(emmeans(ModeloB, pairwise ~ Quintil_ingresos|Genero), comparison = TRUE, type = "response")

# Quintil y Año de edicion
emmeans(ModeloB, revpairwise ~ Quintil_ingresos|Año_Edicion, type = "response")
plot(emmeans(ModeloB, pairwise ~ Quintil_ingresos|Año_Edicion), comparison = TRUE, type = "response")

# emmeans(ModeloB, revpairwise ~ Año_Edicion|Quintil_ingresos, type = "response")
# plot(emmeans(ModeloB, pairwise ~ Año_Edicion|Quintil_ingresos), comparison = TRUE, type = "response")




  ### Emmeans y comparaciones generales (con intervalos de confianza):

  # Quintil*Año:
em_IC_QuintilVSaño <- emmeans(ModeloB, revpairwise ~ Quintil_ingresos|Año_Edicion, type = "response")
confint(contrast(em_IC_QuintilVSaño, method = "revpairwise", adjust = "none"), level = 0.95)

  # Genero*Quintil:
em_IC_generoVSquintil <- emmeans(ModeloB, pairwise ~ Genero|Quintil_ingresos, type = "response")
confint(contrast(em_IC_generoVSquintil, method = "pairwise", adjust = "none"), level = 0.95)
confint(em_IC_generoVSquintil, method = "pairwise", adjust = "none", level = 0.95)


# Variables aisladas:
emmeans(ModeloB, revpairwise ~ Rango_edad  , type = "response")
plot(emmeans(ModeloB, pairwise ~ Rango_edad, type = "response"), comparison = TRUE)

emmeans(ModeloB, revpairwise ~ Nivel_de_instrucción , type = "response")
plot(emmeans(ModeloB, pairwise ~ Nivel_de_instrucción, type = "response"), comparison = TRUE)

emmeans(ModeloB, pairwise ~ Indice_NBI_hogar_dic , type = "response")
plot(emmeans(ModeloB, pairwise ~ Indice_NBI_hogar_dic , type = "response"), comparise = TRUE)

### Grafico final para congreso ###
# Quintil*Año:
em_IC_QuintilVSaño <- emmeans(ModeloB, revpairwise ~ Quintil_ingresos|Año_Edicion, type = "response")
Emm_df <- as.data.frame(em_IC_QuintilVSaño$emmeans)
names(Emm_df)

ggplot( Emm_df, mapping = aes( x = Quintil_ingresos, y = prob, color = Año_Edicion))+
  geom_point(size = 4)+
  geom_errorbar(aes(ymin = asymp.LCL , ymax= asymp.UCL ), 
                width = 0.1)+
  labs(
    title = "Probabilidad en el CFyV segun quintil de pertenencia para las tres ediciones",
    fill = "Año de edicion",
    x = "Quintil de ingreso",
    y = "Probabilidad de un CFyV minimo recomendado",
    caption = ""
  ) +
  theme_bw()+
  



          

### Colinealidad###

car :: vif(ModeloB)


#Graficos de efectos marginales:

# Efectos marginales de la interacción Género:Quintil_ingresos
efecto_genero_quintil <- allEffects(ModeloB, focus = "Genero|Quintil_ingresos")

# Efectos marginales de la interacción Género:Año_edicion
efecto_genero_anio <- allEffects(ModeloB, focus = "Quintil_ingresos:Año_Edicion")

# Graficar los efectos marginales para Genero:Quintil_ingresos
plot(efecto_genero_quintil, main="Efecto Marginal: Género y Quintil de Ingresos")

# Graficar los efectos marginales para Genero:Año_edicion
plot(efecto_genero_anio, main="Efecto Marginal: Quintil de ingresos y Año de Edición")





##------------------------------------------------------------------------------



                               ### MODELO C con interaccion: Dos interacciones dobles (genero*Quintil y  genero*anio)###

ModeloC <- glmmTMB(Cumple_No_Cumple_FyV ~ Genero*Quintil_ingresos + Genero*Año_Edicion + Rango_edad + Nivel_de_instrucción  + Indice_NBI_hogar_dic + Terciles_NBI_provincial + (1|Provincia ), ENFR_t_NBI_CNA, family = binomial)


## Salida de modelos: 
summary(ModeloC)

drop1(ModeloC, test = "Chisq")

AIC(ModeloC)

## Comparaciones:
#Interacciones:

emmeans(ModeloC, pairwise ~ Genero|Quintil_ingresos, type = "response")
#emm_interaccion_mod2 <- emmeans(ModeloC, pairwise ~ )
#plot(emm_interaccion_mod2, comparison = TRUE, type = "response")

# Variables aisladas:
emmeans(ModeloC, pairwise ~ Rango_edad  , type = "response")
emmeans(ModeloC, pairwise ~ Año_Edicion , type = "response")
emmeans(ModeloC, pairwise ~ Nivel_de_instrucción , type = "response")
emmeans(ModeloC, pairwise ~ Indice_NBI_hogar_dic , type = "response")


#Graficos de efectos marginales:

# Efectos marginales de la interacción Género:Quintil_ingresos
efecto_genero_quintil <- allEffects(ModeloC, focus = "Genero:Quintil_ingresos")

# Efectos marginales de la interacción Género:Año_edicion
efecto_genero_anio <- allEffects(ModeloC, focus = "Genero:Año_Edicion")

# Graficar los efectos marginales para Genero:Quintil_ingresos
plot(efecto_genero_quintil, main="Efecto Marginal: Género y Quintil de Ingresos")

# Graficar los efectos marginales para Genero:Año_edicion
plot(efecto_genero_anio, main="Efecto Marginal:genero y Año de Edición")




                                                            ### Colinealidad###
car :: vif(ModeloC)


##------------------------------------------------------------------------------



                                           ### MODELO D con interaccion Genero*Quintil de ingresos ###


ModeloD <- glmmTMB(Cumple_No_Cumple_FyV ~ Genero*Quintil_ingresos + Rango_edad + Año_Edicion + Nivel_de_instrucción  + Indice_NBI_hogar_dic + Terciles_NBI_provincial + (1|Provincia ), ENFR_t_NBI_CNA, family = binomial) 


## Salidas de modelo:

summary(ModeloD)

drop1(ModeloD, test="Chisq") 
#Su propósito principal es evaluar el efecto de cada término sobre el ajuste del modelo y determinar si su eliminación deterioraría significativamente el ajuste.

AIC(ModeloD)

## Comparaciones:

#Interacciones:
emmeans(ModeloD, pairwise ~ Genero|Quintil_ingresos)
emmeans(ModeloD, pairwise ~ Genero|Quintil_ingresos*Año_Edicion, type = "response")

emm_interaccion_mod1 <- emmeans(ModeloD, pairwise ~ Genero|Quintil_ingresos)
plot(emmeans(ModeloD, pairwise ~ Genero|Quintil_ingresos), comparison = TRUE, type = "response")

# Variables aisladas:
#Escala del predictor lineal:
emmeans(ModeloD, pairwise ~ Rango_edad  )
emmeans(ModeloD, pairwise ~ Año_Edicion )
emmeans(ModeloD, pairwise ~ Nivel_de_instrucción )
emmeans(ModeloD, pairwise ~ Indice_NBI_hogar_dic )

#Escala de VR:
emmeans(ModeloD, pairwise ~ Rango_edad  , type = "response")
emmeans(ModeloD, pairwise ~ Año_Edicion , type = "response")
emmeans(ModeloD, pairwise ~ Nivel_de_instrucción , type = "response")
emmeans(ModeloD, pairwise ~ Indice_NBI_hogar_dic , type = "response")
emmeans(ModeloD, pairwise ~ Genero  , type = "response")
emmeans(ModeloD, pairwise ~ Quintil_ingresos  , type = "response")



#Graficos de efectos marginales:

# Efectos marginales de la interacción Género:Quintil_ingresos
efecto_genero_quintil <- allEffects(ModeloD, focus = "Genero:Quintil_ingresos")
plot(efecto_genero_quintil, main="Efecto Marginal: Género y Quintil de Ingresos")





## Variable de efectos aleatorios:

## Colinealidad?
car :: vif(ModeloD)

#-------------------------------------------------------------------------------

                                                                 ### Comparacion entre Modelos ####
AIC(modeloA, modeloB, modeloC, modeloD)
anova(modeloA, modeloB, modeloC, modeloD)


#-------------------------------------------------------------------------------

### Modelo  con interaccion entre Genero*Año de edicion: MODELO QUE SOLE NO TUVO EN CUENTA

Mod_interaccion  <- glmmTMB(Cumple_No_Cumple_FyV ~ Genero*Año_Edicion + Quintil_ingresos + Rango_edad  + Nivel_de_instrucción + Indice_NBI_hogar_dic + Terciles_NBI_provincial +(1|Provincia ), ENFR_t_NBI_CNA, family = binomial)



#summary(ModeloC)$varcor # El resumen de las varianzas de los efectos aleatorios muestra una desviación estándar de 0.263 para el intercepto de Provincia. Esto indica que hay cierta variabilidad entre provincias en el efecto del intercepto, pero no proporciona información completa sobre el ajuste del modelo ni su convergencia.


## Variable de efectos aleatorios:

## Colinealidad?
car :: vif(Mod2_interaccion)


## Salidas de modelo:

summary(Mod2_interaccion)
drop1(Mod2_interaccion, test = "Chisq")


#Interacciones:
emmeans(Mod1_interaccion, pairwise ~ Genero:Quintil_ingresos)
plot(emm_interaccion_mod1, comparison = TRUE, type = "response")

# Variables aisladas:
emmeans(Mod_interaccion, pairwise ~ Rango_edad  , type = "response")
emmeans(Mod_interaccion, pairwise ~ Año_Edicion , type = "response")
emmeans(Mod_interaccion, pairwise ~ Nivel_de_instrucción , type = "response")
emmeans(Mod_interaccion, pairwise ~ Indice_NBI_hogar_dic , type = "response")
emmeans(Mod_interaccion, pairwise ~ Genero , type = "response")
emmeans(Mod_interaccion, pairwise ~ Quintil_ingresos, type = "response" )


#Graficos de efectos marginales:

# Efectos marginales de la interacción Género:Quintil_ingresos
efecto_genero_quintil <- allEffects(Mod1_interaccion, focus = "Genero:Quintil_ingresos")
plot(efecto_genero_quintil, main="Efecto Marginal: Género y Quintil de Ingresos")


#-------------------------------------------------------------------------------
#_________________________________________________________________________________________________________________________________________________________________________________
#_________________________________________________________________________________________________________________________________________________________________________________

                                                          ### Segunda parte: AMBIENTAL (Ojala sea AMBIENTAL + TEMPORAL)

#_______________________________________________________________________________




