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
# 2) git commit -m "cambios del 21/08"
# 3) git push

#---

#_______________________________________________________________________________

ENFR_temporal <- read.csv("C:/Tesis/Datos/EsNsFR.csv", header = T, sep = ",", dec = ".")

NBI_CNA <- read.csv("C:/Tesis/Datos/CNA + NBI/NBI_Prov_Total_Y_CNA.csv", header = T, sep = ",", dec = ".")


#_______________________________________________________________________________

### Algunos arreglos en el dataset:

## Pasamos a factor todas las variables:

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

#---
## Transformamos Edad a numerico:

ENFR_temporal$Edad <- as.numeric(ENFR_temporal$Edad)

#---
## Ver que onda los Na`s:

sum(is.na(ENFR_temporal)) # La base de datos tiene 484 NA`s (RARO PORQUE NO HABIA ELIMINADO LOS NA´s DE LAS BASES INDIVIDUALES Y DEBERIAN HABER MAS)
colSums(is.na(ENFR_temporal)) # Vienen de Ingreso_mensual_pesos_hogar + Quintil_ingresos

ENFR_temporal <- na.omit(ENFR_temporal)

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
 
#_______________________________________________________________________________ 
 

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
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
 
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
 # M1g <-CFV ~ año + Género + Quintil de Ingreso + Nivel Educativo Alcanzado + CMV + Rango etario + NBI Provincial. TENGO QUE VER QUE ONDA PORQUE ACA ENTRA UNA VARIABLE DE OTRO DATASET

  M1g <- glmer(Cumple_No_Cumple_FyV ~ Genero + Rango_edad + Año_Edicion + Nivel_de_instrucción + Sit_laboral + Quintil_ingresos + Indice_NBI_hogar_dic + Porcentaje_hogar_NBI + (1|Provincia ), ENFR_temporal, family = binomial())
 
## Supuestos:
 
 # Variable de efectos aleatorios:
 
 # Colinealidad?
 
## Salidas de modelo:
 
 ## Ajuste del modelo: AIC/BIC
 
 #______________________________________________________________________________
 
 
                        ### MODELOS CON INTERACCION ###
# 
# Mod1_interaccion <- CFV ~ año + Género * Quintil de Ingreso + Nivel Educativo Alcanzado + CMV + Rango etario + NBI Provincial 
# 
# Mod2_interaccion <- CFV ~ año*Genero +  Quintil de Ingreso*Género + Nivel Educativo Alcanzado + CMV + Rango etario + NBI Provincial 
#  
# Mod3_interaccion <- CFV ~  genero * quintil_ingresos*año_edicion  + nivel_instruccion + NBI_Provincial + CarenciasVivienda + rango_edad + (1/ Provincia)





