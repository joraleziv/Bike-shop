
# importamos datos a R --> dataset_bike_shop
# seleccionando la variable --> Var Discreta Adq Bicicleta o ST Ventas Totales

# en primer lugar creamos una copia de los datos
# para las pestañas: ST Ventas Totales o Var Discreta Adq Bicicleta
# en los primeros apartados utilizaremos --> Var Discreta Adq Bicicleta

datosbici <- dataset_bike_shop



# Análisis preliminar de los datos:
str(datosbici)
summary(datosbici)
dim(datosbici)
View(datosbici)

# Observaciones:

# hay un total de 18.484 observaciones y 19 columnas (o variables)
# Obtenemos las medias, máximos y mínimos para cada variable
# tenemos variables categóricas (solo toman determinados valores)
# NO existen celdas vacías (NA) o con datos nulos (NULL)
# existe la posibilidad de que R no encuentre celdas vacías en
# aquellas variables que NO tengan formato numérico
# las variables son de caracter: numérico, texto y fecha
# Problema, en las columnas DateFirstPurchase y BirthDate NO se indica dicha variable como "fecha"
# hay variables categóricas que R considera númericas (como "BikePurchase")

# obetenemos la varianza (fijarnos solo en las variables numericas)
var(datosbici)

# se realiza un diagrama de caja para ver el Gasto Total por países (o zonas geográficas):
boxplot(datosbici$TotalAmount~datosbici$Country)

# se puede apreciar que países tienen un mayor o menor gasto, incluso gasto medio del total
# podemos descartar a priori ciertos datos anómalos (outliers)

# por otro lado se realiza un histograma para ver la frecuencia del gasto total:
hist(datosbici$TotalAmount)

# sería interesante ver la relación entre variables
# por ejemplo entre el el gasto total y la edad del cliente

# covarainza:
cov(datosbici$TotalAmount,datosbici$Age)

# la relación entre el gasto total y la edad es negativa, es decir, a mayor edad menor es el gasto total

# coeficiente de correlación (entre -1 y 1):
cor(datosbici$TotalAmount,datosbici$Age)

# coeficiente = -0,049 --> relación negativa y poco significativa ya que está más cerca del cero

# para una mejor intepretación visual dibujamos en una gráfica el coeficiente de correlación:
plot(datosbici$TotalAmount,datosbici$Age)


# Por otro lado, calculamos el porcentaje de los clientes que
# han comprado una bicicleta y los que NO han comprado
# vemos el procentaje de 0 y 1 en la variable de BikePurchase

prop.table(table(datosbici$BikePurchase))

# el 51% NO han comprado bici y el 49% si han comprado bici


# A continuación se procede a realizar un Análisis de ### REGRESION ###

# Este analisis nos permite indentificar de una forma más nítida la relación
# entre 2 o más variables
# en este tipo de análisis debe existir siempre una variable dependiente (endógena)
# y 1 o varias variables independientes (exógenas)
# a modo de ejemplo:

# variable dependiente = TotalAmount (gasto total)
# Variable indpendientes: Age (edad)

# regresión:
lm(datosbici$TotalAmount~datosbici$Age)

# intepretación, cuando el gasto total aumenta en 2131,94 unidades monetarias
# la edad se reduce en -9 años

# pero como las magnitudes de las variables son diferentes
# procedemos a calcular una REGRESION LOGARITMICA (eliminando las magnitudes),
# pasando a interpretar en porcentajes (%) y facilitando la compresión

lm(log(datosbici$TotalAmount)~log(datosbici$Age))

# interpretación, cuando el gasto total aumenta un 90,43% 
# el porcentaje de edad se reducirá en un 9% de ese 90,43% del gasto total

# Se procede a realizar un contraste de hipótesis para avalar los resultados obtenidos

# CONTRASTE HIPOTESIS:

estimacion_bici <- lm(log(datosbici$TotalAmount)~log(datosbici$Age))

summary(estimacion_bici)

# tenemos unos resultados más detallados
# en los resultados fijarnos en la columna Pr(>|t|) 
# la columna Pr(>|t|) indica el numero de veces que se cumple la hipótesis
# la hipótesis nula = que el valor de parámetro sea cero, sea nulo
# si no se rechaza la hipótesis nula, se dice que los parámetos NO son significativos

# nota: para que un parámetro sea significativo como mucho tiene que ser < o = a 0,1 (10%)

# en este supuesto, los resultados obtenidos son significativos, 
# ya que al 99,99% NO se cumple la hipótesis nula.


 
# Usamos un modelo de regresión logística y un árbol de decisión. 
# (utilizamos la pestaña Var Discreta Adq Bicicleta)

# para este apartado utilizaremos --> "datosbici"

### CLASIFICAR ###  --> BikePurchase, donde 1 = si ha comprado y 0 = NO ha comprado

########### REGRESION LOGISTICA (MODELO LOGIT) #############

# se procede a crear un MODELO LOGIT para clasificar por categorías o predecir
# que clientes van a comprar o no una bicicleta

# Donde la variable dependiente --> BikePurchase
# cumple la condición de ser una variable categórica

# Utilizar --> "as.factor" para evitar problemas con algunas variables categoricas que no reconco R

# el resto de variables que utilizaremos son: MaritalStatus, Age, 
# YearlyIncome, Gender, TotalChildren, Education, Occupation,
# HomeOwnerFlag y NumberCarsOwned

modelologit_datosbici <- glm(datosbici$BikePurchase~datosbici$MaritalStatus
                             +datosbici$Age
                             +datosbici$YearlyIncome
                             +datosbici$Gender
                             +datosbici$TotalChildren
                             +datosbici$Education
                             +datosbici$Occupation
                             +datosbici$HomeOwnerFlag
                             +datosbici$NumberCarsOwned,
                             family = "binomial")

summary(modelologit_datosbici)


# interpretación:

# En primer lugar, analizamos la significación de los parámetros tomando como referencia el pvalue o Pr(>|z|), 
# pero en una regresión logística NO se puede interpretar directamente la magnitud del coeficiente, 
# solo se puede interpretar el signo (negativo o positivo). Sin embargo, es posible interpretar 
# la magnitud aplicando la siguiente fórmula “1-exp(coeficiente)”.

# observamos que la mayoría de las variables son significativas

# a modo de ejemplo aplicamos la siguiente fórmula para interpretar la magnitud:

# MaritalStatus
1-exp(0.291752) 
# = -0.3387717
# existe un 33,87% menos de probabilidad de que los solteros compren una bicilceta
# en comparación con los clientes casados


########## ARBOL DE DECISION ###############

# para calcular el arboL de decision necesitamos instalar 2 librerias
# RPART y RPART.PLOT

install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

# varaible dependiente --> BikePurchase
# el resto de variables que utilizaremos son: MaritalStatus, Age, 
# YearlyIncome, Gender, TotalChildren, Education, Occupation,
# HomeOwnerFlag y NumberCarsOwned

# dato: como observamos con el comando "str" R considera
# la variable "BikePurchase" como NUMERICA
# con "as.factor" le indicamos que es una variable CATEGORICA

modeloarbol_datosbici <- rpart(as.factor(BikePurchase)~MaritalStatus
                               +Age
                               +YearlyIncome
                               +TotalChildren
                               +Gender
                               +Education
                               +Occupation
                               +HomeOwnerFlag
                               +NumberCarsOwned,
                               data=datosbici,
                               method = "class")
                               
   
# para analizar los resultados del árbol es mejor dibujarlo
# para ellos utilizamos rpart.plot

rpart.plot(modeloarbol_datosbici)

# Observamos las variables más relevantes del modelo con "summary"

summary(modeloarbol_datosbici)

# Variables importantes:

# 1) NumberCarsOwned
# 2) Age
# 3) TotalChildren
# 4) Education
# 5) Occupation
# 6) YearlyIncome


# Interpretación TOP variables más relevantes en el modelo "árbol de decisión"

# Del total de clientes, solo el 49% ha comprado una bicicleta

# Del total de la muestra el 51% de los clientes tienen 2 coches en propiedad,
# pero 49% de los clientes NO tiene 2 coches en propiedad (y el 41% de estos ha comprado una bicicleta)
# De ese 49% de clientes, el 45% tiene menos 69 años (y de estos el 39% ha comprado bicicleta)
# De ese 45% de clientes, el 41% tiene menos de 4 hijos (y de estos el 36% ha comprado bicicleta)
# De ese 41% de clientes, el 33% tiene una edad superior a 48 años (y de estos el 33% ha comprado bicicleta)



# Una vez tenemos el modelo creado hay que validar (si acierta o no)
# comparando los datos reales con la predicción
# a través de la MATRIZ DE CONFUSION

# VALIDACION MODELOS:

###### MATRIZ DE CONFUSION DEL MODELO LOGIT #######

#datos reales
table(datosbici$BikePurchase)

#datos predicción
prediccion_datosbici <- predict(modelologit_datosbici, type = 'response')

View(prediccion_datosbici)

# recordemos que nuestra variable dependiente (la variable real) "BikePurchase" solo toma 2 valores
# pero nuestra predicción toma valores ENTRE 0 y 1
# por lo cual tenemos que cambiar los valores de la predicción (entre 0 y 1) a valores de 0 o 1 (únicamente)
# aquellos valores que se acerquen más valores a 0 serán 0 y los que se acerquen más a 1 serán 1
# es decir, tenemos que CODIFICAR nuestra predicción a 0 y 1

#¿como lo hacemos?

prediccion_datosbici_cod <- ifelse(prediccion_datosbici>0.5,1,0)

View(prediccion_datosbici_cod)

# observamos que ya solo se toman los valores 0 y 1 en los resultados de la prediccion (codificada)

# ahora si es posible comparar los datos reales y los datos predichos (prediccion codificada)
# en otras palabras, ya se puede calcular la MATRIZ DE CONFUSION

# MATRIZ DE CONFUSION (modelo logit):

table(datosbici$BikePurchase,prediccion_datosbici_cod)

# Interpretacion matriz de confusión:

#ACIERTOS = diagonal matriz = 5691 + 5890 = 11581
#FALLOS = 3242 + 3661 = 6903
#TOTAL observaciones = 11581 + 6903 = 18484

# métrica de exactitud (Accuracy) --> porcentajes de aciertos en total = aciertos / total
# 11581 / 18484 = 0.6265419  --> el 62,65% de las veces acierta el modelo logit


# Continuamos con la validación del modelo a través de la matriz de confusión


###### MATRIZ DE CONFUSION DEL MODELO ARBOL DE DECISION #######

#datos reales
table(datosbici$BikePurchase)

#datos predicción (predicción 2 --> para ARBOL DECISION) y en type ="class"

prediccion_datosbici2 <- predict(modeloarbol_datosbici, type = "class")


# MATRIZ DE CONFUSION (modelo arbol de decision):

table(datosbici$BikePurchase,prediccion_datosbici2)

# Interpretación matriz de confusión:

#ACIERTOS = diagonal matriz = 7137 + 4604 = 11741
#FALLOS = 4528 + 2215 = 6743
#TOTAL observaciones = 11741 +6743 = 18484

# métrica de exactitud (Accuracy) --> porcentajes de aciertos en total = aciertos / total
# 11741 / 18484 = 0.635198  --> el 63,51% de las veces acierta el modelo árbol de decisión


# Para obtener una clasificación de las variables de forma general es conveniente 
# utilizar el Modelo de Árbol de Decisión, ya que ofrece de forma más eficiente una visión del total de la muestra

# Relevancia o importancia de variables según el modelo


# RELVANCIA VARIABLES en el modelo árbol de decisión:

# Observamos las variables más relevantes del modelo con "summary"

summary(modeloarbol_datosbici)

# Variables importantes:

# 1) NumberCarsOwned
# 2) Age
# 3) TotalChildren
# 4) Education
# 5) Occupation
# 6) YearlyIncome

# Por otro lado, también se observa la relevancia de las variables 
# en el dibujo del árbol de decisión, al aplicar el siguiente comando

rpart.plot(modeloarbol_datosbici)



# RELEVANCIA VARIABLES en el modelo logit:

# Por otro lado, si lo que le interesa al usuario es comparar una variable respecto a otra 
# se puede utilizar el Modelo Logit, ya que permite interpretar la relación 
# y la magnitud utilizando la fórmula “1-exp(estimate)”de una variable.
# como se comentaba en el apartado anterior

# se utiliza el comando "summary" para ver una clasificación de las variables

summary(modelologit_datosbici)

# Ahora vamos a dividir la muestra en dos subconjuntos (entrenamiento a 80% y validación a 20%)

# Utilizar la librería caTools
install.packages("caTools")
library(caTools)
# creamos una variable que divida la muestra, la vamos a llamar "division"
division <- sample.split(datosbici$BikePurchase, SplitRatio = 0.8)
View(division)

entrenamiento <- subset(datosbici, division==TRUE)
# entrenamiento tiene 14788 observaciones ( 80% de las observaciones de la muestra)
prop.table(table(entrenamiento$BikePurchase))
# en entrenamiento 50.59% no han comprado bici y un 49.04% sí lo han hecho. El resultado es similar a la muestra completa.
# ahora calculamos el conjunto de validación (que es el 20%)
validacion <- subset(datosbici, division==FALSE)
# vemos que el conjunto de validación o test está formado por 3696 observaciones( el 20% del total)
prop.table(table(validacion$BikePurchase))
# se confirma que los porcentajes se mantienen casi idénticos que la muestra de entrenamiento.

# Creamos el modelo con la muestra de entrenamiento

modelologit_entrenamiento <- glm(BikePurchase~MaritalStatus
                             +Age
                             +YearlyIncome
                             +Gender
                             +TotalChildren
                             +Education
                             +Occupation
                             +HomeOwnerFlag
                             +NumberCarsOwned, data = entrenamiento,
                             family = "binomial")
summary(modelologit_entrenamiento)

# Para ver si el modelo es óptimo, calculamos la matriz de confusión, para ello, debemos de hacer la predicción
prediccion_logit_entrenamiento <- predict(modelologit_entrenamiento, type = "response")
# lo codificamos
prediccion_logit_entrenamiento_cod <- ifelse(prediccion_logit_entrenamiento>0.5,1,0)
library(caret)
install.packages("ISLR")
library(ISLR)
install.packages("MASS")
library(MASS)
install.packages("dplyr")
library(dplyr)
confusionMatrix(as.factor(entrenamiento$BikePurchase),as.factor(prediccion_logit_entrenamiento_cod))

# la métrica de precisión o accuracy da un 63%, igual que si no hubiéramos hecho una separación de las observaciones entre entrenamiento y test
# probamos el modelo con las observaciones de test
prediccion_logit_validacion <- predict(modelologit_entrenamiento, newdata = validacion, 
                                       type="response")
# como estamos en modelos logit, hay que codificar los datos
prediccion_logit_validacion_cod <- ifelse(prediccion_logit_validacion>0.5,1,0)
# ahora, calculo otra vez, la matriz de confusión, pero con los datos de validación
confusionMatrix(as.factor(validacion$BikePurchase),as.factor(prediccion_logit_validacion_cod))

# comprobamos que con los datos de la muestra de test, los resultado son los mismos en cuanto a accuracy.



# Podemos definir tipologías de clientes diferentes 


### APRENDIZAJE NO SUPERVISADO ###

# Aprendizaje NO supervisado = problema de clusterización o agrupación

# analizamos la estructura de los datos
str(datosbici)

# existen variables categóricas que R las interpreta como numéricas (BikePurchase)
# también existen variables de fecha (date) que R no interpreta como tales

# el análisis cluster en principio solo funciona con DATOS NUMERICOS
# por tanto es interesante eliminar las columnas NO numéricas

# tenemos que dibujar/representar gráficamente los datos para analizar
# que variables se pueden utilizar para el análisis cluster

# Por lo cual, se procede a eliminar las variables o columnas NO numéricas
# pero como son bastantes las variables NO numéricas, 
# seleccionaremos solo las variables numéricas que nos interesan

# utilizamos para lo expuesto anteriormente la libreria DPLYR

# instalamos libreria DPLYR 
install.packages("dplyr")
library(dplyr)

# y usaremos el comando SELECT para extraer las columnas o variables
# vamos a extraer COLUMNAS o VARIABLES NUMERICAS: TotalAmount,Age ,NumberCarsOwned y TotalChildren

str(datosbici)

datosbici_extraidos <- select(datosbici, TotalAmount, Age, NumberCarsOwned, TotalChildren)

plot(datosbici_extraidos)

table(datosbici$BikePurchase)

# a priori se puede observar ciertos grupos separados
# esto se puede hacer también con un análisis de correlación
# hay que recordar que la "correlación" solo se puede hacer con datos numéricos

cor(datosbici_extraidos)

# se puede comparar el tipo de relación entre las variables

# a continuación, se procede a dibujar las observaciones coloreadas 
# respecto la variable BikePurchase (si el cliente ha comprado o no bici)

# recordatorio, indicar que la variable "BikePurchase" es CATEGORICA

plot(datosbici_extraidos,col=as.factor(datosbici$BikePurchase))

# una vez familiarizados con los datos podemos pasar a realizar


####### ANALISIS CLUSTER ########

# se utilizará el método --> #### K-MEANS #### para calcular los subgrupos

# este metodo esta por defecto en R --> comando --> "kmeans"

# EL METODO DE KMEANS --> NECESITA DETERMINAR A PRIORI EL NUMERO DE CLUSTERS (o subgrupos)

# le indicamos por ejemplo 2 clusters

# recordemos que kmeans solo funciona con VARAIBLES NUMERICAS, 
# por eso usamos los "datosbici_extraidos"

kmeans(datosbici_extraidos,2)

# K-means clustering with 2 clusters of sizes 14044, 4440

# nos indica el número de grupos y su tamaño:

# cluster 1 --> 14044
# cluster 2 --> 4440

# las medias pos clusters:

# Cluster means:
#     TotalAmount      Age       NumberCarsOwned  TotalChildren
# 1     544.2789     58.69681        1.525491       1.903019
# 2    4890.7261     57.54459        1.430631       1.658784

# se observa que en el cluster 1 la media del TotalAmount (gasto total) 
# es significativamente superior a la del cluster 2 y
# la media de edad (Age), NumberCarsOwned y TotalChildren
# son smiliares en todos los clusters

#  Métricas WWS
#  Within cluster sum of squares by cluster:
# [1]  8867117690 10806845166
# (between_SS / total_SS =  76.4 %)

# esto indica la dispersión (76,4%)


# se procede a ##### DETERMINAR el NUMERO de CLUSTERS OPTIMOS #######

### OPCION 1 ###


# vamos a utilizar 2 librerias

install.packages("factoextra")
library(factoextra)

#esta libereria tiene un comando para determinar el numero de clusters
# comando --> fviz_nbclust

fviz_nbclust(datosbici_extraidos, kmeans)

# indica que el número óptimo de cluster es 2

# pero si tenemos en cuenta la métrica WSS para determinar el numero de clusters

fviz_nbclust(datosbici_extraidos, kmeans, method = "wss")

# obtenemos el ###### METODO ELBOW (Método del Codo) #######

# método que indica que el número óptimo de cluster está entre 2 y 3


### OPCION 2 ###

# Otra libreria (para determinar también el número optimo de clusters)
# es la libreria NbClust

install.packages("NbClust")
library(NbClust)

# esta libreria permite determinar automaticamente el numero de clusters optimos

# seleccionamos por ejemplo como minimo 2 cluster y como maximo 4 clusters
# de la base de datos que escogamos y usando el metodo kemeans

NbClust(datosbici_extraidos, min.nc = 2 , max.nc = 6, method = "kmeans")

### NO ME CARGA EL COMANDO ###


### OPCION 3 ###

# dejando a un lado las librerias o paquetes de R
# a veces es mejor utilizar el conocimiento, la interpretación o intuición

# vamos a suponer que optamos por 2 clusters
# usamos kmeans (recuerda ejecutarlo varias veces)

kmeans(datosbici_extraidos,2)

# K-means clustering with 2 clusters of sizes 14044, 4440

# Within cluster sum of squares by cluster:
#  [1]  8867117690 10806845166
# (between_SS / total_SS =  76.4 %)

summary(datosbici_extraidos,2)

agrupacion_bici2 <- kmeans(datosbici_extraidos,2)

# comprobamos cuantos clientes hay por clusters

# obtenemos el número de clientes por cluster:

table(agrupacion_bici2$cluster)

# 1     2 
# 14044  4440 

# datos reales, clientes que han comprado y no han comprado bicicleta

table(datosbici$BikePurchase)

# 0    1 
# 9352 9132 

# se compara el resultado de aplicar el análisis de cluster 
# con los datos reales

table(datosbici$BikePurchase, agrupacion_bici2$cluster)

#     1    2
# 0 9352     0
# 1 4692  4440

# interpretacion matriz:

# en el cluster 1 existe un total de 9352 + 4692 = 14044 clientes
# y en el cluster 2 un total de 4440 clientes


# el análisis cluster es SUCPETIBLE O SENSIBLE a la magnitud de las variables
# (como se miden las variables, en euros, volumen, gramos, etc)

# para resolver este problema una solución es 
# #NORMALIZAR LOS DATOS (quitamos magnitudes)
# para ellos utilizamos el comando --> scale

#al usar este comando los datos generados NO son dataframe

datosbici_normalizados <- scale(datosbici_extraidos)

kmeans(datosbici_normalizados,2)

# K-means clustering with 2 clusters of sizes 10037, 8447

# Métrica WWS 
# Within cluster sum of squares by cluster:
#  [1] 26062.57 24484.37
# (between_SS / total_SS =  31.6 %)

#guardamos datos normalizados

agrupacion_bici2_normalizados <- kmeans(datosbici_normalizados,2)

# ya es posible comparar los resultados normalizados con la realidad:

table(datosbici$BikePurchase,agrupacion_bici2_normalizados$cluster)

#      1    2
# 0  4086 5266
# 1  5948 3184

# interpretación matriz:

# en el cluster 1 existe un total de 4086 + 5948 = 10034 clientes
# y en el cluster 2 hay un total de 5266 + 3184 = 8450 clientes

# estos resultado se ajustan más a la realidad
# como podemos comprobar con el siguiente comando:

# datos reales (clientes que han comprado y no han comprado bicicleta)

table(datosbici$BikePurchase)

# 0    1 
# 9352 9132 

# conclusión:

# comparando los resultados normalizando y NO normalizando los datos
# los resultados más ajustados a la realidad son los NORMALIZADOS
# ya que el número de clientes por cluster se acerca a los datos
# reales de los clientes que han comprado y NO han comprado una bicicleta


########################################################################################################
# 5) Realice una predicción de las ventas totales 
# (utilice la pestaña ST Ventas totales, la primera columna) 
# para los próximos 2 meses.
########################################################################################################

# CUIDADO --> cambiamos de variable --> ST Ventas totales

# importamos datos a R --> DataSet_SQL_Analisis_Masivo_de_Datos
# seleccionando esta vez la variable -->  ST Ventas Totales

# creamos una copia de los datos

datosst <- DataSet_SQL_Analisis_Masivo_de_Datos

View(datosst)

# solo trabajaremos con la primera columna --> "sales....2"
# para realizar la predicción a 2 meses

# analisis preliminar de los datos

str(datosst)
summary(datosst)

# en este supuesto trabajamos con la variable o columna "sales....2" com formato numerico
# sin emabrgo, las demás columnas son numéricas, pero R NO reconoce su formato númerico
# esto es un problema a la hora de buscar celdas vacías (NA)
# solución: darlesa dichas columnas un formato numérico, 
# pero como NO trabajaremos con ellas no es necesario

# Respecto a la columna "sales....2", NO se encuentran celdas vacías

# inspeccionamos la columna
plot(datosst$Sales...2)

# como NO le hemos dato formato de serie temporal, 
# la representación o dibujo se ve de esa forma
# si queremos dibujarlo como una serie temporal, tenemos opciones:

plot.ts(datosst$Sales...2)

# los datos parecen mostrar que a partir del 30/05/2014 (fila 1903) 
# los datos estan MAL RECOGIDOS
# vamos a optar por eliminar las filas a partir de la 1093

datosst_1<- datosst[1:1093,]

plot.ts(datosst_1$Sales...2)

# para evitar problemas de ruidos en los datos (entre otros) es interesante
# agrupar los datos (en este caso agrupar las fechas para suavizar la serie)

# utilizamos la librería LUBRIDATE para trabajar con FECHAS

install.packages("lubridate")
library(lubridate)



# con esta libreria vamos a crear NUEVAS COLUMNAS de (semanas, meses, trimestres y años)

# para ello utilizamos el comando --> floor_date

# vamos a crear tres nuevas columnas: "dia", semana" y "mes"

# utilizamos como referencia la columna --> OrderDate (donde están las fechas)

datosst_1$dia <- floor_date(datosst_1$OrderDate, "day")

datosst_1$semana <- floor_date(datosst_1$OrderDate, "week")

datosst_1$mes <- floor_date(datosst_1$OrderDate, "month")

# comprobamos que las columnas de fechas se han creado perfectamente

View(datosst_1)

# el siguiente paso es agrupar los datos con esas fechas

# para ello usaremos la libreria --> DPLYR

install.packages("dplyr")
library(dplyr)

# vamos a agrupar las ventas por dia (la llamaremos ventas_dia)

ventas_dia <- datosst_1 %>%
  group_by(dia) %>%
  summarise(Sales...2=sum(Sales...2))

plot.ts(ventas_dia$Sales...2)

# vamos a agrupar las ventas por semana (la llamaremos ventas_semana)

ventas_semana <- datosst_1 %>%
  group_by(semana) %>%
  summarise(Sales...2=sum(Sales...2))

plot.ts(ventas_semana$Sales...2)

# vamos a agrupar las ventas por mes (la llamaremos ventas_mes)

ventas_mes <- datosst_1 %>%
  group_by(mes) %>%
  summarise(Sales...2=sum(Sales...2))

plot.ts(ventas_mes$Sales...2)


# en cualquiera de las frecuencias existe cierto grado de RUIDO
# para minimizar el ruido debemos extraer el componente tendencial
# la tendencia es la parte suavizada o alisada de los datos

# EXTRACCION COMPONENTE TENDENCIAL
# Para ello utilizamos la libreria --> mFilter
# permite extraer la tendencia sin perder mucha información

install.packages("mFilter")
library(mFilter)

# esta libreria contiene el filtro de --> Hodrick-Prescott

tendecia_mfilter <- hpfilter(ventas_mes$Sales...2, freq = 12)

# ahora ya solo queda sacar la tendencia

tendencia_hp <- tendecia_mfilter$trend

plot.ts(ventas_mes$Sales...2)

lines(tendencia_hp, col = "blue")


# ##### PREDICCION #### (A 2 MESES)

# Ya es posible hacer la predicción con ### ARIMA ### para los próximos 2 meses

# utilizamos la libreria

library(forecast)

# procedemos a realizar la predicción a 2 meses para la variable "sales...2"

prediccion_tendenica_hp <- auto.arima(tendencia_hp) 

preidccion_tendencia_hp_2 <- forecast(prediccion_tendenica_hp,2)

plot(preidccion_tendencia_hp_2)

