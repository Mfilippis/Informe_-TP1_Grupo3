###### Trabajo Práctico 1 - Grupo 3 ######
##########################################


#Instalamos paquetes y librerías necesarias
library(readxl)
library(tidyverse)
install.packages("moments")
library(moments)
install.packages("dplyr")
library('dplyr')
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
install.packages("stats")

install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages('fastDummies')
library(fastDummies)
library(tidyverse)

# Ejercicio 1


## Inciso 1

flavors_of_cacao <- read_excel("flavors_of_cacao.xlsx", 
                               col_types = c("text","text","numeric","numeric","numeric",
                                             "text","numeric","text", "text"))
View(flavors_of_cacao)
datos<-flavors_of_cacao

### Le cambiamos el nombre a las columnas para que sean más accesibles
colnames(datos)<-c("cia","nombre","ref","año","porcentaje","pais","rating","tipo","origen")

### Análisis descriptivo
promedio<- mean(datos$rating)
minimo<- min(datos$rating)
maximo<- max(datos$rating)
mediana<- median(datos$rating)
d1<-data.frame(promedio,minimo,maximo,mediana)
cuantiles<- quantile(datos$rating)
d2<-data.frame(cuantiles)
intercuartil<- IQR(datos$rating)
d3<-data.frame(intercuartil)
desvio<- sd(datos$rating)
varianza<- var(datos$rating)
asimetria<- skewness(datos$rating)
kurtosis<- kurtosis(datos$rating)
d4<-data.frame(desvio,varianza,asimetria,kurtosis)
d5<-data.frame(d1,d2,d3)
d5

### Gráficos
par(mfrow=c(1,2))
hist(datos$rating,col="navy", main="Histograma" ,ylab="Frecuencia" ,xlab="Valores")
boxplot(datos$rating,col="gold" ,main="Gráfico de Box-Plot",horizontal=T,xlab="Valores")

### Outliers
Q1<-quantile(datos$rating,1/4)
Q3<-quantile(datos$rating,3/4)
Q3 + 1.5*intercuartil # Todos los valores que superen esta ecuación son outliers
Q1 - 1.5*intercuartil # Todos los valores que estén por debajo de la ecuación son outliers

### Datos faltantes
#### Columna "tipo"
tipo<-table(datos$tipo)
Porcentaje_datos_faltantes1<-tipo[1]/1795      
Porcentaje_datos_faltantes1
datos<-datos[,-8] # Se elimina porque tiene el 49 % de datos faltantes

#### Columna "origen"
origen<-table(datos$origen)
Porcentaje_datos_faltantes2<-origen[1]/1795      
Porcentaje_datos_faltantes2
datos<-datos # No se elimina por sólo 4% de datos faltantes
##### Reemplazamos las celdas vacias por "sin dato"
datos<-datos %>% mutate(origen = replace(origen, which(is.na(origen)), "NA"))
datos

#### Columna "ref"
datos<-datos[,-3] # Se elimina porque dado que no es de imprtancia para nuestro análisis
datos


## Inciso 2

### Promedio por año
Año_rating<-datos %>% 
  group_by(año=datos$año) %>% 
  summarise(promedio=mean(rating), cantidad=n()) 
Año_rating
Año_rating$año<-as.numeric(Año_rating$año) #VER#
#### Grafico
ggplot(data=Año_rating) +
  geom_point(mapping=aes(x=año, y=promedio)) +
  labs(x="Año", y="Rating promedio", title="Rating promedio por año")

### Promedio por porcentaje cacao
Porcentaje_rating<-datos %>% 
  group_by(porcentaje=datos$porcentaje) %>% 
  summarise(promedio=mean(rating), cantidad=n()) 
Porcentaje_rating
#### Grafico
ggplot(data=Porcentaje_rating) +
  geom_point(mapping=aes(x=porcentaje, y=promedio)) +
  labs(x="Porcentaje de cacao", y="Rating promedio", title="Rating promedio por porcentaje de cacao")

### Promedio por país
Pais_rating<-datos %>% 
  group_by(pais=datos$pais) %>% 
  summarise(promedio=mean(rating), cantidad=n()) 
Pais_rating
#### Grafico
ggplot(data=Pais_rating) +
  geom_point(mapping=aes(x=pais, y=promedio)) +
  labs(x="País/Ciudad de elaboración", y="Rating promedio", title="Rating promedio por País/Ciudad de elaboración") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))


## Inciso 3

### a) Mejores granos de cafe
Mejores_granos<-subset(x=datos, subset=datos$rating==5, select=origen)
Mejores_granos

### b) Mejor calificación del país
Mejor_pais<-subset(x=datos, subset=datos$rating==5, select=pais)
Mejor_pais

### c) Mejor calificación por porcentaje
Mejor_porcentaje<-subset(x=datos, subset=datos$rating==5, select=porcentaje)
Mejor_porcentaje


##########


# Ejercicio 2


## Inciso 1

datos_vd<-datos
datos_vd$rating<-ifelse(datos_vd$rating==5,1,0)
lista_vd<-list(datos,datos_vd)
lista_vd


## Inciso 2

### Datos continuos
datos_vc<-datos #para alojar datos de forma continua

datos_vc$rating<-ifelse(datos_vc$rating==1,1,
                        ifelse(datos_vc$rating==1.5,2,
                               ifelse(datos_vc$rating==1.75,3,
                                      ifelse(datos_vc$rating==2,4,
                                             ifelse(datos_vc$rating==2.25,5,
                                                    ifelse(datos_vc$rating==2.5,6,
                                                           ifelse(datos_vc$rating==2.75,7,
                                                                  ifelse(datos_vc$rating==3,8,
                                                                         ifelse(datos_vc$rating==3.25,9,
                                                                                ifelse(datos_vc$rating==3.5,10,
                                                                                       ifelse(datos_vc$rating==3.75,11,
                                                                                              ifelse(datos_vc$rating==4,12,
                                                                                                     ifelse(datos_vc$rating==5,13,0))))))))))))) #Rating de 1 a n


datos_vc$porcentaje.c<-ifelse(datos_vc$porcentaje<0.5,1,
                            ifelse(datos_vc$porcentaje>=0.5 & datos_vc$porcentaje<0.6,2,
                                   ifelse(datos_vc$porcentaje>=0.6 & datos_vc$porcentaje<0.7,3,
                                          ifelse(datos_vc$porcentaje>=0.7 & datos_vc$porcentaje<0.8,4,
                                                 ifelse(datos_vc$porcentaje>=0.8 & datos_vc$porcentaje<0.9,5,
                                                        ifelse(datos_vc$porcentaje>=0.9,6,0)))))) #Porcentaje de 1 a n

datos_vc$año<-ifelse(datos_vc$año==2006,1,
                     ifelse(datos_vc$año==2007,2,
                            ifelse(datos_vc$año==2008,3,
                                   ifelse(datos_vc$año==2009,4,
                                          ifelse(datos_vc$año==2010,5,
                                                 ifelse(datos_vc$año==2011,6,
                                                        ifelse(datos_vc$año==2012,7,
                                                               ifelse(datos_vc$año==2013,8,
                                                                      ifelse(datos_vc$año==2014,9,
                                                                             ifelse(datos_vc$año==2015,10,
                                                                                    ifelse(datos_vc$año==2016,11,
                                                                                           ifelse(datos_vc$año==2017,12,0)))))))))))) #Año de 1 a n

lista_vc<-list(datos,datos_vc)
lista_vc

### Datos variable binaria

datos_vd$año<-ifelse(datos_vd$año==2006,1,
                     ifelse(datos_vd$año==2007,1,
                            ifelse(datos_vd$año==2008,2,
                                   ifelse(datos_vd$año==2009,2,
                                          ifelse(datos_vd$año==2010,3,
                                                 ifelse(datos_vd$año==2011,3,
                                                        ifelse(datos_vd$año==2012,4,
                                                               ifelse(datos_vd$año==2013,4,
                                                                      ifelse(datos_vd$año==2014,5,
                                                                             ifelse(datos_vd$año==2015,5,
                                                                                    ifelse(datos_vd$año==2016,6,
                                                                                           ifelse(datos_vd$año==2017,6,0))))))))))))
datos_vd <- dummy_cols(datos_vd, select_columns = 'año')



lista_vd<-list(datos,datos_vd)
lista_vd


## Inciso 3


###Hallamos cuantas filas son el 70% de datos, necesarios para datos_train
cant_train=floor(0.70 * nrow(datos))
dim(datos)[1]
set.seed(23) #semilla para repetir el resultado
train_ind_vc <- sample(1:nrow(datos_vc),cant_train,replace = FALSE)
train_ind_vd <- sample(1:nrow(datos_vd),cant_train,replace = FALSE)

###Generamos los 4 datasets
datos_train_vc <- datos_vc[train_ind_vc, ]
datos_test_vc <- datos_vc[-train_ind_vc, ]
datos_train_vd <- datos_vd[train_ind_vd, ]
datos_test_vd <- datos_vd[-train_ind_vd, ]

lista_vc2<-list(datos_train_vc,datos_test_vc)
lista_vd2<-list(datos_train_vd,datos_test_vd)


## Inciso 4


### Modelo lineal
modelo_lineal_1 <- lm(rating ~ año + porcentaje, data = datos_train_vc)
modelo_lineal_2 <- lm(rating ~ año + porcentaje.c, data = datos_train_vc)

distPred_1 <- predict(modelo_lineal_1, datos_test_vc)
predictions_1 <- data.frame(cbind(actuals=datos_train_vc$rating, predicteds=distPred_1))
cor_1 <- cor(predictions_1)
cor_1

distPred_2 <- predict(modelo_lineal_2, datos_test_vc)
predictions_2 <- data.frame(cbind(actuals=datos_train_vc$rating, predicteds=distPred_2))
cor_2 <- cor(predictions_2)
cor_2

summary(modelo_lineal_1)
summary(modelo_lineal_2)


### Modelo logístico
modelo_logistico <- glm(formula= rating ~ año_1 + año_2 + año_3 + año_4 +año_5 + año_6 + porcentaje, data = datos_train_vd, family = "binomial"(link='logit'))

distPred_ml <- predict(modelo_logistico, datos_test_vd)
predictions_ml <- data.frame(cbind(actuals=datos_train_vd$rating, predicteds=distPred_ml))
cor_ml <- cor(predictions_ml)
cor_ml

summary(modelo_logistico)
residuals.glm(modelo_logistico, type="deviance")
par(mfrow=c(1,4))
plot(residuals.glm(modelo_logistico, type="deviance"), main="Residuals")
hist(residuals.glm(modelo_logistico, type="deviance"), main="Residuals")
boxplot(residuals.glm(modelo_logistico, type="deviance"), main="Residuals")
qqnorm(residuals.glm(modelo_logistico, type="deviance"))
qqline(residuals.glm(modelo_logistico, type="deviance"))
summary(residuals.glm(modelo_logistico, type="deviance"))


