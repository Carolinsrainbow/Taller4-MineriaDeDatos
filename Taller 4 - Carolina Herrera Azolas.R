####Taller 4 
 
## Incorporación de los datos a la biblioteca 
datos <- read.csv("/Users/user/Desktop/SouthGermanCredit.csv",sep=";", header=TRUE)

#Visualización general 
head(datos)

## Resumen estadístico
summary(datos)

## Cantidad de datos 
length(datos$personal)

## histograma de la situación laboral
hist(x=datos$status)


## histograma de la situación laboral
hist(x=datos$history)

## histograma de la situación laboral
hist(x=datos$employed)

# Instalamos los paquetes necesarios, en caso que no los tengamos instaladas
install.packages("class")

# Cargamos las librerias que utilizaremos
library(class)

## Resumen estadístico
summary(datos)


# En este ejemplo solo utilizaremos cuatro variables explicativas, por simplicidad
# La cantidad de variables a utilizar es algo que usted debe analizar en su evaluacion

# Elegiremos como variables exogenas:
# age: variable continua, la podemos usar tal cual esta en la base de datos
# housing: variable categorica con 3 niveles, debe ser transformada
# job: variable ordinal numerica, la podemos usar tal cual esta en la base de datos
# foreign: variable binaria, la podemos usar tal cual esta en la base de datos

# Ademas usaremos obviamente la variable endogena a clasificar:
# credit: variable binaria, la podemos usar tal cual esta en la base de datos

datos_small <- as.data.frame(cbind(age = datos$age,
                                   housing_free = (datos$housing == 1),
                                   housing_rent = (datos$housing == 2),
                                   job = datos$job,
                                   foreign = datos$foreign,
                                   credit = datos$credit))


# Normalizamos los datos
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

datos_norm <- as.data.frame(lapply(datos_small, normalize))


# Dividimos los datos en entrenamiento (75%) y validacion (25%)
set.seed(123)
subset <- sample(1:nrow(datos), size = 0.75*nrow(datos), replace = FALSE)

# Creamos las bases de datos a utilizar
datos_train <- datos_norm[subset,1:5]
label_train <- datos_norm[subset,6]
datos_test <- datos_norm[-subset,1:5]
label_test <- datos_norm[-subset,6]


# Modelo KNN con dos K distintos
modelo_k3 <- knn(datos_train,
                 datos_test,
                 label_train,
                 k = 3)

modelo_k5 <- knn(datos_train,
                 datos_test,
                 label_train,
                 k = 5)


# Matriz de confusion
table(label_test, modelo_k3)
table(label_test, modelo_k5)

# Accuracy
sum(label_test == modelo_k3)/length(label_test)
sum(label_test == modelo_k5)/length(label_test)


# Paquetes para matrices de confusion
library(caret)
library(e1071)

# Matriz de confusion
confusionMatrix(modelo_k3,as.factor(label_test))
confusionMatrix(modelo_k5,as.factor(label_test))


# Accuracy para distintos K

i = 1
k.optm = 1
for (i in 1:25){
  knn.mod <- knn(datos_train,
                 datos_test,
                 label_train,
                 k = i)
  k.optm[i] <- sum(label_test == knn.mod)/length(label_test)
  cat(i,'=',k.optm[i],'\n') 
}

plot(k.optm)


# Corramos otra vez el modelo con K = 4
modelo_k_4 <- knn(datos_train,
                  datos_test,
                  label_train,
                  k = 4)

sum(label_test == modelo_k_4)/length(label_test)


# Comparacion con un arbol de clasificacion
library(rpart)
library(rpart.plot)

datos_tree <- cbind(datos_train,
                    credit = label_train)

modelo_tree <- rpart(credit ~ age + housing_free + housing_rent + job + foreign,
                     data = datos_tree,
                     method = "class",
                     parms = list(split = "information"))

tree_predict <- predict(modelo_tree,
                        datos_test,
                        type = "class")

sum(label_test == tree_predict)/length(label_test)

rpart.plot(modelo_tree)

table(label_test, tree_predict)


# Instalamos los paquetes necesarios, en caso que no los tengamos instalados
install.packages("cluster")
install.packages("factoextra")
install.packages("tidyverse")

# Cargamos las librerias que utilizaremos
library(cluster)
library(factoextra)
library(tidyverse)

# Indicamos el directorio de trabajo
setwd("C:/Users/Sebastian/Documents/R")

# Cargamos la base de datos
datos <- read.csv("faithful.csv", sep=";",header=TRUE)

# Normalizamos los datos
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

datos_norm <- as.data.frame(lapply(datos, normalize))

# K Means con K = 3
k3 <- kmeans(datos_norm,
             centers = 3,
             nstart = 25)

# Analicemos los resultados

# Tamano de cada cluster
k3$size
# Cluster al que pertenece cada observacion
k3$cluster
# Centros de cada cluster
k3$centers
# Suma cuadratica inicial de distancias
k3$totss
# Sumas cuadraticas de distancias dentro de cada cluster
k3$withinss
k3$tot.withinss


# Apliquemos el codo
clusters <- c(1,2,3,4,5,6,7,8,9,10)
codo <- c(0,0,0,0,0,0,0,0,0,0)
for (k in 1:10) { 
  codo[k] <- kmeans(datos_norm,
                    centers = k,
                    nstart = 25)$tot.withinss
}

plot(clusters, codo, type = "l")


# Calculemos los valores de silueta para nuestros clusters
silueta_k3 <- silhouette(k3$cluster, dist(datos_norm))
summary(silueta_k3)

# La primera columna es el cluster de cada observacion
silueta_k3[,1]
# La segunda columna es el cluster vecino de cada observacion
silueta_k3[,2]
# La tercera columna es el valor de silueta de cada observacion
silueta_k3[,3]

# Grafiquemos la silueta
silueta <- c(0,0,0,0,0,0,0,0,0)
for (k in 2:10) { 
  modelo_aux <- kmeans(datos_norm,
                       centers = k,
                       nstart = 25)
  silueta_aux <- silhouette(modelo_aux$cluster, dist(datos_norm))
  silueta[k] <- mean(silueta_aux[, 3])
}

plot(clusters, silueta, type = "l")

# Distribuciones de valores de silueta
fviz_silhouette(silueta_k3)


# Interpretemos que es cada cluster

# Agreguemos el cluster de cada observacion a los datos
datos$cluster3 <- k3$cluster

# Podemos analizar visualmente los clusteres
plot(datos$eruptions, datos$waiting, col = datos$cluster3)

# Podemos analizar las variables promedio de cada cluster
View(datos
     %>% group_by(cluster3)
     %>% summarise_all(mean))