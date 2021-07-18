####Taller 4 
 
## Incorporación de los datos a la biblioteca 
datos <- read.csv("/Users/user/Desktop/SouthGermanCredit.csv",sep=";", header=TRUE)

#Visualización general 
head(datos)

## Resumen estadístico
summary(datos)

## Cantidad de datos 
length(datos$personal)

## histograma de historial de cuenta corriente
hist(x=datos$status)

## histograma de historial de créditos
hist(x=datos$history)

## histograma de la situación laboral
hist(x=datos$employed)

# Instalamos los paquetes necesarios, en caso que no los tengamos instaladas
install.packages("class")

# Cargamos las librerias que utilizaremos
library(class)

## Elegimos las variables para el análisis y las que son categóricas se normalizan

datos_adaptados <- as.data.frame(cbind(age = datos$age,
                                       historial_delay = (datos$history == 0),
                                       historial_critic = (datos$history == 1),
                                       historial_new =(datos$history ==2),
                                       historial_clean = (datos$history == 3),
                                       savings = datos$savings,
                                       employed = datos$employed,
                                       job = datos$job,
                                       foreign = datos$foreign,
                                       credit = datos$credit))


# Normalizamos los datos
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

## datos_norm <- as.data.frame(lapply(datos_small, normalize))
datos_norm1 <- as.data.frame(lapply(datos_adaptados, normalize))


# Dividimos los datos en entrenamiento (75%) y validacion (25%)
set.seed(123)
subset <- sample(1:nrow(datos), size = 0.75*nrow(datos), replace = FALSE)

# Creamos las bases de datos a utilizar
datos_train <- datos_norm1[subset,1:9]
label_train <- datos_norm1[subset,10]
datos_test <- datos_norm1[-subset,1:9]
label_test <- datos_norm1[-subset,10]


# Modelo KNN con tres K distintos

modelo_k3 <- knn(datos_train,
                 datos_test,
                 label_train,
                 k = 3)

modelo_k5 <- knn(datos_train,
                 datos_test,
                 label_train,
                 k = 5)


modelo_k7 <- knn(datos_train,
                 datos_test,
                 label_train,
                 k = 7)

# Matriz de confusion
table(label_test, modelo_k3)
table(label_test, modelo_k5)
table(label_test, modelo_k7)

# Accuracy
sum(label_test == modelo_k3)/length(label_test)
sum(label_test == modelo_k5)/length(label_test)
sum(label_test == modelo_k7)/length(label_test)

# Paquetes para matrices de confusion
library(caret)
library(e1071)

# Matriz de confusion
confusionMatrix(modelo_k3,as.factor(label_test))
confusionMatrix(modelo_k5,as.factor(label_test))
confusionMatrix(modelo_k7,as.factor(label_test))


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


# Corramos otra vez el modelo con K = 15
modelo_k_15 <- knn(datos_train,
                  datos_test,
                  label_train,
                  k = 15)

sum(label_test == modelo_k_15)/length(label_test)


# Comparacion con un arbol de clasificacion
library(rpart)
library(rpart.plot)

datos_tree <- cbind(datos_train,
                    credit = label_train)

modelo_tree <- rpart(credit ~ age + historial_delay + historial_critic + historial_new + historial_clean + savings + employed +job +foreign,
                     data = datos_tree,
                     method = "class",
                     parms = list(split = "information"))

tree_predict <- predict(modelo_tree,
                        datos_test,
                        type = "class")

sum(label_test == tree_predict)/length(label_test)

rpart.plot(modelo_tree)

table(label_test, tree_predict)
