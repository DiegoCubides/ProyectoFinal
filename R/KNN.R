
library(tidyverse)
library(caret)
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
dataset <-read_csv(paste0(folder,"/Dataset_ambiente.csv"))
datasetcopia <-read_csv(paste0(folder,"/Dataset_ambiente_prueba.csv"))

brary(tidyverse)#paquetes útiles para el análisis y manipulación de datos
library(caret)#paquetes útiles para el análisis y manipulación de datos

folder <-  dirname(rstudioapi::getSourceEditorContext()$path )#Se guarda la ruta de la carpeta que contiene el archivo de código actual en la variable
dataset <-read_csv(paste0(folder,"/Dataset_ambiente.csv"))#Se leen dos conjuntos de datos desde archivos CSV utilizando la función read_csv() d
datasetcopia <-read_csv(paste0(folder,"/Dataset_ambiente2.csv"))
#Se muestran las primeras filas del conjunto de datos dataset utilizando la función head(). Luego se generan histogramas para las variables ROJO, VERDE, AZUL, TEMP, HUME y PT100 utilizando la función hist().

head(dataset)
hist(dataset$ROJO,breaks= 10)
hist(dataset$VERDE,breaks= 10)
hist(dataset$AZUL,breaks= 10)
hist(dataset$TEMP,breaks= 10)
hist(dataset$HUME,breaks= 10)
hist(dataset$PT100,breaks= 10)
kable(summary(dataset))#Se genera un resumen estadístico del conjunto de datos dataset utilizando la función summary(). Luego, el resultado se formatea en una tabla utilizando la función kable() del paquete knitr para una presentación más legible.
#Se convierten las columnas AMBIENTE en factores utilizando la función as.factor(). Esto es útil cuando se trabaja con variables categóricas en modelos de aprendizaje automático.
dataset$AMBIENTE <-as.factor(dataset$AMBIENTE)
datasetcopia$AMBIENTE <- as.factor(datasetcopia$AMBIENTE)

plot(dataset[1:5]#Se genera un diagrama de dispersión utilizando las primeras 5 columnas del conjunto de datos dataset. Cada punto en el diagrama de dispersión está marcado con un símbolo (pch) y un color de fondo (bg) determinados por la variable categórica AMBIENTE.

     ,pch=21,bg=c("green","blue3","yellow")[unclass(dataset$AMBIENTE)])

sample.index <- sample(1:nrow(dataset)#Se crea un vector sample.index que contiene una muestra aleatoria de índices de filas del conjunto de datos dataset. El tamaño de la muestra es el 70% del número total de filas del conjunto de datos.
                       ,nrow(dataset)*0.7
                       ,replace = F)
#Se crean conjuntos de datos de entrenamiento y prueba a partir del conjunto de datos completo. Los conjuntos de datos de entrenamiento (train.data) y prueba (test.data) contienen las variables predictoras especificadas en predictors junto con la variable respuesta AMBIENTE. Los índices de fila seleccionados aleatoriamente se utilizan para dividir los datos en conjuntos de entrenamiento y prueba.
predictors <- c("ROJO","VERDE","AZUL","TEMP","HUME","PT100")
train.data  <-  dataset[sample.index
                                   ,c(predictors,"AMBIENTE")
                                   ,drop=F]
test.data  <-  dataset[-sample.index
                                  ,c(predictors,"AMBIENTE")
                                  ,drop=F]
#Se configura la variable de control ctrl para el entrenamiento del modelo utilizando la función trainControl() del paquete caret.
ctrl <- trainControl(method = "cv",p=0.7) #variable de control
Knnfit <- train(AMBIENTE ~ ROJO+VERDE+AZUL+TEMP+HUME+PT100#Luego se ajusta un modelo de clasificación K-Nearest Neighbors (knn) utilizando la función train() del paquete caret. El modelo se entrena utilizando las variables predictoras especificadas en la fórmula AMBIENTE ~ ROJO + VERDE + AZUL + TEMP + HUME + PT100,
                ,data = train.data
                ,method = "knn", trControl = ctrl
                ,preProcess= c("range") #Range preprocesamiento de los datos utilizando el método de rango
                ,tuneLength=20)#  se realiza una búsqueda de hiperparámetros con una longitud de ajuste de 20 (tuneLength = 20).
Knnpredict <- predict(Knnfit,newdata = datasetcopia)#Se realizan predicciones en el conjunto de datos datasetcopia utilizando el modelo ajustado
confusionMatrix(Knnpredict# Luego se genera una matriz de confusión utilizando la función
                ,datasetcopia$AMBIENTE)# comparando las predicciones con las etiquetas reales de la variable respuesta AMBIENTE en datasetcopia.




