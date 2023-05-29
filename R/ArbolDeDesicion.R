library(tidyverse)#manipulacion y visualizacion de los datos
library(caret)#manipulacion y visualizacion de los datos, entrana modelos automatico
library(randomForest)
library(rpart)


folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
dataset <-read_csv(paste0(folder,"/Dataset_ambiente.csv"))
datasetcopia <-read_csv(paste0(folder,"/Dataset_ambiente_prueba.csv"))


folder <-  dirname(rstudioapi::getSourceEditorContext()$path )#Obtiene la ruta de la carpeta en la que se encuentra el archivo de código fuente
dataset <-read_csv(paste0(folder,"/Dataset_ambiente.csv"))#Lee el archivo de datos "Dataset_ambiente.csv" utilizando la función read_csv
datasetcopia <-read_csv(paste0(folder,"/Dataset_ambiente2.csv"))#Lee el archivo de datos "Dataset_ambiente2.csv" utilizando la función, almacena opjeto ata objeto datacopia


fit <- rpart(AMBIENTE ~ ROJO+VERDE+AZUL+TEMP+HUME+PT100#Ajusta un modelo de árbol de decisión utilizando la función rpart. El modelo predice la variable "AMBIENTE" utilizando las variables predictoras "ROJO", "VERDE", "AZUL", "TEMP", "HUME" y "PT100". Se especifica el método de clasificación y los datos de entrenamiento.
             ,method = "class"
             ,data=dataset)
plot(fit, uniform = T, margin = 0.10)#Genera un gráfico del árbol de decisión ajustado utilizando la función plot. Se especifica uniform = TRUE para que los nodos del árbol tengan la misma altura y margin = 0.10 para agregar un margen adicional alrededor del árbol.
text(fit,use.n = T,all = T, Cex=0.8)#Agrega etiquetas a los nodos del árbol utilizando la función text. Se especifica use.n = TRUE para mostrar el número de observaciones en cada nodo y all = TRUE para mostrar todas las etiquetas.
rpart.plot::rpart.plot(fit)#Utiliza la función rpart.plot de la librería "rpart.plot" para generar un gráfico más detallado del árbol de decisión ajustado.
fit$cptable#Muestra la tabla de cost-complexity del árbol de decisión ajustado utilizando el atributo cptable del objeto fit.
treepredict <- predict(fit,newdata = datasetcopia)#Realiza predicciones del árbol de decisión ajustado en nuevos datos datasetcopia utilizando la función predict. Las predicciones se almacenan en el objeto treepredict.
