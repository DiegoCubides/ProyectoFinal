library(tidyverse)
library(caret)
library(randomForest)

folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
dataset <-read_csv(paste0(folder,"/Dataset_ambiente.csv"))
datasetcopia <-read_csv(paste0(folder,"/Dataset_ambiente_prueba.csv"))
dataset$AMBIENTE <-as.factor(dataset$AMBIENTE)
datasetcopia$AMBIENTE <- as.factor(datasetcopia$AMBIENTE)

dataset.complete <- dataset[complete.cases(dataset),]
data.samples <- sample(1:nrow(dataset),
                       nrow(dataset)*0.7, replace = FALSE)
training.data <- dataset[data.samples, ]
test.data <- dataset[-data.samples, ]
fit.rf <- randomForest(AMBIENTE ~ ROJO  +VERDE + AZUL + TEMP + HUME +
                         PT100, data = training.data)
prediction.rf <- predict(fit.rf, datasetcopia)
table(datasetcopia$AMBIENTE,prediction.rf)
