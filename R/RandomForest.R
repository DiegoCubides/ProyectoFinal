library(tidyverse)
library(caret)
library(randomForest)

folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
dataset <-read_csv(paste0(folder,"/Dataset_ambiente.csv"))

dataset.complete <- dataset[complete.cases(dataset),]
data.samples <- sample(1:nrow(dataset.complete),
                       nrow(dataset.complete)*0.7, replace = FALSE)
training.data <- dataset.complete[data.samples, ]
test.data <- dataset.complete[-data.samples, ]
fit.rf <- randomForest(AMBIENTE ~ ROJO+VERDE+AZUL+TEMP+HUME+PT100
                       ,method = "class"
                       ,data=dataset)
prediction.rf <- predict(fit.rf, test.data)
output <- data.frame(test.data$AMBIENTE, prediction.rf)
RMSE= sqrt(sum((output$test.data.AMBIENTE -output$prediction.rf)^2))/
  nrow(output)
RMSE
