library(tidyverse)
library(caret)

folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
dataset <-read_csv(paste0(folder,"/Dataset_ambiente.csv"))
datasetcopia <-read_csv(paste0(folder,"/Dataset_ambiente2.csv"))

fit <- rpart(AMBIENTE ~ ROJO+VERDE+AZUL+TEMP+HUME+PT100
             ,method = "class"
             ,data=dataset)
plot(fit, uniform = T, margin = 0.10)
text(fit,use.n = T,all = T, Cex=0.8)
rpart.plot::rpart.plot(fit)
fit$cptable
treepredict <- predict(fit,newdata = datasetcopia)
