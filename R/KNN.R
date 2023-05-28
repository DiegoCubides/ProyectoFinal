library(tidyverse)
library(caret)
folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
dataset <-read_csv(paste0(folder,"/Dataset_ambiente.csv"))
datasetcopia <-read_csv(paste0(folder,"/Dataset_ambiente.csv"))
head(dataset)
hist(dataset$ROJO,breaks= 10)
hist(dataset$VERDE,breaks= 10)
hist(dataset$AZUL,breaks= 10)
hist(dataset$TEMP,breaks= 10)
hist(dataset$HUME,breaks= 10)
hist(dataset$PT100,breaks= 10)

dataset$AMBIENTE <-as.factor(dataset$AMBIENTE)

plot(dataset[1:5]
     ,pch=21,bg=c("green","blue3","yellow")[unclass(dataset$AMBIENTE)])

sample.index <- sample(1:nrow(dataset)
                       ,nrow(dataset)*0.7
                       ,replace = F)
predictors <- c("ROJO","VERDE","AZUL","TEMP","HUME","PT100")
train.data  <-  dataset[sample.index
                                   ,c(predictors,"AMBIENTE")
                                   ,drop=F]
test.data  <-  dataset[-sample.index
                                  ,c(predictors,"AMBIENTE")
                                  ,drop=F]
ctrl <- trainControl(method = "cv",p=0.7) #variable de control
Knnfit <- train(AMBIENTE ~ ROJO+VERDE+AZUL+TEMP+HUME+PT100
                ,data = train.data
                ,method = "knn", trControl = ctrl
                ,preProcess= c("range") #Range
                ,tuneLength=20)
Knnpredict <- predict(Knnfit,newdata = test.data)
confusionMatrix(Knnpredict
                ,test.data$AMBIENTE)
summary(Knnfi)