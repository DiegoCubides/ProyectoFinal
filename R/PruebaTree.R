library(rpart)
library(tidyverse)
library(caret)

folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
box_test <-read_csv(paste0(folder,"/dataset_ambiente.csv"))


head(box_test)

dummy <- dummyVars(" ~ AMBIENTE",data = AMBIENTE.box_test)

fit <- rpart(
  AMBIENTE ~ ROJO + VERDE + AZUL,
  method = "anova",
  data = box_test
)

plot(fit, uniform = TRUE, margin = 0.1)
text(fit, use.n = TRUE,all = TRUE, cex=.8)
