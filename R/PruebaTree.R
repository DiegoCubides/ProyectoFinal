library(rpart)
library(tidyverse)
library(caret)
library(rpart.plot)

folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
datos <-read_csv(paste0(folder,"/dataset_ambiente.csv"))



head(datos)

dummy <- dummyVars(" ~ AMBIENTE",data = datos)

 # Ajuste de los datos
datos$ROJO <- as.numeric(as.character(datos$ROJO))  # Convertir a numérico la columna ROJO
datos$VERDE <- as.numeric(as.character(datos$VERDE))  # Convertir a numérico la columna VERDE
datos$AZUL <- as.numeric(as.character(datos$AZUL))  # Convertir a numérico la columna AZUL
datos$TEMP <- as.numeric(as.character(datos$TEMP))  # Convertir a numérico la columna TEMPERATURA
datos$HUME <- as.numeric(as.character(datos$HUME))  # Convertir a numérico la columna HUME
datos$PT100 <- as.numeric(as.character(datos$PT100))  # Convertir a numérico la columna PT100

datos_entrenamiento <- sample_frac(datos, .7)
datos_prueba <- setdiff(datos, datos_entrenamiento)

fit <- rpart(formula = AMBIENTE ~ ROJO + VERDE + AZUL, data = datos_entrenamiento)

fit <- rpart(
  AMBIENTE~ROJO + VERDE + AZUL,
  method = "anova",
  data = box_test
)

plot(fit, uniform = TRUE, margin = 0.1)
text(fit, use.n = TRUE,all = TRUE, cex=.8)
