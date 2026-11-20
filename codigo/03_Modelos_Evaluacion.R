# PROYECTO FINAL - BUSINESS ANALYTICS
# Modelos de probabilidad y evaluacion de clasificacion

# Cargar librerias
library(dplyr)

# Crear carpeta para guardar resultados
if (!dir.exists("resultados")) {
  dir.create("resultados")
}

# --------------------------------------------------
# Etapa 12: Importacion de la base preparada
# --------------------------------------------------

# Importar la base de datos preparada
Shoppers_modelo <- read.csv("datos/online_shoppers_modelo.csv")

# Verificar estructura de la base
str(Shoppers_modelo)

# Verificar distribucion de la variable dependiente
table(Shoppers_modelo$Compra)

# Asegurar que las variables categoricas sean factores
Shoppers_modelo <- Shoppers_modelo %>%
  mutate(
    Compra = as.numeric(Compra),
    Month = as.factor(Month),
    VisitorType = as.factor(VisitorType),
    Weekend = as.factor(Weekend),
    OperatingSystems = as.factor(OperatingSystems),
    Browser = as.factor(Browser),
    Region = as.factor(Region),
    TrafficType = as.factor(TrafficType)
  )

# --------------------------------------------------
# Etapa 13: Division de los datos en entrenamiento y prueba
# --------------------------------------------------

# Establecer semilla para garantizar resultados reproducibles
set.seed(123)

# Separar los registros segun el resultado de compra
indices_compra <- which(Shoppers_modelo$Compra == 1)
indices_sin_compra <- which(Shoppers_modelo$Compra == 0)

# Seleccionar aleatoriamente el 70% de cada grupo para entrenamiento
muestra_compra <- sample(
  indices_compra,
  size = floor(0.70 * length(indices_compra))
)

muestra_sin_compra <- sample(
  indices_sin_compra,
  size = floor(0.70 * length(indices_sin_compra))
)

# Unir los registros seleccionados para entrenamiento
indices_entrenamiento <- c(muestra_compra, muestra_sin_compra)

# Crear las bases de entrenamiento y prueba
Datos_entrenamiento <- Shoppers_modelo[indices_entrenamiento, ]
Datos_prueba <- Shoppers_modelo[-indices_entrenamiento, ]

# Verificar la distribucion de compra en ambas bases
table(Datos_entrenamiento$Compra)
table(Datos_prueba$Compra)

# --------------------------------------------------
# Etapa 14: Modelo lineal de probabilidad
# --------------------------------------------------

# Estimar el modelo lineal de probabilidad inicial
modelo_lineal <- lm(
  Compra ~ PageValues + ProductRelated_Duration + ExitRates +
    BounceRates + Administrative_Duration + Informational_Duration +
    VisitorType + Month + Weekend + SpecialDay,
  data = Datos_entrenamiento
)

# Mostrar los resultados del modelo lineal
summary(modelo_lineal)

# --------------------------------------------------
# Etapa 15: Interacciones y terminos cuadraticos
# --------------------------------------------------

# Centrar variables numericas usando la media
# de la base de entrenamiento

media_PageValues <- mean(Datos_entrenamiento$PageValues, na.rm = TRUE)
media_ExitRates <- mean(Datos_entrenamiento$ExitRates, na.rm = TRUE)
media_Duracion <- mean(Datos_entrenamiento$ProductRelated_Duration, na.rm = TRUE)

Datos_entrenamiento <- Datos_entrenamiento %>%
  mutate(
    PageValues_c = PageValues - media_PageValues,
    ExitRates_c = ExitRates - media_ExitRates,
    ProductRelated_Duration_c = ProductRelated_Duration - media_Duracion
  )

Datos_prueba <- Datos_prueba %>%
  mutate(
    PageValues_c = PageValues - media_PageValues,
    ExitRates_c = ExitRates - media_ExitRates,
    ProductRelated_Duration_c = ProductRelated_Duration - media_Duracion
  )

# Estimar modelo ampliado con interacciones y terminos cuadraticos
modelo_ampliado <- lm(
  Compra ~ PageValues + ProductRelated_Duration + ExitRates +
    BounceRates + Administrative_Duration + Informational_Duration +
    VisitorType + Month + Weekend + SpecialDay +
    PageValues_c:ExitRates_c +
    ProductRelated_Duration_c:ExitRates_c +
    I(PageValues_c^2) +
    I(ExitRates_c^2) +
    I(ProductRelated_Duration_c^2),
  data = Datos_entrenamiento
)

# Mostrar resultados del modelo ampliado
summary(modelo_ampliado)

# --------------------------------------------------
# Etapa 16: Comparacion estadistica de modelos
# --------------------------------------------------

# Comparar modelo lineal inicial y modelo ampliado
comparacion_modelos <- anova(modelo_lineal, modelo_ampliado)

# Mostrar prueba de comparacion
print(comparacion_modelos)

# --------------------------------------------------
# Etapa 17: Preparacion para evaluacion de clasificacion
# --------------------------------------------------

# Instalar paquetes si no estan disponibles
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC")
}

# Cargar paquetes
library(caret)
library(pROC)

# --------------------------------------------------
# Etapa 18: Matriz de confusion y evaluacion
# de clasificacion
# --------------------------------------------------

# Obtener probabilidades de compra con el modelo lineal
prob_lineal <- predict(
  modelo_lineal,
  newdata = Datos_prueba
)

# Obtener probabilidades de compra con el modelo ampliado
prob_ampliado <- predict(
  modelo_ampliado,
  newdata = Datos_prueba
)

# Definir umbral de clasificacion
umbral <- 0.50

# Clasificar las observaciones del modelo lineal
pred_lineal <- ifelse(
  prob_lineal >= umbral, 1, 0
)

# Clasificar las observaciones del modelo ampliado
pred_ampliado <- ifelse(
  prob_ampliado >= umbral, 1, 0
)

# Convertir resultados a factores
# 0 = No compra
# 1 = Compra

compra_real <- factor(
  Datos_prueba$Compra,
  levels = c(0, 1)
)

pred_lineal <- factor(
  pred_lineal,
  levels = c(0, 1)
)

pred_ampliado <- factor(
  pred_ampliado,
  levels = c(0, 1)
)

# Matriz de confusion del modelo lineal
matriz_lineal <- confusionMatrix(
  data = pred_lineal,
  reference = compra_real,
  positive = "1"
)

# Matriz de confusion del modelo ampliado
matriz_ampliada <- confusionMatrix(
  data = pred_ampliado,
  reference = compra_real,
  positive = "1"
)

# Mostrar resultados
print(matriz_lineal)
print(matriz_ampliada)

# ==========================================
# Etapa 18: Curvas ROC y calculo del AUC
# ==========================================

# Curva ROC del modelo lineal
roc_lineal <- roc(
  response = compra_real,
  predictor = prob_lineal,
  levels = c("0", "1"),
  direction = "<"
)

# Curva ROC del modelo ampliado
roc_ampliado <- roc(
  response = compra_real,
  predictor = prob_ampliado,
  levels = c("0", "1"),
  direction = "<"
)

# Calcular AUC de ambos modelos
auc_lineal <- auc(roc_lineal)
auc_ampliado <- auc(roc_ampliado)

# Mostrar resultados
print(auc_lineal)
print(auc_ampliado)

# Graficar ambas curvas ROC
plot(roc_lineal, col = "blue", main = "Comparacion de curvas ROC")
plot(roc_ampliado, col = "red", add = TRUE)

legend(
  "bottomright",
  legend = c("Modelo lineal", "Modelo ampliado"),
  col = c("blue", "red"),
  lwd = 2
)
