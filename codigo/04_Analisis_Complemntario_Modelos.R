
# ============================================================
# PROYECTO FINAL - BUSINESS ANALYTICS
# ANALISIS COMPLEMENTARIO DE MODELOS
# ============================================================

# 1. Cargar paquetes
library(dplyr)
library(caret)
library(pROC)

# 2. Ejecutar el archivo de evaluacion original
source("codigo/03_Modelos_Evaluacion.R")

# 3. Verificar que los modelos fueron estimados
print(summary(modelo_lineal))
print(summary(modelo_ampliado))

# 4. Verificar dimensiones de los datos
dim(Datos_entrenamiento)
dim(Datos_prueba)

# 5. Verificar variables principales
names(Shoppers_modelo)

# Fin de la preparacion inicial

# ============================================================
# 6. INDICADORES DE AJUSTE Y SIGNIFICANCIA ESTADISTICA
# ============================================================

# Extraer indicadores del modelo lineal
resumen_lineal <- summary(modelo_lineal)
resumen_ampliado <- summary(modelo_ampliado)

# Funcion para extraer el estadistico F y su significancia
extraer_F <- function(modelo) {
  fs <- summary(modelo)$fstatistic
  
  data.frame(
    Estadistico_F = unname(fs[1]),
    Grados_libertad_1 = unname(fs[2]),
    Grados_libertad_2 = unname(fs[3]),
    Valor_p = pf(fs[1], fs[2], fs[3],
                 lower.tail = FALSE)
  )
}

# Crear tabla comparativa
tabla_ajuste <- data.frame(
  Modelo = c("Modelo lineal", "Modelo ampliado"),
  R2 = c(resumen_lineal$r.squared,
         resumen_ampliado$r.squared),
  R2_ajustado = c(resumen_lineal$adj.r.squared,
                  resumen_ampliado$adj.r.squared),
  Estadistico_F = c(
    extraer_F(modelo_lineal)$Estadistico_F,
    extraer_F(modelo_ampliado)$Estadistico_F
  ),
  Valor_p_F = c(
    extraer_F(modelo_lineal)$Valor_p,
    extraer_F(modelo_ampliado)$Valor_p
  )
)

# Mostrar resultados
print(tabla_ajuste)

# Comparacion estadistica entre modelos
comparacion_anova <- anova(modelo_lineal, modelo_ampliado)

print(comparacion_anova)

# Guardar resultados
write.csv(
  tabla_ajuste,
  "resultados/Tabla_5_Indicadores_Ajuste.csv",
  row.names = FALSE
)

write.csv(
  as.data.frame(comparacion_anova),
  "resultados/Tabla_6_Comparacion_Modelos.csv"
)

# =====================================================
# 7. CALCULO DE MAPE Y SMAPE
# =====================================================

# Convertir la variable real a formato numerico
y <- as.numeric(as.character(compra_real))

# Funcion para calcular MAPE
# Se calcula solo para observaciones con compra real = 1,
# porque MAPE no esta definido cuando el valor real es cero.

calcular_mape <- function(real, predicho) {
  indices <- real != 0
  mean(abs((real[indices] - predicho[indices]) /
             real[indices])) * 100
}

# Funcion para calcular SMAPE
calcular_smape <- function(real, predicho) {
  denominador <- abs(real) + abs(predicho)
  errores <- ifelse(
    denominador == 0,
    0,
    2 * abs(real - predicho) / denominador
  )
  mean(errores) * 100
}

# Calcular indicadores de ambos modelos
tabla_errores <- data.frame(
  Modelo = c("Modelo lineal", "Modelo ampliado"),
  MAPE = c(
    calcular_mape(y, prob_lineal),
    calcular_mape(y, prob_ampliado)
  ),
  SMAPE = c(
    calcular_smape(y, prob_lineal),
    calcular_smape(y, prob_ampliado)
  )
)

# Mostrar resultados
print(tabla_errores)

# Guardar tabla en resultados
write.csv(
  tabla_errores,
  "resultados/Tabla_7_Indicadores_Error.csv",
  row.names = FALSE
)

# Abrir tabla para verificar
View(tabla_errores)

# =====================================================
# 8. GUARDAR MATRICES DE CONFUSION
# =====================================================

# Matriz del modelo lineal
tabla_matriz_lineal <- as.data.frame.matrix(
  matriz_lineal$table
)

tabla_matriz_lineal <- data.frame(
  Prediccion = rownames(tabla_matriz_lineal),
  tabla_matriz_lineal,
  row.names = NULL
)

write.csv(
  tabla_matriz_lineal,
  "resultados/Tabla_8_Matriz_Confusion_Lineal.csv",
  row.names = FALSE
)

# Matriz del modelo ampliado
tabla_matriz_ampliado <- as.data.frame.matrix(
  matriz_ampliada$table
)

tabla_matriz_ampliado <- data.frame(
  Prediccion = rownames(tabla_matriz_ampliado),
  tabla_matriz_ampliado,
  row.names = NULL
)

write.csv(
  tabla_matriz_ampliado,
  "resultados/Tabla_9_Matriz_Confusion_Ampliado.csv",
  row.names = FALSE
)

# Verificar resultados
print(tabla_matriz_lineal)
print(tabla_matriz_ampliado)

