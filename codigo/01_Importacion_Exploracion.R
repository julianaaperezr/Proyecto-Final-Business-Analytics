# PROYECTO FINAL - BUSINESS ANALYTICS
# Comportamiento de navegacion y probabilidad de compra

# Etapa 1: Importacion y exploracion de datos
# Importar la base de datos
Shoppers <- read.csv("datos/online_shoppers_intention.csv")

# Revisar las dimensiones
dim(Shoppers)

# Revisar los nombres de las variables
names(Shoppers)

# Revisar los primeros registros
head(Shoppers)

# Revisar la estructura de la base de datos
str(Shoppers)

# Etapa 2: Revision de calidad de los datos
# Identificar valores faltantes por variable
colSums(is.na(Shoppers))

# Calcular el total de valores faltantes
sum(is.na(Shoppers))

# Identificar registros duplicados
sum(duplicated(Shoppers))

# Etapa 3: Analisis exploratorio de los datos
# Distribucion de la variable dependiente
table(Shoppers$Revenue)

# Proporcion de sesiones con y sin compra
prop.table(table(Shoppers$Revenue))

# Porcentaje de sesiones con y sin compra
prop.table(table(Shoppers$Revenue)) * 100

# Etapa 4: Visualizacion de la variable dependiente
# Cargar libreria
library(ggplot2)

# Grafico 1: Distribucion de sesiones con y sin compra
grafico1 <- ggplot(Shoppers, aes(x = Revenue)) +
  geom_bar(fill = "steelblue") +
  geom_text(
    stat = "count",
    aes(label = paste0(round(after_stat(count / sum(count) * 100), 2), "%")),
    vjust = -0.5
  ) +
  scale_x_discrete(labels = c("FALSE" = "Sin compra",
                              "TRUE" = "Con compra")) +
  labs(
    title = "Distribucion de sesiones segun resultado de compra",
    x = "Resultado de la sesion",
    y = "Numero de sesiones"
  ) +
  theme_minimal()

# Mostrar el grafico
grafico1

# Guardar el grafico en la carpeta resultados
ggsave(
  filename = "resultados/Grafico_1_Distribucion_Compras.png",
  plot = grafico1,
  width = 9,
  height = 6,
  dpi = 300
)

# Etapa 5: Estadisticas descriptivas
# Resumen general de las variables
summary(Shoppers)

# Estadisticas de las variables de navegacion
summary(Shoppers$Administrative)
summary(Shoppers$Informational)
summary(Shoppers$ProductRelated)

# Estadisticas de los tiempos de navegacion
summary(Shoppers$Administrative_Duration)
summary(Shoppers$Informational_Duration)
summary(Shoppers$ProductRelated_Duration)

# Estadisticas de las tasas de navegacion
summary(Shoppers$BounceRates)
summary(Shoppers$ExitRates)
summary(Shoppers$PageValues)

# --------------------------------------------------
# Etapa 6: Graficas exploratorias
# --------------------------------------------------

library(ggplot2)

# Grafico 2: Compras segun tipo de visitante

grafico2 <- ggplot(Shoppers, aes(x = VisitorType, fill = Revenue)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Compras segun tipo de visitante",
    x = "Tipo de visitante",
    y = "Numero de sesiones",
    fill = "Compra"
  ) +
  theme_minimal()

grafico2

ggsave(
  "resultados/Grafico_2_Tipo_Visitante.png",
  plot = grafico2,
  width = 9,
  height = 6,
  dpi = 300
)


# Grafico 3: Compras segun mes

grafico3 <- ggplot(Shoppers, aes(x = Month, fill = Revenue)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Compras segun mes",
    x = "Mes",
    y = "Numero de sesiones",
    fill = "Compra"
  ) +
  theme_minimal()

grafico3

ggsave(
  "resultados/Grafico_3_Compras_Mes.png",
  plot = grafico3,
  width = 9,
  height = 6,
  dpi = 300
)


# Grafico 4: Compras segun fin de semana

grafico4 <- ggplot(Shoppers, aes(x = Weekend, fill = Revenue)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Compras segun dia de la semana",
    x = "Fin de semana",
    y = "Numero de sesiones",
    fill = "Compra"
  ) +
  theme_minimal()

grafico4

ggsave(
  "resultados/Grafico_4_Fin_Semana.png",
  plot = grafico4,
  width = 9,
  height = 6,
  dpi = 300
)


# Grafico 5: Paginas de productos visitadas segun compra

grafico5 <- ggplot(Shoppers, aes(x = Revenue, y = ProductRelated)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Paginas de productos visitadas segun resultado de compra",
    x = "Resultado de compra",
    y = "Numero de paginas de productos visitadas"
  ) +
  theme_minimal()

grafico5

ggsave(
  "resultados/Grafico_5_Paginas_Productos.png",
  plot = grafico5,
  width = 9,
  height = 6,
  dpi = 300
)


# Grafico 6: Valor de las paginas segun compra

grafico6 <- ggplot(Shoppers, aes(x = Revenue, y = PageValues)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Valor de las paginas segun resultado de compra",
    x = "Resultado de compra",
    y = "Valor de las paginas"
  ) +
  theme_minimal()

grafico6

ggsave(
  "resultados/Grafico_6_Valor_Paginas.png",
  plot = grafico6,
  width = 9,
  height = 6,
  dpi = 300
)

# Etapa 5: Estadisticas descriptivas
# Resumen general de las variables
summary(Shoppers)

# Estadisticas de las variables de navegacion
summary(Shoppers$Administrative)
summary(Shoppers$Informational)
summary(Shoppers$ProductRelated)

# Estadisticas de los tiempos de navegacion
summary(Shoppers$Administrative_Duration)
summary(Shoppers$Informational_Duration)
summary(Shoppers$ProductRelated_Duration)

# Estadisticas de las tasas de navegacion
summary(Shoppers$BounceRates)
summary(Shoppers$ExitRates)
summary(Shoppers$PageValues)

# Etapa 6: Graficas exploratorias
library(ggplot2)

# Grafico 2: Compras segun tipo de visitante
grafico2 <- ggplot(Shoppers, aes(x = VisitorType, fill = Revenue)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Compras segun tipo de visitante",
    x = "Tipo de visitante",
    y = "Numero de sesiones",
    fill = "Compra"
  ) +
  theme_minimal()

grafico2

ggsave(
  "resultados/Grafico_2_Tipo_Visitante.png",
  plot = grafico2,
  width = 9,
  height = 6,
  dpi = 300
)

# Grafico 3: Compras segun mes
grafico3 <- ggplot(Shoppers, aes(x = Month, fill = Revenue)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Compras segun mes",
    x = "Mes",
    y = "Numero de sesiones",
    fill = "Compra"
  ) +
  theme_minimal()

grafico3

ggsave(
  "resultados/Grafico_3_Compras_Mes.png",
  plot = grafico3,
  width = 9,
  height = 6,
  dpi = 300
)

# Grafico 4: Compras segun fin de semana
grafico4 <- ggplot(Shoppers, aes(x = Weekend, fill = Revenue)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Compras segun dia de la semana",
    x = "Fin de semana",
    y = "Numero de sesiones",
    fill = "Compra"
  ) +
  theme_minimal()

grafico4

ggsave(
  "resultados/Grafico_4_Fin_Semana.png",
  plot = grafico4,
  width = 9,
  height = 6,
  dpi = 300
)

# Grafico 5: Paginas de productos visitadas segun compra
grafico5 <- ggplot(Shoppers, aes(x = Revenue, y = ProductRelated)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Paginas de productos visitadas segun resultado de compra",
    x = "Resultado de compra",
    y = "Numero de paginas de productos visitadas"
  ) +
  theme_minimal()

grafico5

ggsave(
  "resultados/Grafico_5_Paginas_Productos.png",
  plot = grafico5,
  width = 9,
  height = 6,
  dpi = 300
)

# Grafico 6: Valor de las paginas segun compra
grafico6 <- ggplot(Shoppers, aes(x = Revenue, y = PageValues)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Valor de las paginas segun resultado de compra",
    x = "Resultado de compra",
    y = "Valor de las paginas"
  ) +
  theme_minimal()

grafico6

ggsave(
  "resultados/Grafico_6_Valor_Paginas.png",
  plot = grafico6,
  width = 9,
  height = 6,
  dpi = 300
)

# Etapa 7: Tasas de conversion por categoria
library(dplyr)

# Tabla 1: Conversion por tipo de visitante
tabla_visitantes <- Shoppers %>%
  group_by(VisitorType) %>%
  summarise(
    Sesiones = n(),
    Compras = sum(Revenue),
    Tasa_Conversion = mean(Revenue) * 100
  )

tabla_visitantes

# Tabla 2: Conversion por mes
tabla_meses <- Shoppers %>%
  group_by(Month) %>%
  summarise(
    Sesiones = n(),
    Compras = sum(Revenue),
    Tasa_Conversion = mean(Revenue) * 100
  )

tabla_meses

# Tabla 3: Conversion por fin de semana
tabla_fin_semana <- Shoppers %>%
  group_by(Weekend) %>%
  summarise(
    Sesiones = n(),
    Compras = sum(Revenue),
    Tasa_Conversion = mean(Revenue) * 100
  )

tabla_fin_semana

# Etapa 8: Exportacion de tablas descriptivas
# Guardar tabla de conversion por tipo de visitante
write.csv(
  tabla_visitantes,
  "resultados/Tabla_1_Conversion_Visitantes.csv",
  row.names = FALSE
)

# Guardar tabla de conversion por mes
write.csv(
  tabla_meses,
  "resultados/Tabla_2_Conversion_Meses.csv",
  row.names = FALSE
)

# Guardar tabla de conversion por fin de semana
write.csv(
  tabla_fin_semana,
  "resultados/Tabla_3_Conversion_Fin_Semana.csv",
  row.names = FALSE
)
