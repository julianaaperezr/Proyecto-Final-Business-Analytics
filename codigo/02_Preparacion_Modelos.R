# PROYECTO FINAL - BUSINESS ANALYTICS
# Preparacion de datos para modelos

library(dplyr)

# Importar base original
Shoppers <- read.csv("datos/online_shoppers_intention.csv")

# Preparar variables
Shoppers_modelo <- Shoppers %>%
  mutate(
    Compra = as.numeric(Revenue),
    Month = as.factor(Month),
    VisitorType = as.factor(VisitorType),
    Weekend = as.factor(Weekend),
    OperatingSystems = as.factor(OperatingSystems),
    Browser = as.factor(Browser),
    Region = as.factor(Region),
    TrafficType = as.factor(TrafficType)
  )

# Verificar estructura
str(Shoppers_modelo)

# Verificar variable dependiente
table(Shoppers_modelo$Compra)
