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
