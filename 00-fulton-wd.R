# Directorio ----
setwd("~/R/TESIS/Base de Datos/Data/Final/")
dir() # Ubicacion de la BD
# Base de datos -----
dbT   <- readxl::read_excel("BD.xlsx", sheet = "BDT")
# Estructura, dimension y clase 
str(dbT)
dim(dbT)
class(dbT)
# Recodificacion ----
require(data.table)
df    <- data.table(dbT, stringsAsFactors = T)
df$ID <- as.character(df$ID)
