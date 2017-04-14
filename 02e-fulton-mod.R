# Base de datos -----
dbT   <- readxl::read_excel("BD.xlsx", sheet = "BDT")

# Recodificacion ----
require(data.table)
df    <- data.table(dbT, stringsAsFactors = T)
df$ID <- as.character(df$ID)
names(df)
# Estratificando df -----
# Var. Metricas y Var. Ambientales
dfK <- data.table(df[,1:22]) 
i   <- na.omit(dfK, cols=c("LTC", "Peso"))
# Tablas ----
with(dfK, table(Sexo, Clase, Estacion))
with(dfK, table(Sexo, Estacion))
with(dfK, table(Sexo, Clase))
with(dfK, table(Sexo))
# Var. Sexo & Estacion ----
H   <- dfK[Sexo == "H"]
Hs  <- dfK[Sexo == "H" & Estacion == "Seca"]
Hll <- dfK[Sexo == "H" & Estacion == "Lluviosa"]

M   <- dfK[Sexo == "M"] 
Ms  <- dfK[Sexo == "M" & Estacion == "Seca"]
Mll <- dfK[Sexo == "M" & Estacion == "Lluviosa"]

Ad  <- dfK[Clase == "Adulto"]
Su  <- dfK[Clase == "Sub adulto"]
# Var. log10 ----
require(dplyr)
# Cumulativo
K <- dfK  %>%   
  mutate(logW    =log10(Peso),
         logLTC  =log10(LTC),
         logCCu  =log10(CCu),
         logCPCH =log10(CPCH),
         logCCL  =log10(CCL),
         logLHC  =log10(LHC), 
         logLT   =log10(LT))
# Hembras
KH <- H  %>%  
  mutate(logW    =log10(Peso),
         logLTC  =log10(LTC),
         logCCu  =log10(CCu),
         logCPCH =log10(CPCH),
         logCCL  =log10(CCL),
         logLHC  =log10(LHC), 
         logLT   =log10(LT))
# Machos
KM <- M  %>%  
  mutate(logW    =log10(Peso),
         logLTC  =log10(LTC),
         logCCu  =log10(CCu),
         logCPCH =log10(CPCH),
         logCCL  =log10(CCL),
         logLHC  =log10(LHC), 
         logLT   =log10(LT))
# Exploracion Modelo y Supuesto isometrico (B=3)----
require(exploreR)
names(KH)
# Var. dep    = masa (Peso en Kg)
# Var. ind    = longitudes en cm (LTC...)
# Modelo log10
## Cumulativo 
RegK1 <- masslm(data = K[23:29], dv.var = "logW", ignore = "Sexo", p.round = T, c.round = T)
## Hembras 
RegKH <- masslm(data = KH[23:29], dv.var = "logW", p.round = T, c.round = T)
## Machos
RegKM <- masslm(data = KM[23:29], dv.var = "logW", p.round = T, c.round = T)

## RESULTADOS LOG ----
RegK1 # Cumulativo
RegKH # Hembras
RegKM # Machos

# Min - MAx peso
range(dfK$Peso, na.rm = T)
max(dfK$LTC, na.rm = T)- min(dfK$LTC, na.rm = T)
IQR(dfK$LTC, na.rm = T)

# Plot Masa~Longitud ----
plot(dfK$LTC, dfK$Peso, 
     xlab = "LTC", 
     ylab = "Peso", 
     pch = 19, 
     cex = 2, 
     col = rgb(0,0,0,1/2))

# Fulton Modelos lm ----
# Data sin Na
Ki   <- na.omit(K, cols=c("logLTC", "logW"))
lm1  <- lm(K$logW~K$logLTC,na.action = na.omit)
summary(lm(K$logW~K$logLTC))
lm1i <- lm(Ki$logW~Ki$logLTC)
## Parámetros ----
# Valores ajustados
a   <- model.frame(lm(logW~logLTC, na.action = na.omit, data = K)) 
b   <- model.extract(a,"response")
# Parametros estimados del modelo
ps <- coef(lm1)     # Coeficientes
ci <- confint(lm1)  # IC
# Rango: Vector valores explicativos
rng <- range(K$logLTC[!is.na(K$logLTC)])
# Pesos predichos para K
xs <- seq(rng[1],rng[2],length.out=42)
# Intercepto y Pendiente
ys <- ps[1] + ps[2]*xs
## Plot Dx ----
# Ajustes Plot
op <- par(mfrow= c(2,2), mar = c(5,4,4,2))
plot(lm1)
# Resetear ajustes
par(op)
## Plot con linea ajustada ----
plot(logW~logLTC,
     data=K,
     pch=19,
     cex = 2,
     col=rgb(0,0,0,1/2),
     ylab="LogPeso (kg)",xlab = "Log LTC (cm)")+
# superponer línea
lines(ys~xs,lwd=2)
## Plot Norm-Homoc ----
require(FSA)
residPlot(lm1)
residPlot(lm1i)

## Tests de Hipótesis -----
## Hipótesis:  Ho: B = 3 | "Crecimiento Isométrico"
require(car)
## Estadisitco - F
lm1i$coefficients[2]
## "K$logLTC: es la pendiente (B) del modelo (lm1)
Ho  <- linearHypothesis(lm1, "K$logLTC = 3")
Hoi <- linearHypothesis(lm1i, "Ki$logLTC = 3")

Ho$F[2]       # F
Ho$Res.Df[2]  # Grados de libertad
Ho$`Pr(>F)`[2]# Valor p

# Funciones ----
## fulton K
fulton   <- function(W, L, b, n, na.rm = T){
  K <- W / (L^3) * b^n
  output = round(K, 2)
  output
}
## Resumenes de K
resumenK <- function(x, print = T){
    n       <- length(x[!is.na(x)]) 
    Media   <- mean(x, na.rm = T)
    MAD     <- mad(x, na.rm = T) 
    DE      <- sd(x, na.rm = T) 
    Mediana <- median(x, na.rm = T)
    MinMax  <- range(x[!is.na(x)])
    rango   <- max(x, na.rm = T)- min(x, na.rm = T)
    q       <- quantile(x, na.rm = T, type = 7)
    round(q, 2)
    iq      <- IQR(x, na.rm = T, type = 7)
    result  <- list(Observaciones = n, Media = Media, MAD = MAD, Mediana = Mediana, DE = DE ,MinMax = MinMax, rango = rango, q = q, iq = iq)
    return(result)
}
# Fulton Modelo ----
## Peso~LTC
## NOTA: Debido a que K debe acercarse, en la medida de lo posible, a la unidad, n puede variar en su valor ( 2...5)
f1   <- fulton(df$Peso, df$LTC, 10, 5)# n = 5
f1_1 <- fulton(df$Peso, df$LTC, 10, 3)# n = 3
# Resumenes ----
rf1   <- resumenK(f1)
rf1_1 <- resumenK(f1_1)
## Plots f1 y f1_1 ----
# Ajustes Plot 
op <- par(mfrow= c(1,2), mar = c(5,4,4,2))
# LTC; e = 5
plot(f1, type = "b", xlab = "n = 33",ylab = "Fulton K", frame.plot = F, main = "Factor 5")
# LTC; e = 3
plot(f1_1, type = "b", xlab = "n = 33",ylab = "Fulton K", frame.plot = F, main = "Factor 3")
# Resetear ajustes
par(op)

## Predicciones ----

#LTC   <- c(90,100)  # vector de Longitudes
nd    <- data.frame(K$logLTC)  # log Long
nd2   <- data.frame(logL=log10(LTC))
plogW <- predict(lm1) # Pred para Masa

# Factor de corrección 
require(FSA)
cf <- logbtcf(lm1,10) # factor de correccion (cf)
cf*(10^plogW) # back-transforming with bias correction
# Intervalos de Confianza ----
mlogW <- predict(lm1,interval="confidence")
ICm   <- cf*10^mlogW # IC para la media

plogW <- predict(lm1,interval="prediction")
ICp   <- cf*10^plogW # IC para la predccion

# Limpiar workspace ----
rm(list=ls()) 



