# Base de datos -----
dbT   <- readxl::read_excel("BD.xlsx", sheet = "BDT")

# Recodificacion ----
require(data.table)
require(tidyr)
df    <- data.table(dbT, stringsAsFactors = T)
df$ID <- as.character(df$ID)
names(df)
# Estratificando df -----
# Var. Metricas y Var. Ambientales
dfK <- data.table(df[,1:22]) 
# Tablas ----
with(dfK, table(Sexo, Clase, Estacion))
with(dfK, table(Sexo, Estacion))
with(dfK, table(Sexo, Clase))
with(dfK, table(Sexo))
# Var. Sexo & Clase & Estacion ----
H   <- dfK[Sexo == "H"]
M   <- dfK[Sexo == "M"] 
Ad  <- dfK[Clase == "Adulto"]
Su  <- dfK[Clase == "Sub adulto"]
Es  <- dfK[Estacion == "Seca"] 
El  <- dfK[Estacion == "Lluviosa"]
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
names(K)
# Var. dep    = masa (Peso en Kg)
# Var. ind    = longitudes en cm (LTC...)
## Modelo log10 ----
## Cumulativo 
RegK1 <- masslm(data = K[23:29], 
                dv.var = "logW", 
                ignore = "Sexo", 
                p.round = T, 
                c.round = T)
## Hembras 
RegKH <- masslm(data = KH[23:29], 
                dv.var = "logW", 
                p.round = T, 
                c.round = T)
## Machos
RegKM <- masslm(data = KM[23:29], 
                dv.var = "logW", 
                p.round = T, 
                c.round = T)
## Adultos
names(Ad)
RegKA <- masslm(data = log(Ad[,10:16]), 
                dv.var = "Peso", 
                p.round = T, 
                c.round = T)
## Sub adultos
RegKS <- masslm(data = log(Su[,10:16]), 
                dv.var = "Peso", 
                p.round = T, 
                c.round = T)
## Estacion Seca
RegKSe <- masslm(data = log(Es[,10:16]), 
                 dv.var = "Peso", 
                 p.round = T, 
                 c.round = T)
## Estacion Lluviosa
RegKL <- masslm(data = log(El[,10:16]), 
                dv.var = "Peso", 
                p.round = T, 
                c.round = T)

## RESULTADOS LOG ----
RegK1 # Cumulativo
RegKH # Hembras
RegKM # Machos
RegKA # Adultos
RegKS # Subadultos
RegKSe # Seca
RegKL  # Lluviosa
# Res indep ----
# Min - MAx peso
range(dfK$Peso, na.rm = T)
# Rango
max(dfK$LTC, na.rm = T)- min(dfK$LTC, na.rm = T)
# Funcion rango
rango <- function(x) max(x) - min(x)
rango(dfK$LTC[!is.na(dfK$LTC)])
# Rango IQ
IQR(dfK$LTC,na.rm = T)
# Intervalos de Clase
hist(dfK$Peso, breaks = 6, plot = T)
# Plot Masa~Longitud ----
plot(dfK$LTC, dfK$Peso, 
     xlab = "LTC", 
     ylab = "Peso", 
     pch = 19, 
     cex = 2, 
     col = rgb(0,0,0,1/2))

# Fulton Modelos lm ----
lm1  <- lm(K$logW~K$logLTC) # Cumulativo
lmH  <- lm(log(H$Peso)~log(H$LTC))# Hembras
lmM  <- lm(log(M$Peso)~log(M$LTC))# Machos
lmA  <- lm(log(Ad$Peso)~log(Ad$LTC)) # Adultos
lmS  <- lm(log(Su$Peso)~log(Su$LTC)) # Sub adultos
lmEs <- lm(log(Es$Peso)~log(Es$LTC)) # Seca
lmEl <- lm(log(El$Peso)~log(El$LTC)) # Lluviosa

## Resumen modelos
summary(lm1) # b = 3.1888
summary(lmH) # b = 3.5058
summary(lmM) # b = 2.7870
summary(lmA) # b = 2.9869
summary(lmS) # b = 2.9256
summary(lmEs)# b = 2.7840
summary(lmEl)# b = 3.3060
## Parámetros lm1 ----
# Valores ajustados
a   <- model.frame(lm(logW~logLTC, data = K)) 
b   <- model.extract(a,"response")
# Parametros estimados del modelo
ps <- coef(lm1)     # Coeficientes
ci <- confint(lm1)  # IC
# Rango: Vector valores explicativos
rng <- range(K$logLTC[!is.na(K$logLTC)])
# Pesos predichos para K
xs <- seq(rng[1],
          rng[2],
          length.out = 42)
# Intercepto y Pendiente
ys <- ps[1] + ps[2]*xs
## Plot Dx ----
# Ajustes Plot
op <- par(mfrow= c(2,2), 
          mar = c(5,4,4,2))
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

## Tests de Hipótesis -----
## Hipótesis:  Ho: B = 3 | "Crecimiento Isométrico"
require(car)
## Estadisitco - F
lm1$coefficients[2] # Pendiente (K$logLTC)
## "K$logLTC: es la pendiente (B) del modelo (lm1)
Ho   <- linearHypothesis(lm1, "K$logLTC = 3")
HoH  <- linearHypothesis(lmH, "log(H$LTC) = 3")
HoM  <- linearHypothesis(lmM, "log(M$LTC) = 3")
HoA  <- linearHypothesis(lmA, "log(Ad$LTC) = 3")
HoS  <- linearHypothesis(lmS, "log(Su$LTC) = 3")
HoEs <- linearHypothesis(lmEs, "log(Es$LTC) = 3")
HoEl <- linearHypothesis(lmEl, "log(El$LTC) = 3")
# Resultados 
Ho
Ho$F[2]       # F
Ho$Res.Df[2]  # Grados de libertad
Ho$`Pr(>F)`[2]# Valor p

# Funciones ----
## fulton K
fulton   <- function(W, L, b, n, na.rm = T){
  K <- W / (L^3) * (b^n)
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
## NOTA: Debido a que K debe acercarse a la unidad, n puede variar en su valor ( 2...5). n es una constante de la ecuación
f1x  <- fulton(df$Peso, df$LTC, 10, 5)# n = 5
f1   <- fulton(df$Peso, df$LTC, 10, 3)# Cumulativo
fad  <- fulton(Ad$Peso, Ad$LTC, 10, 3)# Adultos
fsa  <- fulton(Su$Peso, Su$LTC, 10, 3)# Sub adultos
fes  <- fulton(Es$Peso, Es$LTC, 10, 3)# Seca
fel  <- fulton(El$Peso, El$LTC, 10, 3)# Lluviosa 
## Resumenes ----
rf1  <- resumenK(f1)
rfad <- resumenK(fad)
rfsa <- resumenK(fsa)
rfes <- resumenK(fes)
rfel <- resumenK(fel)


### Obtener IC de Fulton para PLOT
## Plots Fulton ----
## Estratificacion K -----
# DK  <- K %>% mutate(Fulton = Peso/LTC^3*10^3)
DK  <- K  %>% mutate(Fulton = f1)
str(DK)
fad <- DK %>% filter(Clase == "Adulto")
fsa <- DK %>% filter(Clase == "Sub adulto")
fes <- DK %>% filter(Estacion == "Seca")
fel <- DK %>% filter(Estacion == "Lluviosa")
## Categorizacion fulton K -----
### Baja =  Valor de K menor a la Media, - 1 DE
### Buena = Valor de K => a Baja y Menor = a Media +1 DE
### Excelente = K > Media+1DE
m <- rf1$Media
k <- DK %>% select(Fulton)

Exc <- k > m + (1*rf1$DE) 
Baj <- k < m - (1*rf1$DE)
Bue <- k > m - (1*rf1$DE) & k <m + (1*rf1$DE)

xK <- DK %>% 
  mutate(
    Excelente = DK$Fulton > m + (1*rf1$DE),
    Buena = DK$Fulton > m - (1*rf1$DE) & 
      DK$Fulton < m + (1*rf1$DE),
    Baja = DK$Fulton < m - (1*rf1$DE)
    ) 

names(xK)
xK[c(1:4, 6, 16, 30:33)]

## Cumulativo ----
plot(DK$Fulton, 
     type = "b", 
     xlab = "n = 33",
     ylab = "Fulton K", 
     frame.plot = T, 
     main = "Cumulativo", 
     xaxt = "n")
abline(h = c(rf1$Media, rf1$q[2], rf1$q[4]), 
       lty = 4, 
       lwd = 1.5, 
       col = c("red", "blue", "grey"))
legend("bottomright",
       c("Q3","Media", "Q1"), 
       col = c("grey", "red" ,"blue"), 
       lty = 4, 
       lwd = 1.5, 
       cex = 0.8, 
       bty = "n")
with(DK, 
     text(DK$Fulton, 
          labels = DK$ID, 
          pos = 4, 
          cex = 0.6))
## Ajustes ----
#op <- par(mfrow= c(2,2), mar = c(4,4,4,2))
op <- par(mfrow= c(2,2), mar = c(4,4,4,2))
## Adultos ----
plot(fad$Fulton, 
     type = "b", 
     xlim = c(0,25), 
     xlab = "n = 23",
     ylab = "Fulton K", 
     frame.plot = F, 
     main = "Adultos", 
     xaxt = "n")
abline(h = c(rf1$Media, rf1$q[2], rf1$q[4]), 
       lty = 4, 
       lwd = 1.5, 
       col = c("red", "blue", "grey"))
legend("topright",c("Q3","Media", "Q1"), 
       col = c("grey", "red" ,"blue"), 
       lty = 4, 
       lwd = 1.5, 
       cex = 0.8, 
       bty = "n")
with(fad, 
     text(fad$Fulton, 
          labels = fad$ID, 
          pos = 4, 
          cex = 0.6))
## Sub adultos ----
plot(fsa$Fulton, 
     type = "b", 
     xlab = "n = 10",
     ylab = "Fulton K", 
     frame.plot = F, 
     main = "Sub adultos", 
     xaxt = "n")
abline(h = c(rf1$Media, rf1$q[2], rf1$q[4]), 
       lty = 4, 
       lwd = 1.5, 
       col = c("red", "blue", "grey"))
legend("bottomright",c("Q3","Media", "Q1"), 
       col = c("grey", "red" ,"blue"), 
       lty = 4, 
       lwd = 1.5, 
       cex = 0.8, 
       bty = "n")
with(fsa, 
     text(fsa$Fulton, 
          labels = fsa$ID, 
          pos = 4, 
          cex = 0.6))
## Seca ----
plot(fes$Fulton, 
     type = "b", 
     xlim = c(0,10), 
     xlab = "n = 9",
     ylab = "Fulton K", 
     frame.plot = F, 
     main = "Seca", 
     xaxt = "n")
abline(h = c(rf1$Media, rf1$q[2], rf1$q[4]), 
       lty = 4, 
       lwd = 1.5, 
       col = c("red", "blue", "grey"))
legend("bottomright",c("Q3","Media", "Q1"), 
       col = c("grey", "red" ,"blue"), 
       lty = 4, 
       lwd = 1.5, 
       cex = 0.8, 
       bty = "n")
with(fes, 
     text(fes$Fulton, 
          labels = fes$ID, 
          pos = 1, 
          cex = 0.6))
## LLuviosa ----
plot(fel$Fulton, 
     type = "b", 
     xlim = c(0,25), 
     xlab = "n = 24",
     ylab = "Fulton K", 
     frame.plot = F, 
     main = "Lluviosa", 
     xaxt = "n")
abline(h = c(rf1$Media, rf1$q[2], rf1$q[4]), 
       lty = 4, 
       lwd = 1.5, 
       col = c("red", "blue", "grey"))
legend("bottomright",c("Q3","Media", "Q1"), 
       col = c("grey", "red" ,"blue"),
       lty = 4, 
       lwd = 1.5, 
       cex = 0.8, 
       bty = "n")
with(fel, 
     text(fel$Fulton, 
          labels = fel$ID[!is.na(fel$ID)], 
          pos = 1, 
          cex = 0.6))
## Resetear ajustes ----
par(op)

# Predicciones ----
nd    <- data.frame(K$ID,K$logLTC)  # log Long
plogW <- predict(lm1, nd) # Pred para Masa
## Factor de corrección ----
require(FSA)
cf  <- logbtcf(lm1,10) # factor de correccion (cf)
cf*(10^plogW) # back-transforming with bias correction

## Intervalos de Confianza ----
# Intervalos de la media
mlogW <- predict(lm1,interval="confidence")
ICm   <- cf*10^mlogW # IC para la media
ICm[1,] # Primer FIla
# Intervalos  Predichos
plogW <- predict(lm1,interval="prediction")
ICp   <- cf*(10^plogW) # IC para la predccion
ICp[1,] # Primer Fila

# Plot con ICp ----
# IC Cumulativo para la media de K (n = 3)
Wc  <- mean(DK$Peso[!is.na(DK$Peso)]) 
me  <- mean(ICp[,1]) 
lwr <- mean(ICp[,2])
upr <- mean(ICp[,3])
## Cumulativo -----
plot(DK$Peso, 
     type = "b", 
     xlab = "n = 33",
     ylab = "Peso (Kg)", 
     frame.plot = T, 
     main = "IC para la predicción", 
     xaxt = "n")
abline(h = c(me, lwr, upr), 
       lty = 4, 
       lwd = 1.5, 
       col = c("red", "blue", "grey"))
legend("topright",c("Q3","Media", "Q1"), 
       col = c("grey", "red" ,"blue"), 
       lty = 4, 
       lwd = 1.5, 
       cex = 0.8, 
       bty = "n")
with(DK, 
     text(DK$Peso, 
          labels = DK$ID, 
          pos = 4, 
          cex = 0.6))
## Adultos -----
DKp  <- data.frame(DK[1:33,], ICp)
# Con pesos Adultos
ad   <-  DKp %>% filter(Clase == "Adulto")
meA  <- mean(ad$fit) 
lwrA <- mean(ad$lwr)
uprA <- mean(ad$upr)
# Plot Adultos -----
plot(ad$Peso, 
     type = "b", 
     xlim = c(0,25), 
     xlab = "n = 24",
     ylab = "Peso (Kg)", 
     frame.plot = T, 
     main = "IC Predicho para Adultos", 
     xaxt = "n")
abline(h = c(meA, lwrA, uprA), 
       lty = 4, 
       lwd = 1.5, 
       col = c("red", "blue", "grey"))
legend("topright",c("Q3","Media", "Q1"), 
       col = c("grey", "red" ,"blue"), 
       lty = 4, 
       lwd = 1.5, 
       cex = 0.8, 
       bty = "n")
with(ad, 
     text(ad$Peso, 
          labels = ad$ID, 
          pos = 4, 
          cex = 0.6))

# Sub Adultos ----
sa    <-  DKp %>% filter(Clase == "Sub adulto")
meSA  <- mean(sa$fit) 
lwrSA <- mean(sa$lwr)
uprSA <- mean(sa$upr)
# Plot Sub Adultos ----
plot(sa$Peso, 
     type = "b", 
     xlim = c(0,9), 
     ylim = c(0,50), 
     xlab = "n = 8",
     ylab = "Peso (Kg)", 
     frame.plot = T, 
     main = "IC Predicho para Sub adultos", 
     xaxt = "n")
abline(h = c(meSA, lwrSA, uprSA), 
       lty = 4, 
       lwd = 1.5, 
       col = c("red", "blue", "black"))
legend("bottomright",c("Q3","Media", "Q1"), 
       col = c("black", "red" ,"blue"), 
       lty = 4, 
       lwd = 1.5, 
       cex = 0.8, 
       bty = "n")
with(sa, 
     text(sa$Peso, 
          labels = sa$ID, 
          pos = 1, 
          cex = 0.6))

# Intervalos de Clase ----
nclass.Sturges(mlogW) # 8 Clases
summary(mlogW) # Min 1.134 | Max 2.152
ran  <- range(mlogW[,1], na.rm = T)
ranT <- max(ran[2])- min(ran[1])
amp  <- ranT+0.1
ampr <- amp/8

hist(mlogW[,1], freq = T, nclass = 6, plot = T)
# Limpiar workspace ----
rm(list=ls()) 


