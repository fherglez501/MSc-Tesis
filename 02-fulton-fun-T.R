fultonT <- function(est, se, df, m, H){
  est <- summary.lm(x)$coef[2, 1] # Pendiente
  se  <- summary.lm(x)$coef[2, 2] # Error Estandar
  df  <- summary.lm(x)$df[2] # Grados de libertad
  # Hipótesis:  Ho: B = 3 | "Crecimiento Isométrico"
  H   <- pt((est-m)/se, df, lower.tail = TRUE)
  result  <- list(Pendiente = est, t = H, gl = df)
  return(result)
}

## Prueba T
est <- summary.lm(lm1)$coef[2, 1] # Pendiente
se  <- summary.lm(lm1)$coef[2, 2] # Error Estandar
df  <- summary.lm(lm1)$df[2] # Grados de libertad
m   <- 3 # mu = 3
# Hipótesis:  Ho: B = 3 | "Crecimiento Isométrico"
H   <- pt((est-m)/se, df, lower.tail = TRUE)

names(K)
lmX  <- lm(K$logW~K$logLTC*K$Sexo)
lmX2 <- lm(K$logW~K$logLTC*K$Estacion)
lmX3 <- lm(K$logW~K$logLTC*K$Clase)
require(car)
Anova(lmX3)
