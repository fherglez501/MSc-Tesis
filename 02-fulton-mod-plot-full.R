## Estratificacion K -----
DK  <- K  %>% mutate(Fulton = Peso/(LTC^3)*(10^3))
fad <- DK %>% filter(Clase == "Adulto")
fsa <- DK %>% filter(Clase == "Sub adulto")
fes <- DK %>% filter(Estacion == "Seca")
fel <- DK %>% filter(Estacion == "Lluviosa")
# Cumulativo ----
plot(DK$Fulton, 
     type = "b", 
     xlab = "n = 33",
     ylab = "Fulton K", 
     frame.plot = F, 
     main = "Cumulativo", 
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
with(DK, 
     text(DK$Fulton, 
          labels = DK$ID, 
          pos = 4, 
          cex = 0.6))
# Ajustes----
#op <- par(mfrow= c(2,2), mar = c(4,4,4,2))
op <- par(mfrow= c(2,2), mar = c(4,4,4,2))
# Adultos = LTC; e = 3 ----
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
# Sub adultos = LTC; e = 3 ----
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
# Seca ----
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
# LLuviosa ----
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
# Resetear ajustes ----
par(op)
 

# Revisar
# Plots con ggplot----
DK <- K %>%
  mutate(Fulton = Peso/LTC^3*10^3)

require(ggplot2)
qplot()
plot(DK$Fulton, type = "b")  
# fx
plot(DK$Fulton,  
     type = "b", 
     xlab = "n = 33",
     ylab = "Fulton K", 
     frame.plot = F, 
     main = "Factor 3")
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
with(DK, 
     text(DK$Fulton, 
          labels = DK$ID, 
          pos = 4, 
          cex = 0.6))