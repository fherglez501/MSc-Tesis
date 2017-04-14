plot(f1, 
     type = "b", 
     xlab = "n = 33",
     ylab = "Fulton K", 
     frame.plot = F, 
     main = "Factor 5")
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

# Plot K con Q1,Q2,Q3, MEDIA
plot(f1, 
     type = "b", 
     xlab = "n = 33",
     ylab = "Fulton K", 
     frame.plot = F, 
     main = "Facto")
abline(h = c(rf1$Media, rf1$q[2:4]), 
       lty = 2:6, 
       lwd = 1.5, 
       col = c("red", "blue", "black", "grey"))
legend("topright",c("Q3","Q2","Q1", "Media" ),
       col = c("grey","black", "blue" ,"red"), 
       lty = 2:6, 
       lwd = 1.5, 
       cex = 0.8,
       bty = "n")