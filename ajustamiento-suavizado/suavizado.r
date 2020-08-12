# Suavizado de datos por Whittaker Henderson.
# Utilizar suavizar.csv

#install.packages("pracma")
library(pracma)

u = read.csv(file = file.choose(), header = T, row.names = 1)

par(mfrow = c(2,3))
plot(u$V1, col = "gray", xlab = "t", ylab = "u(t)",
     ylim = c(-1.5, 1.5), main = "Datos",
     xaxs = "i", yaxs = "i")

# Suavizado default, el parametro lambda es 1600
y_default = whittaker(u$V1, d = 2)
plot(u$V1, col = "grey", xlab = "t", ylab = "u(t)",
     ylim = c(-1.5, 1.5), main = "WhitTaker Default",
     xaxs = "i", yaxs = "i")
lines(y_default, col = "red")

# Suavizo mucho, extremadamente suave
y_muysuave = whittaker(u$V1, lambda = 10^9)
plot(u$V1, col = "grey", xlab = "t", ylab = "u(t)",
     ylim = c(-1.5, 1.5), main = "Muy suave",
     xaxs = "i", yaxs = "i")
lines(y_muysuave, col = "blue")

# Suavizo poco , alta fidelidad
y_pocosuave = whittaker(u$V1, lambda = 0.1)
plot(u$V1, col = "grey", xlab = "t", ylab = "u(t)",
     ylim = c(-1.5, 1.5), main = "Muy fiel",
     xaxs = "i", yaxs = "i")
lines(y_pocosuave, col = "black")

# Ejercicios, en realidad es bastante suave
y_notansuave = whittaker(u$V1, lambda = 10^6)
plot(u$V1, col = "grey", xlab = "t", ylab = "u(t)",
     ylim = c(-1.5, 1.5), main = "bastante suave",
     xaxs = "i", yaxs = "i")
lines(y_notansuave, col = "green")

# Suave normal
y_suave = whittaker(u$V1, lambda = 10000)
plot(u$V1, col = "grey", xlab = "t", ylab = "u(t)",
     ylim = c(-1.5, 1.5), main = "suave",
     xaxs = "i", yaxs = "i")
lines(y_suave, col = "yellow")
