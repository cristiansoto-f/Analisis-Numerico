# Ajustamiento Paramétrico

# Mínimos Cuadrados

xp = c(1, 1.3, 1.6, 1.9, 2.2)
fp = c(0.7651977, 0.6200860, 0.2554022, 0.2818186, 0.1103623)
Datos = data.frame(xp, fp)

## Ajuste mediante mínimos cuadrados----

# Polinomio de grado 4

# Ajusta un modelo linea utilizando el metodo de minimos cuadrados ordinarios
# No es lineal en el polinomio pero si en los parametros, es una combinacion lineal de funciones
# Los aparametros aparecen linealmente, no necesariamente una recta
# fp = Ingreso los valores de y, y 
# I funcion AsIs, toma cada elemento al cuadrado, al cubo
# Determina un coeficiente al lado de I, para cada xp^2, xp^3
(MCO4 = lm(fp~xp + I(xp^2) + I(xp^3) + I(xp^4), data = Datos)) #Polinomio de grado 4

# Escribo el polinomio
MCO_P4 = paste("f(x)=", round(MCO4$coefficients[1], 4))
for (i in 2:length(MCO4$coefficients)) {
  MCO_P4 = paste(MCO_P4, "+", round(MCO4$coefficients[i], 4), "x^", (i-1))
}; rm(i)
MCO_P4

# Obtengo los datos del modelo
# No hay grados de libertad, de alguna manera estoy interpolando.
summary(MCO4)

# Buscar los valores en el intervalo
ajuste = data.frame(xp = seq(from = min(xp), to = max(xp), length.out = 1000))
ajuste$fit = predict(MCO4, newdata = ajuste)

plot(Datos, type = 'p', pch = 16, ylim = c(0,1))
lines(ajuste$xp, ajuste$fit, lty = 1, col = "red")


# Polinomio de grado 3

(MCO3 = lm(fp~xp + I(xp^2) + I(xp^3), data = Datos))
# Escribo el polinomio
MCO_P3 = paste("f(x)=", round(MCO3$coefficients[1], 4))
for (i in 2:length(MCO3$coefficients)) {
  MCO_P3 = paste(MCO_P3, "+", round(MCO3$coefficients[i], 4), "x^", (i-1))
}; rm(i)
MCO_P3

summary(MCO3)

# Buscar los valores en el intervalo
ajuste2 = data.frame(xp = seq(from = min(xp), to = max(xp), length.out = 1000))
ajuste2$fit = predict(MCO3, newdata = ajuste2)

lines(ajuste2$xp, ajuste2$fit, lty = 1, col = "green")

# Ejemplo exponencial

(MCOexp = lm(fp~exp(xp)))
summary(MCOexp)
ajuste2$fitExp = predict(MCOexp, newdata = ajuste)
lines(ajuste2$xp, ajuste2$fitExp, lty = 2, col = "orange")

## Ejemplo curva de rendimientos USD BONDS ----
USTBonds = read.csv(file.choose(), header = T, row.names = 1)
Plazo = c(1/12, 2/12, 3/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30)
colores = rainbow(nrow(USTBonds) -1)
plot(Plazo, USTBonds[1,], ylim = c(1, 2.5), type = "b", lty = "dotted",
     pch = 16, ylab = "rate")
for (i in 2:nrow(USTBonds)) {
  points(Plazo, USTBonds[i,], col = colores[i-1], type = "b", lty = "dotted", pch = 16)
}; rm(i)
legend("bottomright", legend = row.names(USTBonds),
       col = c("black", colores), cex = 0.75, pch = 16)

# Grafico para un dia en particular
k = 2 #Dia 2
xp = Plazo
fp = as.double(USTBonds[k,])
plot(xp, fp, ylim = c(1, 2.5), type = "b", lty="dotted", pch = 16,
     xlab = "Plazo", ylab = paste("Yield curve on", row.names(USTBonds)[k]))

# Ajuste del modelo de Nelson-Siegel ----
# install.packages("YieldCurve")
library("YieldCurve")
NS = Nelson.Siegel(fp, xp)
NScurve = function(x){ #1.02
  b0 = NS[1]
  b1 = NS[2]
  b2 = NS[3]
  l = NS[4]
  return(b0 + b1*(1-exp(-l*x))/(l*x) + b2*((1 - exp(-l*x))/(l*x) - exp(-l*x)))
}
curve(NScurve, 0, 30, add = T, col = "blue")

# Ajuste de distribucion, modelo no lineal----
rm(list = ls())
graphics.off()

Datos = read.csv(file.choose(), header = T, row.names = 1)
plot(Datos)
abline(h=c(0,1), col = "lightgrey")
Modelo = nls(Fx ~ pnorm(x, mu, sigma), #minimos no cuadrados no lineales
  data = Datos,
  start = list(mu = 120, sigma = 50)) #Parametros ingresados
summary(Modelo)

ajusteNL = data.frame(x = seq(from = min(Datos$x), to = max(Datos$x), length.out = 1000))
ajusteNL$fit = predict(Modelo, newdata = list(x = ajusteNL$x))
lines(ajusteNL$x, ajusteNL$fit, col = "red", lty = "dashed")

# Otra forma basada en el metodo de momentos
mu_muestra = mean(Datos$x)
sigma_muestra = sd(Datos$x)
curve(pnorm(x, mu_muestra, sigma_muestra), col = "blue", add = T,
      from = min(Datos$x), to = max(Datos$x))
