# Resolución de Ecuaciones Diferenciales.
# Algoritmos 5.1 y 5.2
# Ejemplo: Aproximar la solucioón al problema de valor inicial
# y' = y - t^2 + 1, entre  0 < t < 2 (menor igual) con y(0) = 0.5 y N = 10
# Solución exacta de la ecuación diferencial: -0.5*exp(x) + x^2 + 2*x + 1

runge_kutta = function(a, b, N, alfa){
  #Paso 1
  h = (b - a)/N
  
  w = c(rep(NA, N + 1))
  w[1] = alfa
  
  t = c(seq(a, b, h))
  t[1] = a
  
  for (i in 1:N) {
    #Paso 3
    k1 = h*fn(t[i], w[i])
    k2 = h*fn(t[i] + h/2, w[i] + k1/2)
    k3 = h*fn(t[i] + h/2, w[i] + k2/2)
    k4 = h*fn(t[i + 1], w[i] + k3)
    
    #Paso 4
    w[i + 1] = w[i] + (k1 + 2*k2 + 2*k3 + k4)/6
    
  }
  #Paso 5
  return(data.frame(t,w))
}

Metodo_Euler = function(a, b, N, alfa){
  #Paso 1
  h = (b - a)/N
  
  w = c(rep(NA, N + 1))
  w[1] = alfa
  
  t = c(seq(a, b, h))
  t[1] = a
  
  #Paso 2
  for (i in 1:N) {
    #Paso 3
    w[i + 1] = w[i] + h * fn(t[i], w[i])
    
  }
  
  #Paso 4
  return(data.frame(t,w))
}

# Resolución del ejercicio ----
datos_R = runge_kutta(0, 2, 10, 0.5)
plot(datos_R$t, datos_R$w, type = "p",
     ylim = c(0,6), ylab = "y(t)", xlab = "t",
     xaxs = "i", yaxs = "i")

datos_E = Metodo_Euler(0, 2, 10, 0.5)
points(datos_E$t, datos_E$w, col = "blue", type = "p")

f_real = function(x){
  f = -0.5*exp(x) + x^2 + 2*x + 1
  return(f)
}

#Dibujo la funcion
xes = seq(from=0, to=2, by=0.01)
lines(xes, f_real(xes), col = "red")

legend('topleft', legend = c("Exacta", "Runge-Kutta", "Euler"),
       col = c("red", "black", "blue"),
       lty = c(1,1,1), pch = c(NA, 1, 1))
title("Solucion del problema: y'= y * t^2 + 1")
