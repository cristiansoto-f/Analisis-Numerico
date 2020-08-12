# Aplicación de la simulación de Monte-Carlo para la simulación del camino de precios de una acción.
# Es utilizado el paquete tidyquant para obtener los datos de la acción.

library(tidyquant)

# Defino fechas desde donde obtendre los datos
data_fecha_comienzo = "2016-01-01"
data_fecha_fin = today()

# Obtengo la accion de interés
NVDA = tq_get("NVDA", get = "stock.prices", from = data_fecha_comienzo, to = data_fecha_fin, complete_cases = T)

# Obtengo los retornos
NVDA_returns <- NVDA %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly",
               type = "log",
               col_rename = "returns")

# Obtengo la caracterización de una acción
mu = mean(NVDA_returns[[2]])
sd = sd(NVDA_returns[[2]])
P0 = last(NVDA[[7]]) #Precio desde donde empezará la simulación

# Agrego otros datos relacionados al movimiento geometrico browniano, para la simulación de Monte Carlo
T = 0.5 #6 meses
n = 182 #numero de dias, "time-steps"
dt = T/n  #tamaño de cada time-step, aproximadamente 1 día

# Con la simulación se generará un camino de precios partiendo del P0, se irán generando precios a
# partir de 1/182 y luego recursivamente se generarán precios del precio del dia anterior.
# hasta llegar a los 6 meses en este caso.

# Simulación ----
m = 1000 #Cantidad de simulaciones(caminos de precio) que realizaré
Pt = matrix(NA, nrow = m, ncol = n + 1) # Cada una de las filas es una simulación. Se agrega una columna porque empieza en P0
Pt[,1] = P0 # Agrego el precio inicial a todas las filas a traves de la primera columna

for (i in 1:m) { #Barre todas las simulaciones
  for (t in 2:(n + 1)) { #Barre a través del tiempo y va generando un camino de precios
    #Desde la segunda columna porque la primera tiene a Po
    #Aplico la formula
    Pt[i, t] = Pt[i, t-1]*exp((mu-0.5*sd^2)*dt + sd*sqrt(dt)*rnorm(1)) #Rnorm es el numero aleatorio epsilon
  }
}

# Grafico ----
t = rep(0:n,m) #Creo un vector de tiempos porque graficare m caminos de precio
t = matrix(t, nrow = m, ncol = n + 1, byrow = T)

plot(t[1,], Pt[1,], type = "l", ylim = c(min(Pt), max(Pt)))
for (i in 2:m) {
  lines(t[i,], Pt[i,], col = trunc(runif(1)*m))
}

# Métricas estadísticas ----
# Vector de promedios
M = matrix(NA, nrow = 1, ncol = n + 1)
for (i in 1:(n + 1)) {
  M[i] = mean(Pt[,i])
}

prob = 0.95
# Limite superior
LS = matrix(NA, nrow = 1, ncol = n + 1)
for(i in 1:(n+1)){
  LS[i] = quantile(Pt[,i], prob)
}

# Limite inferior
LI = matrix(NA, nrow = 1, ncol = n + 1)
for(i in 1:(n+1)){
  LI[i] = quantile(Pt[,i], 1-prob)
}

LS[n + 1]
LI[n + 1]

# Grafico de métricas estadísticas
lines(t[1,], M, col = 'black', lwd = 3)
lines(t[1,], LS, col = 'red', lwd = 3)
lines(t[1,], LI, col = 'red', lwd = 3)

# Simulación solo del precio final ----
m = 1000 #Cantidad de precios finales
e = rnorm(m)
PT = matrix(NA, nrow = m, ncol = 1)
PT = P0 * exp((mu - 0.5*sd^2)*T + sd*sqrt(T)*e)
hist(PT)
#LS
quantile(PT, prob)
#LI
quantile(PT, 1-prob)
