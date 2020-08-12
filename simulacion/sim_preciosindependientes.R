# Simulación de Monte Carlo con precios independientes
library(tidyquant)

# Defino fechas desde donde obtendre los datos
data_fecha_comienzo = "2014-01-01"
data_fecha_fin = today()

# Defino la cantidad de activos
n_assets = 4

# Obtengo las acciones de interés ----
NVDA = tq_get("NVDA", get = "stock.prices", from = data_fecha_comienzo, to = data_fecha_fin, complete_cases = T)
MELI = tq_get("MELI", get = "stock.prices", from = data_fecha_comienzo, to = data_fecha_fin, complete_cases = T)
MSFT = tq_get("MSFT", get = "stock.prices", from = data_fecha_comienzo, to = data_fecha_fin, complete_cases = T)
BABA = tq_get("BABA", get = "stock.prices", from = data_fecha_comienzo, to = data_fecha_fin, complete_cases = T)


# Retornos logaritmicos mensuales de las acciones ----
NVDA_returns <- NVDA %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily",
               type = "log",
               col_rename = "returns")

MELI_returns <- MELI %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily",
               type = "log",
               col_rename = "returns")

MSFT_returns <- MSFT %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily",
               type = "log",
               col_rename = "returns")

BABA_returns <- BABA %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily",
               type = "log",
               col_rename = "returns")

# Caracterización del portafolio----
mu = c(mean(NVDA_returns[[2]]), mean(MELI_returns[[2]]), mean(MSFT_returns[[2]]), mean(BABA_returns[[2]]))
sd = c(sd(NVDA_returns[[2]]), sd(MELI_returns[[2]]), sd(MSFT_returns[[2]]), sd(BABA_returns[[2]]))
P0 = c(last(NVDA[[8]]), last(MELI[[8]]), last(MSFT[[8]]), last(BABA[[8]]))

# Información pertinente a la simulación ----
# Sin considerar caminos de precios
T = 0.5 #6 meses
m = 1000 #Cantidad de camino de precios 
z = matrix(rnorm(n_assets*m), nrow = m, ncol = n_assets) #Matriz de numeros normales estandar
cor(z)

# Simulación de precios independientes a partir de los z generados ----
PT = matrix(NA, m, n_assets)

for (i in 1:m) { #Bucle que pasa por cada simulacion
  for (k in 1:n_assets) { #Bucle que calcula el precio del activo k, en la simulación i
    PT[i,k] = P0[k]*exp((mu[k]-0.5*sd[k]^2)*T + sd[k]*sqrt(T)*z[i,k]) #Simulación i-ésima, activo k-ésimo
  }
}
hist(PT)

prob = 0.95 
# LS
quantile(PT, prob)
# LI
quantile(PT, 1-prob) 
# Para obtener el rendimiento logaritmico de los activos
P0.m = matrix(rep(P0, m), m, n_assets, byrow = T) #Matriz con vector P0 repetido m veces
RL = log(PT/P0.m) #Rendimiento logaritmico de los activos
cor(RL)
