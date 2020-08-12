#Simulación de Monte Carlo con precios correlacionados.

library(tidyquant)

# Defino fechas desde donde obtendre los datos
data_fecha_comienzo = "2014-01-01"
data_fecha_fin = today()

# Para ver las opciones del paquete utilizar
tq_transmute_fun_options()

# Defino la cantidad de activos
n_assets = 4

# Obtengo las acciones de interés ----
NVDA = tq_get("NVDA", get = "stock.prices", from = data_fecha_comienzo, to = data_fecha_fin, complete_cases = T)
MELI = tq_get("MELI", get = "stock.prices", from = data_fecha_comienzo, to = data_fecha_fin, complete_cases = T)
MSFT = tq_get("MSFT", get = "stock.prices", from = data_fecha_comienzo, to = data_fecha_fin, complete_cases = T)
AAPL = tq_get("AAPL", get = "stock.prices", from = data_fecha_comienzo, to = data_fecha_fin, complete_cases = T)


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

AAPL_returns <- AAPL %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily",
               type = "log",
               col_rename = "returns")

# Caracterización del portafolio----
mu = c(mean(NVDA_returns[[2]]), mean(MELI_returns[[2]]), mean(MSFT_returns[[2]]), mean(AAPL_returns[[2]]))
sd = c(sd(NVDA_returns[[2]]), sd(MELI_returns[[2]]), sd(MSFT_returns[[2]]), sd(AAPL_returns[[2]]))
P0 = c(last(NVDA[[8]]), last(MELI[[8]]), last(MSFT[[8]]), last(AAPL[[8]]))

# Simulación ----
m = 1000 #numero de simulaciones
T = 0.5
# Matriz de correlación
c = cbind(NVDA_returns[[2]], MELI_returns[[2]], MSFT_returns[[2]], AAPL_returns[[2]]) #Si el numero de filas difiere se producirá un error
Rho = cor(c)
Cholesky = chol(Rho)
z = matrix(rnorm(n_assets*m), nrow = m, ncol = n_assets) #Matriz de normales estandar
e = z %*% Cholesky #Genero numeros epsilon correlacionados
cor(e)

# Creo los precios correlacionados
PC = matrix(NA, m, n_assets)
for (i in 1:m) { #Bucle que pasa por cada simulacion
  for (k in 1:n_assets) { #Bucle que calcula el precio del activo k, en la simulación i
    PC[i,k] = P0[k]*exp((mu[k]-0.5*sd[k]^2)*T + sd[k]*sqrt(T)*e[i,k]) #Simulación i-ésima, activo k-ésimo
  }
}
hist(PC)
P0.m = matrix(rep(P0, m), m, n_assets, byrow = T) #Matriz con vector P0 repetido m veces
RLC = log(PC/P0.m) #Rendimiento logaritmico de los activos
cor(RLC)

# Grafico de correlación entre activos ----
par(mfrow=c(2,3))
plot(PC[,1], PC[,2])
plot(PC[,1], PC[,3])
plot(PC[,1], PC[,4])
plot(PC[,2], PC[,3])
plot(PC[,2], PC[,4])
plot(PC[,3], PC[,4])
