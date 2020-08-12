# Simulación de numeros aleatorios con distribución compuesta.
# Caso unidad de negocios.
# Ejemplo: Suponga que el número de siniestros de una unidad de negocios de una compañía de seguros se distribuye como una Poisson con λ = 37 mientras que el monto de los 
# siniestros ocurridos se distribuye como una variable Normal con media 100 y desvío estándar 20. Sea S el monto total de siniestros de la unidad de negocios.
# Utilizando Simulación de Monte Carlo, estime el valor esperado y el desvío estándar de S.

M = 10000
resultado = matrix(NA, nrow = M, ncol = 2) # Matriz utilizada para almacenar
for (i in 1:M) {
  N = rpois(n = 1, lambda = 37) # numero de siniestros aleatorios
  resultado[i, 1] = N
  Xi = rnorm(n = N, mean = 100, sd = 20) # monto de los siniestros ocurridos se distribuye como una variable normal 
  S = sum(Xi) # montos totales de la unidad de siniestros
  resultado[i, 2] = S
}

(S.Esperanza = mean(resultado[,2]))
(S.sd = sd(resultado[,2]))
