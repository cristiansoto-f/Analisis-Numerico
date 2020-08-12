# Resolucion de una integral por el método de Monte Carlo.
# Resolución del ejercicio:
# Una variable aleatoria X, con dominio en los números reales, tiene distribución Gumbel, y su densidad está dada por la siguiente función, donde "μ" es un parámetro de ubicación y "β" es un parámetro de escalamiento. Utilizando simulación de Monte Carlo, estime P(X < 0) para μ=0,5 y β=2.

fn = function(x){
  f = (1/2)*exp(-((x - 0.5)/2) - exp(-((x - 0.5)/2)))
  return(f)
}

#Dibujo de la funcion
xes = seq(from=-6, to=15, by=0.01)
plot(xes, fn(xes), type="l", col="red")
abline(h=0)

n = 100000
a = -5
b = 0

u = a + (b - a)*runif(n)

AlturaPromedio = 1/n*sum(fn(u))
AnchoBase = b - a
(IntegralMonteCarlo = AlturaPromedio*AnchoBase) #0.277968727851918
