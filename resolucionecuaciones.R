# Soluciones de las ecuaciones en una variable
# Algoritmos del capitulo 2 del libro Analisis Numerico 10ma edicion. Burden
# Encuentran las raices de la funcion.
#

fn <- function(x) #Funcion 
{
  f <- x^3 - 7*x^2 + 14*x - 6
  return(f)
}

dfn <- function(x) # Derivada de la funcion para para Newton Raphson
{
  df <- 3*x^2 - 14*x + 14
  return(df)
}

g <- function(x){ # Funcion g(x) para la iteracion de punto fijo
  gn <- -(x^3 - 7*x^2 - 6)/14
  return(gn)
}

# Metodo de Biseccion ----
metodo_biseccion <- function(a,b,tol,n)
{
  if(fn(a)*fn(b) < 0)
  {
    #Paso 1
    i = 1
    funA = fn(a)
    #Paso 2
    while(i <= n)
    {
      #Paso 3
      p = a + (b-a)/2 
      funP = fn(p)
      #Paso 4
      if(funP == 0 || (b-a)/2 < tol)
      {
        return(p)
      }
      #Paso 5
      i = i + 1
      #Paso 6
      if(funA * funP > 0)
      {
        a = p
        funA = funP 
      } else{
        b = p
      }
    }
    #Paso 7
    return(paste('El metodo fallo luego de ', n, 'iteraciones'))
  }
  else
  {
    return("No se cumple el teorema de bolzano")
  }
}

# Metodo de Newton Raphson ----
newton_raphson <- function(p0, tol, n)
{
  #Paso 1
  i = 1
  #Paso 2
  while(i <= n)
  {
    #Paso 3
    p = p0 - fn(p0)/dfn(p0)
    #Paso 4
    if(abs(p - p0) < tol)
      return(p)
    #Paso 5
    i = i + 1
    #Paso 6
    p0 = p
  }
  #Paso 7
  return(paste('El metodo fallo luego de ', n, 'iteraciones'))
}

# Metodo de Secante ----
metodo_secante <- function(p0, p1, tol, n)
{
  #Paso 1
  i = 2
  q0 = fn(p0)
  q1 = fn(p1)
  #Paso 2
  while(i <= n)
  {
    #Paso 3
    p = p1 -(q1*(p1 - p0))/(q1 - q0)
    #Paso 4
    if(abs(p - p1) < tol)
    {
      return(p)
    }
    #Paso 5
    i = i + 1
    #Paso 6
    p0 = p1
    q0 = q1
    p1 = p
    q1 = fn(p)
  }
  #Paso 7
  return(paste('El metodo fallo luego de ', n, 'iteraciones'))
}

# Metodo de falsa posicion ----
falsa_posicion <- function(p0, p1, tol, n)
{
  if(fn(p0)*fn(p1) < 0)
  {
    #Paso 1
    i = 2
    q0 = fn(p0)
    q1 = fn(p1)
    #Paso 2
    while(i <= n)
    {
      #Paso 3
      p = p1 -(q1*(p1 - p0))/(q1 - q0)
      #Paso 4
      if(abs(p - p1) < tol)
      {
        return(p)
      }
      #Paso 5
      i = i + 1
      q = fn(p)
      #Paso 6
      if(q * q1 < 0)
      {
        p0 = p1
        q0 = q1
      }
      #Paso 7
      p1 = p
      q1 = q
    }
    #Paso 8
    return(paste('El metodo fallo luego de ', n, 'iteraciones'))
  } else 
    return("No se cumple el teorema de bolzano")
}

# Metodo de punto fijo ----
# Este metodo requiere una funcion un funcion g(x) proveniente de f(x).
# Consultar bibliografia.

punto_fijo <- function(p0,tol,n)
{
  #Paso 1
  i = 1
  #Paso 2
  while(i <= n)
  {
    #Paso 3
    p = g(p0)
    #Paso 4
    if(abs(p-p0) < tol)
    {
      return(p)
    }
    #Paso 5
    i = i + 1
    #Paso 6
    p0 = p
  }
  #Paso 7
  return(paste('El metodo fallo luego de ', n, 'iteraciones'))
}

# Ejemplos ----

#Para visualizar mas decimales utilizar
options(digits = 16)

#Dibujo de la funcion a la cual se le buscaran las raices
xes = seq(from=0, to=5, by=0.01)
plot(xes, fn(xes), type="l", col="red")
abline(h=0)

#Pruebas
#Se pueden cambiar las raices a encontrar cambiando los puntos iniciales.
b = (metodo_biseccion(3.2, 5, 0.1^17, 100)) # 3.414213562373098
n = newton_raphson(4, 0.0000001, 200) #3.414213562373097
s = metodo_secante(3.1, 3.6, 0.0000001, 200) #3.414213562373095
f = falsa_posicion(3.1, 3.6, 0.0000001, 200) #3.414213535857162
p = punto_fijo(3.01, 0.0000000000001, 1000) #3.414213562372043

fn(b) # El resultado es 0, por lo que se verifica que en b hay una raiz.
