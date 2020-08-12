# Integración compuesta.

# Función a integrar
fn = function(x){
  val = x*sqrt(x^2 + 9)
  return(val)
}

simpson.compuesto = function(a, b, n){
  if((n %% 2) == 0){
    #Paso 1
    h = (b-a)/n
    
    #Paso 2
    xio = fn(a) + fn(b)
    xi1 = 0 #Suma impar
    xi2 = 0 #Suma par
    
    #Paso 3
    for (i in 1:(n-1)) {
      #Paso 4
      x = a + i*h
      #Paso 5 
      if((i %% 2) == 0){
        xi2 = xi2 + fn(x)
      }else{
        xi1 = xi1 + fn(x)
      }
    }
    #Paso 6
    val = h*(xio + 2*xi2 + 4*xi1)/3
    return(val)
  }
  else return("n debe ser par")
}

(simpson.compuesto(0,4,200))

trapecio.compuesto = function(a, b, n){
  h = (b-a)/n
  
  xio = fn(a) + fn(b)
  xi = 0
  for (i in 1:(n-1)) {
    x = a + i*h
    xi = xi + fn(x)
  }
  val = h*(xio + 2*xi)/2
  return(val)
}
(trapecio.compuesto(0,4,401))

puntomedio.compuesto = function(a, b, n){
  if((n %% 2) == 0){
    h = (b-a)/(n+2)
    
    xi2 = 0
    for (i in -1:(n+1)) {
      x = a + (i+1)*h
      if((i %% 2) == 0){
        xi2 = xi2 + fn(x)
      }
    }
    val = 2*h*xi2
    return(val)
  }
  else return("n debe ser par")
}

(puntomedio.compuesto(0,4,2000))
