# Integración numérica con Fórmulas de Newton-Cotes abiertas.

# Función a integrar.
fn = function(x){
  val = x*sqrt(x^2 + 9)
  return(val)
}

int_newton.cotesopen = function(a, b, n){
  h = (b - a)/(n + 2)
  xo = a + h
  
  if(n == 0){
    result = 2*h*fn(xo)
    return(result)
  }
  else if(n == 1){
    x1 = b - h
    print(x1)
    #x1a = a + h
    #print(x1a)
    result = ((3/2)*h)*(fn(xo) + fn(x1))
    return(result)
  }
  else if(n == 2){
    x1 = xo + h
    x2 = b - h
    
    result = ((4/3)*h)*(2*fn(xo) - fn(x1) + 2*fn(x2))
    return(result)
  }
  else if(n == 3){
    x1 = xo + h
    x2 = xo + 2*h
    x3 = b - h
    
    result = ((5/24)*h)*(11*fn(xo) + fn(x1) + fn(x2) + 11*fn(x3))
    return(result)
  }
  else print("Grado del polinomio no programado")
}

int_newton.cotesopen(0, 4, 3)
