# Integración numérica con Fórmulas de Newton-Cotes cerradas.

# Función a integrar.
fn = function(x){
  val = x*sqrt(x^2 + 9)
  return(val)
}

int_newton.cotes = function(a, b, n){
  h = (b - a)/n
  
  if(n == 1){
    xo = a
    x1 = b

    result = (h/2)*(fn(xo) + fn(x1))
    return(result)
  }
  else if(n == 2){
    xo = a
    x2 = b
    
    x1 = (xo + x2)/2
    #x1a = a + (b -a)/2
    
    result = (h/3)*(fn(xo) + 4*fn(x1) + fn(x2))
    return(result)
  }
  else if(n == 3){
    xo = a
    x3 = b
    
    x1 = xo + h
    x2 = xo + 2*h
    
    result = ((3/8)*h)*(fn(xo) + 3*fn(x1) + 3*fn(x2) + fn(x3))
    return(result)
  }
  else if(n == 4){
    xo = a
    x4 = b
    
    x1 = xo + h
    x2 = xo + 2*h
    x3 = xo + 3*h
    
    result = ((2/45)*h)*(7*fn(xo) + 32*fn(x1) + 12*fn(x2) + 32*fn(x3) + 7*fn(x4))
    return(result)
  }
  else print("Grado del polinomio no programado")
}

(int_newton.cotes(0, 4, 4))
