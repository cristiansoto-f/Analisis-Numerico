# Derivación numérica.

# La siguiente función calcula las derivadas de datos tabulados en todos los puntos (si es posible) o en el indicado.
# Los parámetros x e y son los datos tabulados iniciales.
# El parámetro ptos indica qué formula se utilizará pudiendo ser la de 2, 3 o 5 puntos.
# El parámetro opcional xindice calcula la derivada en un punto exacto. Es necesario tener en cuenta que el indice corresponde al valor de x en ese indice de la variable:
# es decir que si tengo x = (3, 4, 5) y quiero calcular la derivada en x = 4 el xindice debe ser 2, ya que R empieza a contar desde el 1. Recomiendo visualizar la tabla completa 
# sin el xindice para tener certeza de la derivada buscada. Con este parametro se retorna el valor, por lo cual puede ser guardado en una variable.
# El parámetro, por defecto como TRUE, H indica que las formulas serán progresivas, con H = F las formulas serán regresivas.
# El parámetro, por defecto como FALSE, endpoint indica si la formula es con punto medio o punto extremo (endpoint = T)

derivada = function(x, y, ptos, xindice, H = T, endpoint = F){
  if(ptos == 2 || ptos == 3 || ptos == 5){
    
    n = length(y)
    yprima = c(rep(NA,n))
  
    if(H == T){#Formulas progresivas----
      h = diff(x)[1] #Se asume que todos los valores estan separados por una h constante
      if(ptos == 2){
        if(missing(xindice)){
          for (i in 1:(n - 1)) {
            yprima[i] = (y[i+1] - y[i])/h
          }
          
          print("Aproximacion de la derivada con formula progresiva (h>0) con 2 puntos.")
          tabla = cbind.data.frame(x, y, yprima)
          return(tabla)
        }
        else{
          if(xindice == n){
            return("Este valor no se puede calcular")
          }
          else{
            print("Aproximacion de la derivada en x0 con formula progresiva (h>0) con 2 puntos.")
            yprima[xindice] = (y[xindice+1] - y[xindice])/h
            return(yprima[xindice])
          }
        }
      }else if(ptos == 3){
        if(endpoint == F){
          if(missing(xindice)){
            for (i in 2:(n - 1)) {
              yprima[i] = (-y[i-1] + y[i+1])/(2*h)
            }
            
            print("Aproximacion de la derivada por punto medio progresivo (h>0) con 3 puntos.")
            tabla = cbind.data.frame(x, y, yprima)
            return(tabla)
          }
          else{
            if(xindice == 1 || xindice == n){
              return("Este valor no se puede calcular")
            }
            else{
              print("Aproximacion de la derivada en x0 por punto medio progresivo (h>0) con 3 puntos.")
              yprima[xindice] = (-y[xindice-1] + y[xindice+1])/(2*h)
              return(yprima[xindice])
            }
          }
        }else{
          if(missing(xindice)){
            for (i in 1:(n - 2)) {
              yprima[i] = (-3*y[i] + 4*y[i+1] - y[i+2])/(2*h)
            }
            
            print("Aproximacion de la derivada por punto extremo progresivo (h>0) con 3 puntos.")
            tabla = cbind.data.frame(x, y, yprima)
            return(tabla)
          }
          else{
            if(xindice == n || xindice == n-1){
              return("Este valor no se puede calcular")
            }
            else{
              print("Aproximacion de la derivada en x0 por punto extremo progresivo (h>0) con 3 puntos.")
              yprima[xindice] = (-3*y[xindice] + 4*y[xindice+1] - y[xindice+2])/(2*h)
              return(yprima[xindice])
            }
          }
        }
      }else{
        if(endpoint == F){
          if(missing(xindice)){
            for (i in 3:(n - 2)) {
              yprima[i] = (y[i-2] - 8*y[i-1] + 8*y[i+1] - y[i+2])/(12*h)
            }
            
            print("Aproximacion de la derivada por punto medio progresivo (h>0) con 5 puntos.")
            tabla = cbind.data.frame(x, y, yprima)
            return(tabla)
          }
          else{
            if(xindice == n || xindice == n-1 || xindice == 1 || xindice == 2){
              return("Este valor no se puede calcular")
            }
            else{
              print("Aproximacion de la derivada en x0 por punto medio progresivo (h>0) con 5 puntos.")
              yprima[xindice] = (y[xindice-2] - 8*y[xindice-1] + 8*y[xindice+1] - y[xindice+2])/(12*h)
              return(yprima[xindice])
            }
          }
        }else{
          if(missing(xindice)){
            for (i in 1:(n - 4)) {
              yprima[i] = (-25*y[i] + 48*y[i+1] - 36*y[i+2] + 16*y[i+3] - 3*y[i+4])/(12*h)
            }
            
            print("Aproximacion de la derivada por punto extremo progresivo (h>0) con 5 puntos.")
            tabla = cbind.data.frame(x, y, yprima)
            return(tabla)
          }
          else{
            if(xindice == n || xindice == n-1 || xindice == n-2 || xindice == n-3){
              return("Este valor no se puede calcular")
            }
            else{
              print("Aproximacion de la derivada en x0 por punto medio progresivo (h>0) con 5 puntos.")
              yprima[xindice] = (-25*y[xindice] + 48*y[xindice+1] - 36*y[xindice+2] + 16*y[xindice+3] - 3*y[xindice+4])/(12*h)
              return(yprima[xindice])
            }
          }
        }
      }
    }else{#Formulas regresivas ----
      h = -diff(x)[1]
      if(ptos == 2){
        if(missing(xindice)){
          for (i in 2:n) {
            yprima[i] = (y[i-1] - y[i])/h
          }
          
          print("Aproximacion de la derivada con formula regresiva (h<0) con 2 puntos.")
          tabla = cbind.data.frame(x, y, yprima)
          return(tabla)
        }
        else{
          if(xindice == 1){
            return("Este valor no se puede calcular")
          }
          else{
            print("Aproximacion de la derivada en x0 con formula regresiva (h<0) con 2 puntos.")
            yprima[xindice] = (y[xindice-1] - y[xindice])/h
            return(yprima[xindice])
          }
        }
      }else if(ptos == 3){
        if(endpoint == F){
          if(missing(xindice)){
            for (i in 2:(n - 1)) {
              yprima[i] = (-y[i+1] + y[i-1])/(2*h)
            }
            
            print("Aproximacion de la derivada por punto medio regresivo (h<0) con 3 puntos.")
            tabla = cbind.data.frame(x, y, yprima)
            return(tabla)
          }
          else{
            if(xindice == 1 || xindice == n){
              return("Este valor no se puede calcular")
            }
            else{
              print("Aproximacion de la derivada en x0 por punto medio regresivo (h<0) con 3 puntos.")
              yprima[xindice] = (-y[xindice+1] + y[xindice-1])/(2*h)
              return(yprima[xindice])
            }
          }
        }else{
          if(missing(xindice)){
            for (i in 3:n) {
              yprima[i] = (-3*y[i] + 4*y[i-1] - y[i-2])/(2*h)
            }
            
            print("Aproximacion de la derivada por punto extremo regresivo (h<0) con 3 puntos.")
            tabla = cbind.data.frame(x, y, yprima)
            return(tabla)
          }
          else{
            if(xindice == 1 || xindice == 2){
              return("Este valor no se puede calcular")
            }
            else{
              print("Aproximacion de la derivada en x0 por punto extremo regresivo (h<0) con 3 puntos.")
              yprima[xindice] = (-3*y[xindice] + 4*y[xindice-1] - y[xindice-2])/(2*h)
              return(yprima[xindice])
            }
          }
        }
      }else{
        if(endpoint == F){
          if(missing(xindice)){
            for (i in 3:(n - 2)) {
              yprima[i] = (y[i+2] - 8*y[i+1] + 8*y[i-1] - y[i-2])/(12*h)
            }
            
            print("Aproximacion de la derivada por punto medio regresivo (h<0) con 5 puntos.")
            tabla = cbind.data.frame(x, y, yprima)
            return(tabla)
          }
          else{
            if(xindice == n || xindice == n-1 || xindice == 1 || xindice == 2){
              return("Este valor no se puede calcular")
            }
            else{
              print("Aproximacion de la derivada en x0 por punto medio regresivo (h<0) con 5 puntos.")
              yprima[xindice] = (y[xindice+2] - 8*y[xindice+1] + 8*y[xindice-1] - y[xindice-2])/(12*h)
              return(yprima[xindice])
            }
          }
        }else{
          if(missing(xindice)){
            for (i in 5:n) {
              print(i)
              yprima[i] = (-25*y[i] + 48*y[i-1] - 36*y[i-2] + 16*y[i-3] - 3*y[i-4])/(12*h)
            }
            
            print("Aproximacion de la derivada por punto extremo regresivo (h<0) con 5 puntos.")
            tabla = cbind.data.frame(x, y, yprima)
            return(tabla)
          }
          else{
            if(xindice == 1 || xindice == 2 || xindice == 3 || xindice == 4){
              return("Este valor no se puede calcular")
            }
            else{
              print("Aproximacion de la derivada en x0 por punto extremo regresivo (h<0) con 5 puntos.")
              yprima[xindice] = (-25*y[xindice] + 48*y[xindice-1] - 36*y[xindice-2] + 16*y[xindice-3] - 3*y[xindice-4])/(12*h)
              return(yprima[xindice])
            }
          }
        }
      }
    }
    
  }else return("Numero de puntos invalido")
}

derivada_segunda = function(x, y, xindice){
  h = diff(x)[1]
  n = length(y)
  yprima = c(rep(NA,n))
  
  if(missing(xindice)){
    for (i in 2:(n - 1)) {
      yprima[i] = (y[i-1] - 2*y[i] + y[i+1])/(h^2)
    }
    
    tabla = cbind.data.frame(x, y, yprima)
    return(tabla)
  }
  else{
    if(xindice == 1 || xindice == n){
      return("Este valor no se puede calcular")
    }
    else{
      yprima[xindice] = (y[xindice-1] - 2*y[xindice] + y[xindice+1])/(h^2)
      return(yprima[xindice])
    }
  }
}

x = c(0, 0.05, 0.1, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.5)
y = c(120, 95.67052, 77.25528, 63.12629, 52.15021, 43.52512, 36.67519, 31.18120, 26.73410, 23.10338, 20.11523)
derivada(x, y, 5, 3) #Derivada en x0 = 0.1 (indice 3) utilizando la formula de 5 puntos centrados
derivada(x, y, 2, 6, H = F) #Derivada en x0 = 0.25 (indice 6) utilizando la formula regresiva de 2 puntos
derivada_segunda(x, y, 4)
