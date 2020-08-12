# Interpolacion y aproximacion lineal.
# Algoritmos de interpolacion utilizados.


# Interpolacion iterada de Neville ----
Neville = function(x, y, x0) {
  
  n <- length(x)
  q <- matrix(data = 0, n, n)
  q[,1] <- y
  
  #Paso 1
  for (i in 2:n) {
    for (j in i:n) {
      q[j,i] <- ((x0 - x[j-i+1]) * q[j,i-1] - (x0 - x[j]) * q[j-1,i-1]) / (x[j] - x[j-i+1])
    }
  }
  
  #Paso 2
  #Tabla completa:
  #res <- list('Approximated value'=q[n,n], 'Neville iterations table'=q)
  return(q[n,n])
}


#Diferencias dividadas ----
Diferencias_Divididas = function(x,y)
{
  n = length(y)
  dif_divM <- matrix(NA, nrow = length(y), ncol = n)
  names = c("y",paste("D",1:(n-1),sep="") )
  dimnames(dif_divM) = list(0:(n-1),names)
  coeff <- c()
  
  for(i in 1:n) {
    dif_divM[i,1] <- y[i]
  }
  
  coeff <- c(coeff, dif_divM[1,1])
  
  for(i in 1:(n-1)) {
    for(j in 1:i) {
      dif_divM[(i+1), (j+1)] <- (dif_divM[(i+1),j] - dif_divM[i,j])/(x[i+1] - x[i+1-j])
      if((i+1)==(j+1)) {
        coeff <- c(coeff, dif_divM[(i+1),(j+1)])
      }
    }
  }
  
  #print("Tabla de diferencias divididas:")
  #print(dif_divM)
  #print("Coeficientes del polinomio:")
  #print(coeff)
  return(coeff)
}

# Polinomio interpolante de Lagrange ----
#Ecuacion del polinomio interpolante de lagrange evaluada en x
EcuacionPolinomioInterpolante = function(coef, xes, x){
  if(length(coef) != length(xes)) {return("Error en los datos ingresados")}
  P = 0
  for (n in 1:length(xes)) {
    P  = P + (coef[n] * recursiva(x, xes, n-1))  
  }
  return(P)
}

#Funcion recursiva utilizada para  ir escribiendo el polinomio
recursiva = function(x, xes, n){
  if(n == 0) {return(1)}
  else if(n == 1){
    return(x - xes[1])
  }
  else{
    return((x - xes[n]) * recursiva(x, xes, n - 1))
  }
}

# Expresion del polinomio de lagrange en la consola ----
EscriboPolinomioInterpolante = function(coef, xes){
  if(length(coef) != length(xes)) {return("Error en los datos ingresados")}
  P = ""
  for (n in 1:length(xes)) {
    if(n == 1){
      P = paste0(P, sprintf(fmt = "%3.10f + ", coef[1]))
    }else{
      P = paste0(P, sprintf(fmt = "%3.10f*(%s", coef[n], recursiva2(xes, n-1)))
    }
  }
  return(P)
}

#Funcion recursiva utilizada para ir escribiendo el polinomio.
recursiva2 = function(xes, n){
  if(n == 0) {return(sprintf("H"))}
  else if(n == 1){
    return(sprintf(fmt = "x - %3.10f) + ", xes[1]))
  }
  else{
    return(sprintf(fmt = "x - %s)*(%s", xes[n], recursiva2(xes, n-1)))
  }
}
#EscriboPolinomioInterpolante(coeficientes, x)

# Trazador cubico natural ----
TrazadorCubicoNatural = function(x,y)
{
  n = length(y)
  j = n - 1
  
  a = y
  b = c(rep(NA,n))
  c = c(rep(NA,n))
  d = c(rep(NA,n))
  
  A = c(rep(NA,n))
  h = c(rep(NA,n))
  l = c(rep(NA,n))
  u = c(rep(NA,n))
  z = c(rep(NA,n))
  
  #Paso 1
  for (i in 1:j) { 
    h[i] = x[i + 1] - x[i]
  }
  
  
  #Paso 2
  for (i in 1:j) { 
    if(i != 1){
      A[i] = (3 * (a[i + 1] - a[i])/(h[i])) - (3 * (a[i] - a[i - 1]) /h[i - 1])
    }
  }
  
  
  #Paso 3
  l[1] = 1
  u[1] = 0
  z[1] = 0
  
  #Paso 4
  for (i in 2:j) {
    l[i] = 2 * (x[i + 1] - x[i - 1]) - h[i - 1] * u[i - 1]
    u[i] = h[i]/l[i]
    z[i] = (A[i] - h[i - 1] * z[i - 1])/l[i]
  }
  
  #Paso 5
  l[n] = 1
  z[n] = 0
  c[n] = 0
  
  #Paso 6
  for (i in j:1) {
    c[i] = z[i] - u[i] * c[i + 1]
    b[i] = (a[i + 1] - a[i])/h[i] - h[i] * (c[i + 1] + 2*c[i])/3
    d[i] = (c[i + 1] - c[i])/(3*h[i])
  }
  
  #Paso 7
  results = matrix(rep(NA, 4*j), nrow = j, ncol = 4, byrow = F)
  for (k in 1:j) {
    results[k, 1] = a[k]
    results[k, 2] = b[k]
    results[k, 3] = c[k]
    results[k, 4] = d[k]
  }
  return(results)
  
}

# Trazador Cubico Condicionado ----
TrazadorCubicoCondicionado = function(x, y, fpo, fpn){
  n = length(y)
  j = n - 1
  
  a = y
  b = c(rep(NA,n))
  c = c(rep(NA,n))
  d = c(rep(NA,n))
  
  A = c(rep(NA,n))
  h = c(rep(NA,n))
  l = c(rep(NA,n))
  u = c(rep(NA,n))
  z = c(rep(NA,n))
  
  #Paso 1
  for (i in 1:j) { 
    h[i] = x[i + 1] - x[i]
  }
  
  #Paso 2
  A[1] = 3*(a[2] - a[1])/(h[1]) - 3*fpo#Atencion indices y division
  A[n] = 3*fpn - 3*(a[n] - a[n-1])/(h[n-1])
  
  #Paso 3
  for (i in 2:j) {
    A[i] = 3*(a[i+1] - a[i])/(h[i]) - 3*(a[i] - a[i-1])/(h[i-1])
  }
  
  #Paso 4
  l[1] = 2*h[1]
  u[1] = 0.5
  z[1] = A[1]/l[1]
  
  #Paso 5
  for (i in 2:j) {
    l[i] = 2*(x[i+1] - x[i-1]) - h[i-1]*u[i-1]
    u[i] = h[i]/l[i]
    z[i] = (A[i] - h[i-1]*z[i-1])/l[i]
  }
  
  #Paso 6
  l[n] = h[n-1]*(2 - u[n-1])
  z[n] = (A[n] - h[n-1]*z[n-1])/l[n]
  c[n] = z[n]
  
  #Paso 7
  for (i in j:1) {
    c[i] = z[i] - u[i]*c[i+1]
    b[i] = (a[i+1] - a[i])/h[i] - h[i] * (c[i+1] + 2*c[i])/3
    d[i] = (c[i+1] - c[i])/(3*h[i])
  }
  
  #Paso 8
  results = matrix(rep(NA, 4*j), nrow = j, ncol = 4, byrow = F)
  for (k in 1:j) {
    results[k, 1] = a[k]
    results[k, 2] = b[k]
    results[k, 3] = c[k]
    results[k, 4] = d[k]
  }
  return(results)
}

#Escritura e interpolacion cubic splines ----

#Escribre los polinomios interpolantes
S_function = function(x, results)
{
  n = length(x)
  j = n-1
  for (i in 1:j) {
    msg = sprintf(fmt = "S%i: %f + (%f*(x - %f)) + (%f*(x - %f))^2 + (%f*(x - %f))^3 en [%f, %f] ", 
                  i, results[i,1], results[i,2], x[i], results[i,3], x[i], results[i,4], x[i], x[i], x[i+1])
    print(msg)
  }
}

Interpolar_Cubic = function(xo, x, results)
{
  n = length(x)
  j = n-1
  for (i in 1:j) {
    if(xo <  x[i + 1]){
      val = results[i,1] + (results[i,2]*(xo - x[i])) + (results[i,3]*(xo - x[i])^2) + (results[i,4]*(xo - x[i])^3)
      return(val)
    }
  }
}

# Ejemplos ----

# Escribo polinomio interpolante de Lagrange.
x = c(1, 3, 6, 9, 12)
y1 = c(14.5170, 15.2212, 16.3175, 17.4250, 16.5450)

coef_tasas = Diferencias_Divididas(x, y1)
EscriboPolinomioInterpolante(coef_tasas, x)


# Escribo polinomios interpolantes de cubic splines.
# Ejemplo Burden Faires.
x = c(0, 1, 2, 3)
y = c(1, exp(1), exp(2), exp(3))
fpo = 1 #Derivada en el punto inicial
fpn = exp(3) #Derivada en el punto final

estructura = TrazadorCubicoCondicionado(x, y, fpo, fpn)
S_function(x, estructura)

estructura2 = TrazadorCubicoNatural(x, y)
S_function(x, estructura2)

# Completar una columna----
# Datos iniciales incluyendo datos a interpolar.
t = seq(12)
i = c(14.5170, NA, 15.2212, NA, NA, 16.3175, NA, NA, 17.4250, NA, NA, 16.5450)

# Datos iniciales que si se tienen.
x = c(1, 3, 6, 9, 12)
y1 = c(14.5170, 15.2212, 16.3175, 17.4250, 16.5450)

# Interpolacion: diferencias divididas

coef_tasas = Diferencias_Divididas(x, y1)

i_PolInterpolante = c(14.5170, NA, 15.2212, NA, NA, 16.3175, NA, NA, 17.4250, NA, NA, 16.5450) #Columna a completar
for (k in 1:length(t)) {
  if(is.na(i_PolInterpolante[k]))
  {
    i_PolInterpolante[k] = EcuacionPolinomioInterpolante(coef_tasas, x, k)
    msg = sprintf(fmt = "TasaL(%i) = %3.7f", #Se agrego el %3.7f para 7 decimales, sin eso hay un error de redondeo
                  k, EcuacionPolinomioInterpolante(coef_tasas, x, k))
    print(msg)
  }
}

# Interpolacion: Por metodo iterado de neville
i_PolInterpolante = c(14.5170, NA, 15.2212, NA, NA, 16.3175, NA, NA, 17.4250, NA, NA, 16.5450)
for (k in 1:length(t)) {
  if(is.na(i_PolInterpolante[k]))
  {
    i_PolInterpolante[k] = Neville(x, y1, k)
    msg = sprintf(fmt = "TasaL(%i) = %3.7f",
                  k, Neville(x, y1, k))
    print(msg)
  }
}

# Interpolacion: Por trazador cubico natural
estructura = TrazadorCubicoNatural(x, y1)
i_TrazadorCubico = c(14.5170, NA, 15.2212, NA, NA, 16.3175, NA, NA, 17.4250, NA, NA, 16.5450)
for (k in 1:length(t)) {
  if(is.na(i_TrazadorCubico[k]))
  {
    i_TrazadorCubico[k] = Interpolar_Cubic(k, x, estructura)
    msg = sprintf(fmt = "TasaC(%i) = %3.7f", 
                  k, Interpolar_Cubic(k, x, estructura))
    print(msg)
  }
}

# Ejemplo grafico ----
#Realice un gráfico que contenga lo siguiente (ajuste los valores de los ejes para que todas líneas se vean completas):
#(i) Pares (mes, Tasa) originales con puntos sin unir, de color negro.
#(ii) Polinomio de Lagrange para valores desde 1 hasta 12 meses (use al menos 1000 pares de puntos para graficar), usando línea continua verde.
#(iii) Trazador cúbico para valores desde 1 hasta 12 meses (use al menos 1000 pares de puntos para graficar), usando línea continua roja.

time = seq(from=1, to=12, length.out = 1000)
Px = rep(NA,1000)
for (i in 1:1000) {
  Px[i] =  EcuacionPolinomioInterpolante(coef_tasas, x, time[i])
}

plot(x, y1, 
     xlab = "tiempo (meses)", 
     ylab = "i, tasa", 
     main = "Tasas de Interés", 
     type = "p",
     ylim = c(min(Px), max(Px)),
     pch = 19
)

#Agrego el polinomio de Lagrange
lines(time, Px, col = "chartreuse")

#Polinomio Cubic
time = seq(from=1, to=12, length.out = 1000)
Px = rep(NA,1000)
Px[1000] = 16.5450
for (i in 1:999) {
  Px[i] = (Interpolar_Cubic(time[i], x, estructura)) 
}

lines(time, Px, col = "red")

#Leyenda
legend("bottomright",
       c("Pol. Trazador Cúbico","Pol. Interp. de Lagrange"),
       fill=c("red","chartreuse"),
       cex = 0.75)
