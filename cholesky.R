# Factorización de Cholesky

A = matrix(data = c(4, -1, 1, -1, 4.25, 2.75, 1, 2.75, 3.5), nrow = 3, byrow = T)
A

fact_cholesky = function(A){
  n = ncol(A)
  i = 1
  L = matrix(data = 0, nrow = n, ncol = n, byrow = TRUE) 
  #Paso 1
  L[1,1] = (A[1,1])^(0.5)
  #Paso 2
  j = i + 1
  for (j in j:n) {
    L[j,1] = A[j,1]/L[1,1]
  }
  #Paso 3
  for (i in (i+1):(n-1)) {
    #Paso 4
    sum1 = 0
    k = 1
    for (k in k:(i - 1)) {
      sum1 = sum1 + L[i,k]^2
    }
    L[i,i] = sqrt(A[i,i] - sum1)
    #Paso 5
    for (j in (i+1):n) {
      sum2 = 0
      k = 1
      for (k in k:(i - 1)) {
        sum2 = sum2 + (L[j,k]*L[i,k])
      }
      L[j,i] = (A[j,i] - sum2)/L[i,i]
    }
  }
  #Paso 6
  sum3 = 0
  k = 1
  for (k in k:(n - 1)) {
    sum3 = sum3 + (L[n,k]^2)
  }
  L[n,n] = sqrt((A[n,n] - sum3))
  #Paso 7
  return(L)
}

# Compruebo 
B = fact_cholesky(A)
B%*%t(B)
A
