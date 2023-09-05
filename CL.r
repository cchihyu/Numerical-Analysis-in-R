n=3
 A<-matrix(c(4,12,-16,12,37,-43,-16,-43,98),3)
 L<-matrix(0,n,n)
 L[1,1]=sqrt(A[1,1])
 for (i in 2:n) {
     L[i,1]=A[i,1]/L[1,1]
 }
 for (k in 2:(n-1)) {
     t=0
     for (j in 1:(k-1)) {        
        t=t+L[k,j]^2
    }
    L[k,k]=sqrt(A[k,k]-t)
 }

  for (k in 2:(n-1)) {
     t=0
     for (i in (k+1):n) {      
         for (j in 1:(k-1)) {
            t=t+L[i,j]*L[k,j]
         } 
         L[i,k]=(A[i,k]-t)/L[k,k]
    }
 }
 t=0
 for (j in 1:(n-1)) {
    t=t+L[n,j]^2
 }
 L[n,n]=sqrt(A[n,n]-t)
 print(L)
 print(t(L))