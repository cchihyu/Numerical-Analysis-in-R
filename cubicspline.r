tri.sol<-function(a,b,c,d){
   cp<-c()
   dp<-c()
   sol<-matrix(0,length(b),1)
   cp[1]=c[1]/b[1]
   for(i in 2:(length(b)-1)){
      cp[i]=c[i]/(b[i]-a[i-1]*cp[i-1])

   }
   dp[1]=d[1]/b[1]
   for(i in 2:length(b)){
      dp[i]=(d[i]-a[i-1]*dp[i-1])/(b[i]-a[i-1]*cp[i-1])
   }
   sol[length(b)]=dp[length(b)]
   for(i in (length(d)-1):1){
      sol[i]=dp[i]-cp[i]*sol[i+1]
   }
   return(sol)
}
qsKAN<-function(absc,func){
    options(digits=15)
    l=length(absc)
    a<-matrix(0,l,1)
    b<-matrix(0,l,1)
    c<-matrix(0,l,1)
    d<-matrix(0,l,1)
    h<-matrix(0,l-1,1)
    for (i in 1:(l-1)) {
       h[i]=absc[i+1]-absc[i]
    }
    #L
    L<-matrix(0,l-2,1)
    for (i in 1:(l-3)) {
       L[i]=h[i+1]
    }
    L[l-2]=h[l-2]-h[l-1]^2/h[l-2]
    #U
    U<-matrix(0,l-2,1)
    for (i in 2:(l-2)) {
       U[i]=h[i+1]
    }
    U[1]=h[2]-h[1]^2/h[2]
    #D
    D<-matrix(0,l-1,1)
    for (i in 2:(l-2)) {
       D[i]=2*(h[i]+h[i+1])
    }
    D[1]=3*h[1]+2*h[2]+h[1]^2/h[2]
    D[l-1]=3*h[l-1]+2*h[l-2]+h[l-1]^2/h[l-2]
    #b
   B<-matrix(0,l-2,1)
    for (i in 1:(l-3)) {
       B[i]=3*(func(absc)[i+2]-func(absc)[i+1])/h[i+1]-3*(func(absc)[i+1]-func(absc)[i])/h[i]
    }
    B[l-2]=3*((func(absc)[l]-func(absc)[l-1])/h[l-1]-(func(absc)[l-1]-func(absc)[l-2])/h[l-2])
    for(i in 2:(l-1)){
       c[i]=tri.sol(L,D,U,B)[i]
    }
    c[1]=(1+h[1]/h[2])*c[2]-h[1]/h[2]*c[3]
     c[l]=(1+h[l-1]/h[l-2])*c[l-1]-h[l-1]/h[l-2]*c[l-2]
    for (i in 1:l) {
       a[i]=func(absc)[i]
    }
   for (i in 1:l-1) {
       d[i]=(c[i+1]-c[i])/3/h[i]
    }
    for (i in 1:l-1) {
       b[i]=(a[i+1]-a[i])/h[i]-h[i]/3*(2*c[i]+c[i+1])
    }
    coe=cbind(a,b,c,d)
    return(coe)
}

