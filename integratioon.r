simpinteg<- function(f,a,b,n){
    options(digits=12)
    m=n/2
    x<-c()
    for (i in 0:n) {
       x[i+1]=i*(b-a)/n+a
    }
    integ=0
    for (i in 1:m) {
       integ=integ+((x[2*i+1]-x[2*i-1])/6)*(f(x)[2*i-1]+4*f(x)[2*i]+f(x)[2*i+1])
    }
    print(integ)
}
f<-function(x){
    sqrt(x^2+1)*sin(x^3-x+2)
}
simpinteg(f,10,20,2048)