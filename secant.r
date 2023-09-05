secant<- function(f,x0,x1,tol,n) {
  options(digits=15)
  for (i in 1:n) {
    x2 <- x1 - f(x1) / ((f(x1) - f(x0)) / (x1 - x0)) 
    if (abs(x2 - x1) < tol) { 
      return(x2)
    }
    x0 <- x1
    x1 <- x2 
  }
  print('Too many iterations in method')
}
f <- function(x){
    100/(1+x^2)+atan(x)
}
secant(f,-9,-8,6)
