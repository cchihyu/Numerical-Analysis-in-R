newton <- function(f,a,tol,n) {
  require(numDeriv)
  x0 <- a 
  k <- n 
  fa <- f(a)
  if (fa == 0.0) {
    return(a)
  }
  for (i in 1:n) {
    dx <- genD(func = f, x = x0)$D[1] 
    x1 <- x0 - (f(x0) / dx) 
    k[i] <- x1
    if (abs(x1 - x0) < tol) {
      root.approx <- tail(k, n=1)
      res <- list('root approximation' = root.approx, 'iterations' = k)
      return(res)
    }
    x0 <- x1
  }
  print('Too many iterations in method')
}
f <-function(x){
  -sin(x)*exp(cos(x))-2.1*cos(2.1*x)
}
newton(f,-6,1e-10,1000)
