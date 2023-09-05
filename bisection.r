options(digits=10)
bis <- function(f,a,b,tol,n) {
  for (i in 1:n) {
    c <- (a + b) / 2 
    if ((f(c) == 0) || ((b - a) / 2) < tol) {
      return(c)
    }
    ifelse(sign(f(c)) == sign(f(a)), 
           a <- c,
           b <- c)
  }
  print(c)
}
f <-function(x){
  -sin(x)*exp(cos(x))-2.1*cos(2.1*x)
}
bis(f,-10,-4,1e-10,25)
