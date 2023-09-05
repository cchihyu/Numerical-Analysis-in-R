
diff<-function(f,a,h){
    dif=(-11*f(a)+18*f(a+h)-9*f(a+2*h)+2*f(a+3*h))/(6*h)
    print(dif)
}
f<-function(x){
    (sqrt(x^2+5)+exp(1/x))^(sin(-(x-2)^(1/5)))
}
g<-function(x){
    (-11*f(x)+18*f(x+0.05)-9*f(x+0.1)+2*f(x+0.15))/(0.3)
}
diff(f,8.95004994428,0.05)