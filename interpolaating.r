#compute the coefficient wrsp to absc and func
find.coe<-function(absc,func){
 options(digits=15)
 l=length(absc)
 ddt<-matrix(0,l,l)
 for(i in 1:l){
     ddt[i,1]=func(absc)[i]
 }
 for(i in 2:l){
     ind=1
     for(j in i:l){
         ddt[j,i]=(ddt[j,i-1]-ddt[j-1,i-1])/(absc[j]-absc[ind])
         ind=ind+1
     }
 }
 return (diag(ddt))
}
#compute the interpolation at x=pt wrsp to absc and func
interp<-function(pt,absc,func){
 options(digits=15)
 l=length(absc)
 ddt<-matrix(0,l,l)
 for(i in 1:l){
     ddt[i,1]=func(absc)[i]
 }
 for(i in 2:l){
     ind=1
     for(j in i:l){
         ddt[j,i]=(ddt[j,i-1]-ddt[j-1,i-1])/(absc[j]-absc[ind])
         ind=ind+1
     }
 }
 pol=0
 for (i in 2:l) {
     ini=diag(ddt)[i]
    for(j in 1:(i-1) ){
        ini=ini*(pt-absc[j])
    }
    pol=pol+ini
 }
 return(pol+diag(ddt)[1])
}
#compute the max error
find.max.error<-function(vec,absc,func){
fm<-c()
for (i in 1:length(vec)) {
   fm[i]=abs(interp(vec[i],absc,func)-func(vec[i]))
}
 return(max(fm))
}
#the original function
 f<-function(x){
     exp(1+sin(3*x))
 }
#sol for quiz3A 1-1
ab<-c()
 for (i in 1:10) {
     ab[i]=cos((2*i-1)*pi/20)
 }
 interp(0.3,ab,f)
 #sol for quiz3A 1-2
sab1<-c()
 for (i in 1:5) {
     sab1[i]=cos((2*(i+3)-1)*pi/20)
 }
 interp(0.3,sab1,f)
#sol for quiz3A 1-3
 find.coe(ab,f)
 #sol for quiz3A 1-4
 sab2<-c()
 for (i in 1:5) {
    j=2*i-1
    sab2[i]=cos((2*j-1)*pi/20)
 }
  find.coe(sab2,f)
 #sol for quiz3A 1-4
comp<-c()
for (i in 0:100) {
   comp[i]=i/100-1
}
find.max.error(comp,ab,f)
