#define D1 and D2 as vectors with range 0-1
D1<-c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
D2<-c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)

#want g(f(D2)) and g(f(D1))
newN1<-c(0,0,0,0,0,0,0,0,0,0,0)
newN2<-c(0,0,0,0,0,0,0,0,0,0,0)

for (i in 1:length(N1)) {
  newN1[i]<-D2[i]^2/(0.1+D2[i]^2)
  newN2[i]<-D1[i]^2/(0.1+D1[i]^2)
}

#plot on one axis
plot(D1,newN2,xlab="N1",ylab="N2",main="N1 and N2",type="l")
lines(newN1,D2,col="blue")
legend(0.0,0.9,c("N1, N2=0-1","N2, N1=0-1"),fill=c("blue","black"))

#phase portrait
library(phaseR)
library(deSolve)

time<-seq(0,1000,by=100)
i<-c(d1=1,d2=1,n1=1,n2=1)

togglefunc<-function(time,state,parms){

  d1=state[1]
  d2=state[2]
  n1=state[3]
  n2=state[4]
  
  A<-2*d2*(0.1+d2^2)-2*d2^3
  B<-(0.1+d2^2)^2
  
  X<-2*d1*(0.1+d1^2)-2*d1^3
  Y<-(0.1+d1^2)^2
  
  n1dt=A/B
  n2dt=X/Y
  
  A<--20*d2^2/(0.1+d2^2)
  B<-1+10*(d2^2/(0.1+d2^2)^2)^2
  C<-2*d2*(0.1+d2^2)-2*d2^3/((0.1+d2^2)^2)
  
  X<--20*d1^2/(0.1+d1^2)
  Y<-1+10*(d1^2/(0.1+d1^2)^2)^2
  Z<-2*d1*(0.1+d1^2)-2*d1^3/((0.1+d1^2)^2)
  
  d1dt=A/B*C
  d2dt=X/Y*Z
  
  return(list(c(n1dt,n2dt,d1dt,d2dt)))
}

out<-ode(i,time,togglefunc)
plot(out)
#phasePortrait(togglefunc,ylim=c(0,10))
