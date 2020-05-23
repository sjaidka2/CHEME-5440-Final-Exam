#define D1 and D2 as vectors with range 0-1
D1<-c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
D2<-c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)

#want g(f(D2)) and g(f(D1))
newD1<-c(0,0,0,0,0,0,0,0,0,0,0)
newD2<-c(0,0,0,0,0,0,0,0,0,0,0)

for (i in 1:length(D1)) {
  newD1[i]<-1/((1+10*((D2[i]^2)/(0.1+(D2[i]^2))^2)))
  newD2[i]<-1/((1+10*((D1[i]^2)/(0.1+(D1[i]^2))^2)))
}

#plot on one axis
plot(D1,newD2,xlab="D1",ylab="D2",main="D1 and D2",type="l")
lines(newD1,D2,col="blue")
legend("topright",c("D1 for D2 0-1","D2 for D1 0-1"),fill=c("blue","black"))

#phase portrait
library(phaseR)
library(deSolve)

time<-seq(0,10,by=1)
i<-c(d1=1,d2=1)


togglefunc<-function(time,state,parms){
  d1=state[1]
  d2=state[2]
  
  A<--20*d2^2/(0.1+d2^2)
  B<-1+10*(d2^2/(0.1+d2^2)^2)^2
  C<-2*d2*(0.1+d2^2)-2*d2^3/((0.1+d2^2)^2)
  
  X<--20*d1^2/(0.1+d1^2)
  Y<-1+10*(d1^2/(0.1+d1^2)^2)^2
  Z<-2*d1*(0.1+d1^2)-2*d1^3/((0.1+d1^2)^2)
  
  d1dt=A/B*C
  d2dt=X/Y*Z
  
  return(list(c(d1dt,d2dt)))
}

out<-ode(i,time,togglefunc)
phasePortrait(out,ylim=c(-1,1))
plot(out)