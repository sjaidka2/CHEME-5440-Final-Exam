#parameters
kex<-22500
kel<-4200
tx<-(1.875*10^6)
tl<-350000
Rl<-2000
Rx<-2000
G<-4630
W1<-0.25
W2<-98.75
w<-1
mu<-1.5
u<-vector()
u<-seq(from=0.5,to=0.99,by=0.01)

t<-seq(from=0,to=1470,by=30)
degp<-exp(0.0005*t)
degm<-exp(0.13*t)
dil<-mu*t

mstarvec<-vector()
rlvec<-vector()
rxvec<-vector()
KLvec<-vector()
KXvec<-vector()
pstarvec<-vector()

mstarvec<-c(mstarvec,0)
rlvec<-c(rlvec,1)
rxvec<-c(rxvec,1)
KLvec<-c(KLvec,1)
KXvec<-c(KXvec,1)
pstarvec<-c(pstarvec,0)

for (i in 2:length(t)){
  rlvec[i]<-(kel*Rl)*(mstarvec[i-1]/tl*KLvec[i-1])
  rxvec[i]<-(kex*Rx)*(G/(tx*KXvec[i-1]+G*(tx+1)))
  KLvec[i]<-rlvec[i-1]/(dil[i]+degp[i])
  KXvec[i]<-rxvec[i-1]/(dil[i]+degm[i])
  pstarvec[i]<-KLvec[i-1]*w
  mstarvec[i]<-KXvec[i-1]*u[i]
}

plot(u,pstarvec,xlab="u",ylab="pstar",type="l")

#part c, Kp
Kp<-100
KLvecnew<-KLvec*Kp

for (i in 2:length(t)){
  rlvec[i]<-(kel*Rl)*(mstarvec[i-1]/tl*KLvecnew[i-1])
  rxvec[i]<-(kex*Rx)*(G/(tx*KXvec[i-1]+G*(tx+1)))
  KXvec[i]<-rxvec[i-1]/(dil[i]+degm[i])
  pstarvec[i]<-KLvecnew[i-1]*w
  mstarvec[i]<-KXvec[i-1]*u[i]
}

plot(u,pstarvec,xlab="u",ylab="pstar",main="pstar vs u when Kp=10",type="l")