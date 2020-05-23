#parameters
ke<-10^-4
kes<-5^10-3
kf<-5.14*10^-21
kr<-2.5*10^-2
kdeg<-8*10^-4
vs<-18
q<-10^3
nc<-3*10^8
shear<-100
Dl<-10^-10
Sh<-(shear*z^2/Dl)^(1/3)
max<-((1/kes)+(1/kdeg))*vs
z<-seq(from=0,to=10^-4,by=10^-10)
kmvec<-c(0,0,0,0,0,0,0,0,0,0,0)
Rtotvec<-c(0,0,0,0,0,0,0,0,0,0,0)
Lvec<-c(0,0,0,0,0,0,0,0,0,0,0)

#ligand concentration
for (i in 1:length(z)){
  kmvec[i]<-(shear*z[i]^2/Dl)^(1/3)*(Dl/z[i])
  Lvec[i]<-(1/kmvec[i])*(-kf*Lvec[i]*Rtotvec[i]*nc+kr*Rtotvec[i]*nc-q*nc)
  Rtotvec[i]<-(1/kes+1/kdeg)*vs*Lvec[i]
}

plot(Rtotvec,Lvec,xlab="R total",ylab="Ligand over z")