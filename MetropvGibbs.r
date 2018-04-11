library(MASS)
library(mcsm)
library(coda)


Nsim=10^4
r=0.9

#MetropolisHastings
candmu = c(1,1)
actmu = c(0,0)
candsig = matrix(c(1,0.6,0.6,1),2,2)
actsig = matrix(c(1,r,r,1),2,2)
f=function(Y){dmunorm(Y,actmu,actsig)}
q=function(Y,X){dmunorm(X,Y,candsig)}  
genprop=function(X){mvrnorm(1,X,candsig)}
dim=2

c=mvrnorm(Nsim,actmu,actsig)

X=matrix(c(1,1),1,2)
A=c()
R=c()
for (t in 2:Nsim){
  Y=genprop(X[t-1,])
  rho=f(Y)*q(Y,X[t-1,])/(f(X[t-1,])*q(X[t-1,],Y))
  if (runif(1)<rho){
    G=Y
    A[t-1]=1
  } else {
    G=X[t-1,]
    A[t-1]=0
  }
  X=rbind(X,matrix(G,1,2))
  D=c(mean(X[,1]),mean(X[,2]))
  R[t-1]=min(1,rho)
} 

#Gibbs
condf=function(y){rnorm(1,r*y,1-r^2)}
F=H=array(0,dim=c(Nsim,1))
F[1]=0.5
H[1]=0.5
Z=matrix(c(F[1],H[1]),1,2)
for (i in 2:Nsim){
  H[i]=condf(F[i-1])
  Z=rbind(Z,c(F[i-1],H[i]))
  F[i]=condf(H[i])
  Z=rbind(Z,c(F[i],H[i]))
}

#Plots
pdf("GibbsvsMetrop1.pdf",width=16,height=8)
par(mfrow=c(1,2))
plot(c,col="blue",pch=16,cex=0.3,xlab="X1",ylab="X2",main="Gibbs Chain",cex.main=2,cex.axis=1.8,cex.lab=1.5)
lines(Z[1:100,1],Z[1:100,2])
plot(c,col="blue",pch=16,cex=0.3,xlab="X1",ylab="X2",main="Metropolis-Hastings Chain",cex.main=2,cex.axis=1.8,cex.lab=1.5)
lines(X[1:100,1],X[1:100,2])
dev.off()
pdf("GibbsvsMetrop2.pdf",width=12,height=8)
par(mfrow=c(2,2))
plot(1:Nsim,F,type="l",xlab="Iterations",ylab="X",main="Gibbs Chain",cex.main=2,cex.axis=1.8,cex.lab=1.5)
acf(F,main="Autocorrelation of Gibbs Chain",cex.main=2,cex.axis=1.8,cex.lab=1.5)
plot(1:Nsim,X[,1],type="l",xlab="Iterations",ylab="X",main="Metropolis-Hastings Chain",cex.main=2,cex.axis=1.8,cex.lab=1.5)
acf(X[,1],main="Autocorrelation of Metropolis-Hastings Chain",cex.main=2,cex.axis=1.8,cex.lab=1.5)
dev.off()
