library(MASS)
library(mcsm)
library(coda)

Nsim=10^4
alpha=0.3
candmu = c(0,0)
candsig = matrix(c(1,0,0,1),2,2)
mu1= c(1,1)
mu2=c(8,8)
sig1 = matrix(c(1,0,0,1),2,2)
sig2 = matrix(c(1,0,0,1),2,2)

f=function(Y){
  x=alpha*dmunorm(Y,mu1,sig1)+(1-alpha)*dmunorm(Y,mu2,sig2)
  return(x) 
}

q=function(Y,X){dmunorm(X,Y,candsig)}  
genprop=function(X){mvrnorm(1,X,candsig)}
dim=2
startpt=c(0,0)
T=1
X=matrix(startpt,1,dim)
A=c()
R=c()
  for (t in 2:Nsim){
    Y=genprop(X[t-1,])
    rho=((f(Y)^T)*q(Y,X[t-1,]))/((f(X[t-1,])^T)*q(X[t-1,],Y))
    if (runif(1)<rho){
      Z=Y
      A[t-1]=1
    } else {
      Z=X[t-1,]
      A[t-1]=0
    }
    X=rbind(X,matrix(Z,1,2))
    R[t-1]=min(1,rho)
  }  



pdf("NormMixChain.pdf",width=16,height=8)
par(mfrow=c(1,2))
plot(X,cex=0.5,pch=16,col="blue",xlab="X1",ylab="X2",main="Chain Scatter Plot",xlim=c(-3,12),ylim=c(-3,12),cex.main=2,cex.axis=1.8,cex.lab=1.5)
plot(density(X[,1]),"Chain Density",xlim=c(-3,12),cex.main=2,cex.axis=1.8,cex.lab=1.5)
dev.off()
