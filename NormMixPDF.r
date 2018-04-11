library(MASS)
library(mcsm)
library(coda)
library(mvtnorm)

alpha=0.3
mu1= c(1,1)
mu2=c(8,8)
sig1 = matrix(c(1,0,0,1),2,2)
sig2 = matrix(c(1,0,0,1),2,2)
f=function(Y){alpha*dmvnorm(Y,mu1,sig1)+(1-alpha)*dmvnorm(Y,mu2,sig2)}

y=matrix(0,10000,2)
for (i in 1:10000){
  if (runif(1)<alpha){
    y[i,]=mvrnorm(1,mu1,sig1)
  }
  else {
    y[i,]=mvrnorm(1,mu2,sig2)
  }
}


alpha=0.3
mu1=1
mu2=8
sig1=1
sig2=1
z=c()
for (i in 1:10000){
  if (runif(1)<alpha){
    z[i]=rnorm(1,mu1,sig1)
  }
  else {
    z[i]=rnorm(1,mu2,sig2)
  }
}


f=function(Y,p){
  x=(alpha*dnorm(Y,mu1,sig1)+(1-alpha)*dnorm(Y,mu2,sig2))^p
  return(x) 
}
plot(density(z))

pdf("NormMix.pdf",width=16,height=8)
par(mfrow=c(1,2))
plot(y,cex=0.4,pch=16,col="blue",xlab="X1",ylab="X2",main="Normal Mixture Distribution",cex.main=2,cex.axis=1.8,cex.lab=1.5)
plot(density(y[,1]),"Normal Mixture Density",cex.main=2,cex.axis=1.8,cex.lab=1.5)
dev.off()
