library(MASS)
library(mcsm)
library(coda)

Msim=10^3
Nsim=10^4
candmu = c(1,1)
actmu = c(0,0)
actsig = matrix(c(1,0,0,1),2,2)
f=function(Y){dmunorm(Y,actmu,actsig)}
q=function(Y,X){dmunorm(X,Y,candsig)}  
genprop=function(X){mvrnorm(1,X,candsig)}
dim=2


pdf("MetropPlots.pdf",width=12,height=8)
par(mfrow=c(3,4))
for (s in c(0.001,1,1000)){
  E=c()
  a=c()
  r=c()
  candsig = matrix(c(s,0,0,s),2,2)
  for (u in 1:Msim){
    X=matrix(c(1,1),1,2)
    A=c()
    R=c()
    for (t in 2:Nsim){
      Y=genprop(X[t-1,])
      rho=f(Y)*q(Y,X[t-1,])/(f(X[t-1,])*q(X[t-1,],Y))
      if (runif(1)<rho){
        Z=Y
        A[t-1]=1
      } else {
        Z=X[t-1,]
        A[t-1]=0
      }
      X=rbind(X,matrix(Z,1,2))
      D=c(mean(X[,1]),mean(X[,2]))
      R[t-1]=min(1,rho)
    }  
    E=rbind(E,matrix(D,1,2))
    a[u]=mean(A)
    r[u]=mean(R)
  }
  plot(density(E[,1]),main=paste("Density of Chain Means, var=",s),xlim=c(-2,2))
  acf(X[,1],main=paste("Autocorrelation plot of X1, var=",s))
  plot(1:Msim,a,xlab="Chain Number", ylab="Acceptance Rate",main=paste("Acceptance Rate, var=",s),ylim=c(0,1),pch=16,cex=0.3)
  plot(1:Nsim,X[,1],xlab="Iterations",ylab="X1",main=paste("Chain of X1, var=",s),type="l")
}
dev.off()
