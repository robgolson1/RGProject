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



X=matrix(c(-3,-2),1,2)
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
plot(X[,1],X[,2],col="blue",pch=16,cex=0.5,xlab="X1",ylab="X2",main="Metropolis-Hastings Chain Movement",cex.main=3,cex.axis=1.5,cex.lab=1.5)
lines(X[1:100,1],X[1:100,2])

x<-seq(-2,2,length.out=100)
y<-seq(-2,2,length.out=100)
z=expand.grid(x,y)
bivn=matrix(0,100,100)
for (i in 1:10000){
  bivn[i] <- as.matrix(dmunorm(as.numeric(z[i,]),actmu,actsig))
}


pdf("MetropHastingsEgPoster.pdf",width=16,height=8)
par(mfrow=c(1,2))
image(bivn,main="Density and Contour Plot of Target Dist.",cex.main=2,cex.axis=2,cex.lab=2) 
contour(bivn, add = T,lwd=0.75)
plot(X[,1],X[,2],col="blue",pch=16,cex=0.5,xlab="X1",ylab="X2",main="Metropolis-Hastings Chain Movement",cex.main=2,cex.axis=1.8,cex.lab=1.5)
lines(X[1:100,1],X[1:100,2])
dev.off()
pdf("MetropHastingsEgPosterb.pdf",width=16,height=8)
par(mfrow=c(1,1))
plot(1:Nsim,X[,1],type="l",ylab="X1",xlab="Iterations",main="Metropolis-Hastings Chain",cex.main=3,cex.axis=1.6,cex.lab=1.6)
dev.off()
