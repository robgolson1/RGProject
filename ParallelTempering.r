library(MASS)
library(mcsm)
library(coda)
library(mvtnorm)

alpha=0.3
mu1= c(1,1)
mu2=c(8,8)
sig1 = matrix(c(1,0,0,1),2,2)
sig2 = matrix(c(1,0,0,1),2,2)

f=function(Y){
  x=alpha*dmvnorm(Y,mu1,sig1)+(1-alpha)*dmvnorm(Y,mu2,sig2)
  return(x) 
}

q=function(Y,X){dmunorm(X,Y,matrix(c(1,0,0,1),2,2))}  
genprop=function(X){mvrnorm(1,X,matrix(c(1,0,0,1),2,2))}

Nsim=10000
m=100

startpoint=array(c(c(1,1),c(1,2),c(1,1)),c(1,2,3))			
beta=c(1,0.7,0.4,0.1)

dim=length(startpoint[,,1])
X=array(0,c(Nsim,dim,length(beta)))
for (i in 1:length(beta)){
  X[1,,i]=startpoint[,,i]
}
Y=array(0,c(1,dim,length(beta)))
rho=c()
A=c()
for (t in 2:Nsim){
  for (i in 1:length(beta)){
    Y[,,i]=genprop(X[t-1,,i])
    rho[i]=((f(Y[,,i])^beta[i])*q(Y[,,i],X[t-1,,i]))/((f(X[t-1,,i])^beta[i])*q(X[t-1,,i],Y[,,i]))
    if (runif(1)<rho[i]){
      X[t,,i]=Y[,,i]
    } else {
      X[t,,i]=X[t-1,,i]
    }
  }
  if (t%%m==0){
    u=runif(1,0,length(beta)-1)
    for (i in 1:(length(beta)-1)){
      if (i-1 < u & u < i){
        s=c(i,i+1)
      }
    }
    p=array(c(X[t,,s[1]],X[t,,s[2]]),c(1,dim,2))
    rho2=((f(p[,,1])^beta[s[2]])*(f(p[,,2])^beta[s[1]]))/((f(p[,,1])^beta[s[1]])*(f(p[,,2])^beta[s[2]]))
    if (runif(1)<rho2){
      X[t,,s[1]] = p[,,2]
      X[t,,s[2]] = p[,,1]
      append(A,1)
    }
  }
}  

alphahat=length(which(X[,,1]<4))/length(X[,,1])

pdf("partempa.pdf",height=8,width=16)
par(mfrow=c(1,1))
plot(1:Nsim,X[,1,1],type="l",xlab="Iterations",ylab="X1",main="X1 Component of Chain",cex.main=2,cex.axis=1.7,cex.lab=1.6)
dev.off()
pdf("partempb.pdf",height=8,width=16)
par(mfrow=c(1,2))
plot(X[,,1],cex=0.5,pch=16,col="blue",xlab="X1",ylab="X2",main="Chain Scatter Plot",cex.main=2,cex.axis=1.8,cex.lab=1.5)
plot(density(X[,1,1]),"Chain Density",cex.main=2,cex.axis=1.8,cex.lab=1.5)
dev.off()
