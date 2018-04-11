library(MASS)
library(mcsm)
library(coda)


alpha=0.3
mu1= c(1,1)
mu2=c(8,8)
sig1 = matrix(c(1,0,0,1),2,2)
sig2 = matrix(c(1,0,0,1),2,2)

f=function(Y){
  x=alpha*dmunorm(Y,mu1,sig1)+(1-alpha)*dmunorm(Y,mu2,sig2)
  return(x) 
}

q=function(Y,X){dmunorm(X,Y,matrix(c(1,0,0,1),2,2))}  
genprop=function(X){mvrnorm(1,X,matrix(c(1,0,0,1),2,2))}

Nsim=20000
m=200

startpoint <- c(1,1)
dim=length(startpoint)
beta=c(1,0.7,0.5,0.3)
k=1/((alpha^beta)*pnorm(2.5*sqrt(beta/2))+((1-alpha)^beta)*pnorm(11.5*sqrt(beta/2))) #normalising constants

X=matrix(0,Nsim,2)
X[1,]=startpoint
A=c()
R=c(1)
i=1
for (t in 2:Nsim){
  Y=genprop(X[t-1,])
  rho=((f(Y)^beta[i])*q(Y,X[t-1,]))/((f(X[t-1,])^beta[i])*q(X[t-1,],Y))
  if (runif(1)<rho){
    X[t,]=Y
    A[t-1]=1
  } else {
    X[t,]=X[t-1,]
    A[t-1]=0
  }
  R[t]=i
  if (t%%m==0){
    if (runif(1) < 0.5){
      if (i==1){
        x<-i
      }
      else{
        x<-i-1
      }
    }
    else{
      if (i==length(beta)){
        x <- i
      }
      else{
        x<-i+1
      }
    }
    rho2=(k[x]*(f(X[t,])^beta[x]))/(k[i]*(f(X[t,])^beta[i]))
    if (runif(1)<rho2){
      i <- x
      }
    }
}

##########################################
#Above = Simulated Tempering Algorithm
#Below = How I plotted Figure 3.4 
##########################################

index<-function(colour,X,t){
  for (i in 1:length(colour)){
    if (X[t,3]==i){
      return(colour[i])
    }
  }
}

X=cbind(X,R)

pdf("simtempd.pdf",height=8,width=16)
par(mfrow=c(1,1))
plot(1:Nsim,X[,1],type="l",xlab="Iterations",ylab="X1",main="X1 Component of Chain",cex.main=2,cex.axis=1.7,cex.lab=1.6)
for (i in 1:Nsim){
  points(i,X[i,1],col=index(c("blue","slateblue1","palevioletred1","red"),X,i),pch=16,cex=0.35)
}
legend(-650,15,legend=c("1", "0.7","0.5","0.3"),col=c("blue","slateblue1","palevioletred1","red"), lty=3, lwd=16, cex=1.6,title=expression(beta))
length(which(X[which(R==1),1]<4))/length(X[which(R==1),1])
dev.off()
pdf("simtempb.pdf",height=8,width=16)
par(mfrow=c(1,2))
plot(X[which(R==1),],cex=0.5,pch=16,col="blue",xlab="X1",ylab="X2",main="Chain Scatter Plot",cex.main=2,cex.axis=1.8,cex.lab=1.5)
plot(density(X[which(R==1),1]),"Chain Density",cex.main=2,cex.axis=1.8,cex.lab=1.5)
dev.off()
