library(MASS)
library(mcsm)
library(coda)

# simulation using Bivariate Normal Proposal and 
# Bivariate Normal target, with different mu and Sigma

Nsim=10^3			#no. of sims
candmu = c(6,6)			#proposal mean
actmu = c(1,1)			#target mean
candsig = matrix(c(0.4,0,0,0.4),2,2)	#proposal sig
actsig = matrix(c(1,0,0,1),2,2)	#target sig
f=function(Y){dmunorm(Y,actmu,actsig)}	#proposal dist. 
q=function(Y,X){dmunorm(X,Y,candsig)}  	#target dist.
genprop=function(X){mvrnorm(1,X,candsig)} #generate from proposal


dim=2			#no. of dimensions
startpoint=c(6,30)		#initial state

X=matrix(startpoint,1,dim)		#initial state in matrix form
A=c()							#making note of acceptance rate
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
  X=rbind(X,matrix(G,1,2)) 		#adding next state to matrix
}  

pdf("burn-in.pdf",height=10,width=24)
par(mfrow=c(1,1))
plot(1:Nsim,X[,1],type="l",xlab="Iterations",ylab="x",cex.lab=2,cex.axis=2)
dev.off()
