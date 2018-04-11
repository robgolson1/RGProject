library(MASS)
library(mcsm)
library(coda)

Nsim=					#no. of iterations
f=function(Y){}			#target
q=function(Y,X){}  		#proposal	
genprop=function(X){}	#generate from proposal
dim=					#dimension
startpoint=c()			#initial state

X=matrix(startpoint,1,dim)
A=c()					#take note of acceptance
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
  X=rbind(X,matrix(G,1,dim))
}  
