library(MASS)
library(mcsm)
library(coda)

# Note this is the 2-D case - needs modifying
# for higher dimensional cases
Nsim=10^4
init=c()			#initial state

condx=function(x){} #generate from Y|X
condy=function(x){} #generate from X|Y
X=Y=array(0,dim=c(Nsim,1))
X[1]=init[1]
Y[1]=init[2]
Z=matrix(c(X[1],Y[1]),1,2)
for (i in 2:Nsim){
  Y[i]=condx(X[i-1])
  Z=rbind(Z,c(X[i-1],Y[i]))
  X[i]=condy(Y[i])
  Z=rbind(Z,c(X[i],Y[i]))
}
