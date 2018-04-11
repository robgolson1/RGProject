N=3000
n=10
codedmessage <- decrypttransp(y,10,sample(1:10))
p=c(1,0.1,0.01,0.001)
starttime=Sys.time()
#R=matrix(0,50,n)
#for (r in 1:50){
  D=transpdecryptRoutine(1000,n,codedmessage,1)
  oldkey <- matrix(0,length(p),n)
  for (i in 1:length(p)){
    oldkey[i,] <- as.numeric(D[3:(2+n)])
  }
  curkey=oldkey
  bestkey=oldkey
  oldScore=c()
  oldScore[1:length(p)] <- scorefn(decrypttransp(codedmessage,n,oldkey[1,]),mat)
  curScore=oldScore
  bestScore <- oldScore
  a=matrix(0,N,length(p))
  scoreA=matrix(0,N+1,length(p))
  scoreA[1,1:length(p)]=oldScore
  scoreP=scoreA
  t=matrix(0,2,n)
  A=c()
for (i in 1:N){
  for (j in 1:length(p)){
    oldkey[j,] <- curkey[j,]
    curkey[j,] <- newkey(oldkey[j,],n)
    curScore[j] <- scorefn(decrypttransp(codedmessage,n,curkey[j,]),mat)
    scoreP[i+1]<-curScore[j]
    if(curScore[j] > bestScore[j]){
      bestScore[j] <- curScore[j]
      bestkey[j,] <- curkey[j,]
    }
    if (runif(1)>exp(p[j]*(curScore[j] - oldScore[j]))){
      curkey[j,] <- oldkey[j,]
      a[i,j]=0
      scoreA[i+1,j]<-oldScore[j]
    }
    else {
      oldScore[j] <- curScore[j]
      a[i,j]=1
      scoreA[i+1,j]<-curScore[j]
    }
    if ((i%%100) == 0){
      if (j==length(p)){
        print(c(i, decrypttransp(codedmessage,n,curkey[1,])))
        u=runif(1,0,length(p)-1)
        for (i in 1:(length(p)-1)){
          if (i-1 < u & u < i){
            s=c(i,i+1)
        }
        }
        t[1,]=curkey[s[1],]
        t[2,]=curkey[s[2],]
        if (runif(1)<exp((curScore[s[2]]-curScore[s[1]])*p[s[1]]+(curScore[s[1]]-curScore[s[2]])*p[s[2]])){
          curkey[s[1],]=t[2,]
          curkey[s[2],]=t[1,]
          A=append(A,1)
        }
      }
      
    }
  }
}
#R[r,]=bestkey[1,]
#}
endtime=Sys.time()
endtime-starttime

X=D[13:1013] #score = accepted
Y=D[1014:2014] #score2 = proposed
A=c(X,scoreA[,1])
P=c(Y,scoreP[,1])

pdf("transptemp.pdf",width=8,height=8)
plot(1:length(P),P,cex=0.5,pch=16,col="blue",xlab="Iteration",ylab="Score",main="Score Plot",ylim=c(3500,10000),cex.main=2,cex.lab=1.5,cex.axis=1.7)
lines(1:length(A),A,lwd=2)
points(c(1000,2000,3000,4000,5000,6000),c(A[1000],A[2000],A[3000],A[4000],A[5000],A[6000]),col="red",pch=17,cex=2)
dev.off()


#Remove #'s above to acquire the code to run the algorithm 50 times consecutively. 


## Running the Algorithm ##

#1. This assumes the running of all previous code for Algorithm 4.
#2. Open the ciphertext.r file and run 
y = ...
#to get message to encrypt and decrypt.
#3. Highlight entire code and Run.
#4. Watch decryptions



