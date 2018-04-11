N=5000
p=c(1,0.1,0.001,0.0001)
codemess <- decryptsub(message,sample(1:26))
starttime=Sys.time()
#R=matrix(0,50,27)
#for (r in 1:50){
E=decryptionRoutine(1000,codemess,mat,1)
oldFunc <- matrix(0,length(p),27)
for (i in 1:length(p)){
  oldFunc[i,] <- as.numeric(E[4:30])
}
curFunc=oldFunc
bestFunc=curFunc
oldScore = c()
oldScore[1:length(p)] = scorefn(decryptsub(codemess,curFunc[1,]),mat)
curScore = oldScore
bestScore <- oldScore
a=matrix(0,N,length(p))
scoreA=matrix(0,N+1,length(p))
scoreA[1,1:length(p)]=oldScore
scoreP=scoreA
t=matrix(0,2,27)
A=c()
for (i in 1:N) {
  for (j in 1:length(p)){
    oldFunc[j,] <- curFunc[j,]
    curFunc[j,] <- newFunc(curFunc[j,])
    curScore[j] <- scorefn(decryptsub(codemess,curFunc[j,]),mat)
    scoreP[i+1,j]<-curScore[j]
    if(curScore[j] > bestScore[j]){
      bestScore[j] <- curScore[j]
      bestFunc[j,] <- curFunc[j,]
    }
    if (runif(1) > exp(p[j]*(curScore[j]-oldScore[j])))
    {
      curFunc[j,] <- oldFunc[j,]
      a[i,j]=0
      scoreA[i+1,j]<-oldScore[j]
    } else 
    {
      oldScore[j] <- curScore[j]
      a[i,j]=1
      scoreA[i+1,j] <- curScore[j]
    }
    if ((i%%100) == 0){
      if (j==length(p)){
        print(c(i, decryptsub(codemess, curFunc[1,])))
        u=runif(1,0,length(p)-1)
        for (i in 1:(length(p)-1)){
          if (i-1 < u & u < i){
            s=c(i,i+1)
          }
        }
        t[1,]=curFunc[s[1],]
        t[2,]=curFunc[s[2],]
        if (runif(1)<exp((curScore[s[2]]-curScore[s[1]])*p[s[1]]+(curScore[s[1]]-curScore[s[2]])*p[s[2]])){
          curFunc[s[1],]=t[2,]
          curFunc[s[2],]=t[1,]
          A=append(A,1)
        }
      }
    }
  }
}
#R[r,]=bestFunc[1,]
#}
endtime=Sys.time()
endtime-starttime


X=E[31:1031] #score = accepted
Y=E[1032:2032] #score2 = proposed
scoreP[,1] #proposed
scoreA[,1] #accepeted

A=c(X,scoreA[,1])
P=c(Y,scoreP[,1])

pdf("subtemp.pdf",width=8,height=8)
plot(1:length(P),P,cex=0.5,pch=16,col="blue",xlab="Iteration",ylab="Score",main="Score Plot",ylim=c(3500,10000),cex.main=2,cex.lab=1.5,cex.axis=1.7)
lines(1:length(A),A,lwd=2)
points(c(1000,2000,3000,4000,5000,6000),c(A[1000],A[2000],A[3000],A[4000],A[5000],A[6000]),col="red",pch=17,cex=2)
dev.off()


#Remove #'s above to acquire the code to run the algorithm 50 times consecutively. 

## Running the Algorithm ##

#1. This assumes the running of all previous code for Algorithm 4.
#2. Open the ciphertext.r file and run 
y = "..."
#to get message to encrypt and decrypt.
#3. Highlight entire code and Run.
#4. Watch decryptions
