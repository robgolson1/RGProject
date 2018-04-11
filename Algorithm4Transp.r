
scorefn <- function(code, mat)
{  
  logmat = log(mat + 1)
  codemat = freqanalysis(code)
  p <- 0
  for (i in 1:(nchar(code)-1)){
    p <- p + logmat[charIndex(substr(code, i, i)),charIndex(substr(code, i+1, i+1))]*codemat[charIndex(substr(code, i, i)),charIndex(substr(code, i+1, i+1))]
  }
  return(p)
}

sep <- function(z){
  y=c()
  for (i in 1:nchar(z)){
    y[i]=substring(z,i,i)
  }
  return(y)
}

randomlett <- function(n){
  a=c(" ")
  alphabet=c(letters," ")
  if(n==1){
    return(a)
  }
  else {
    for (j in 2:n){
      x=runif(1)
      for (i in 1:27){
        if ((i-1)/27 < x & x < (i/27)){
          a[j]=alphabet[i]
        }
      }
    }
    return(a)
  }
}

pad <- function(z,n){
  y=sep(z)
  if (length(y)%%n==0){
    y=y
  }          
  else {
    l=length(y)
    y=append(y,randomlett(n-(l%%n)))
  }
  x=paste(y,collapse="")
  return(x)
}



decrypttransp <- function(x,n,key){
  z=substring(pad(x,n), seq(1,nchar(pad(x,n)),n), seq(n,nchar(pad(x,n)),n))
  q=matrix(0,n,length(z))
  for (j in 1:length(z)){
    for (i in 1:n){
      q[i,j]=sep(z[j])[key[i]]
    }
  }
  return(paste(q,collapse=""))
}


curFunc <- 1:n
a=floor(runif(1,0,n))
b=c(a,floor(runif(1,0,n-a+1)),floor(runif(1,0,n-a+1)))

newkey <- function(key,n){
  k=ceiling(runif(1,0,n-2))
  b=floor(c(runif(1,0,n-k+1),runif(1,0,n-k+1)))
  a=((b[1]+1):(b[1]+k))
  x=key[a]
  z=append(key[-a],x,b[2])
  if(mean(z==key)==1){
    newkey(z,n)
  }
  else{
    return(z)
  }
}


transpdecryptRoutine <- function(N,n,message,p){
  curkey <- 1:n
  bestkey <- curkey
  oldScore <- scorefn(decrypttransp(message,n,curkey),mat)
  bestScore <- oldScore
  a=c()
  score=c(oldScore)
  score2=c(oldScore)
  for (i in 1:N){
    oldkey <- curkey
    curkey <- newkey(oldkey,n)
    curScore <- scorefn(decrypttransp(message,n,curkey),mat)
    score2[i+1]<-curScore
    if(curScore > bestScore){
      bestScore <- curScore
      bestkey <- curkey
    }
    if (runif(1)>exp(p*(curScore - oldScore))){
      curkey <- oldkey
      a[i]=0
      score[i+1]<-oldScore
    }
    else {
      oldScore <- curScore
      a[i]=1
      score[i+1]<-curScore
    }
    if ((i%%10000) == 0){
      print(c(i, decrypttransp(message,n,curkey)))
    }
  }
  plot(1:(N+1),score2,cex=0.3,pch=16,col="blue",xlab="Iteration",ylab="Score",main="Score Plot")
  lines(1:(N+1),score,lwd=2)
  points(c(100,1000,2000,3000,4000,5000),c(score[100],score[1000],score[2000],score[3000],score[4000],score[5000]),col="red",pch=17)
  return(c(decrypttransp(message,n,bestkey),mean(a),bestkey, score, score2))
}


## Running the algorithm ##

#1. Run all code above
#2. Run the following
mat=freqanalysis(reference)
message <- "the answer is seventy two to be or not to be that is the question whether tis nobler in the mind to suffer the slings and arrows of outrageous fortune or to take arms against a sea of troubles and by opposing end them"
codedmessage <- decrypttransp(message,10,sample(1:10))
transpdecryptRoutine(5000,10,codedmessage,1)
#3. Watch the decryptions
