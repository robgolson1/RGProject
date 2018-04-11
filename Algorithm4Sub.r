
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

ascii <- function(char)
{ 
  strtoi(charToRaw(char),16L)
}

charIndex <- function(char)
{
  aValue <- ascii(char)
  if (aValue == 32)
  {
    27
  } else
  {
    aValue - 96 
  }
}

newFunc <- function(key){
  swaps <- sample(1:26,2,replace=T)
  oldFunc <- key
  key[swaps[1]]<-oldFunc[swaps[2]]
  key[swaps[2]]<-oldFunc[swaps[1]]
  return(key)
}

decryptsub <- function(code,curFunc)
{  	
  out <- code
  for (i in 1:nchar(code))
  {
    charInd <- charIndex(substr(code,i,i))
    if (charInd < 27)
    {
      substr(out,i,i) <- rawToChar(as.raw(curFunc[charInd] + 96))
    }
  }
  out 
}

decryptionRoutine <- function(N, codemess, mat,p) {
  curFunc <- 1:27
  bestFunc <- curFunc
  oldScore <- scorefn(decryptsub(codemess,curFunc),mat)
  bestScore <- oldScore
  a=c()
  score=c(oldScore)
  score2=c(oldScore)
  for (iteration in 1:N) {
    oldFunc <- curFunc
    curFunc <- newFunc(curFunc)

    newScore <- scorefn(decryptsub(codemess,curFunc),mat)
    score2[iteration+1]<-newScore
    
    if (newScore > bestScore){
      bestScore <- newScore
      bestFunc <- curFunc
    }
    if (runif(1) > exp(p*(newScore-oldScore)))
    {
      curFunc <- oldFunc
      a[iteration]=0
      score[iteration+1] <- oldScore
    } else 
    {
      oldScore <- newScore
      a[iteration]=1
      score[iteration+1] <- newScore
    }
    if ((iteration %%  100) == 0)
    {
      print(c(iteration,decryptsub(codemess,curFunc)))
    }
  }
  par(mfrow=c(1,1))
  plot(1:(N+1),score2,cex=0.5,pch=16,col="blue",xlab="Iteration",ylab="Score",main="Score Plot",cex.main=2,cex.lab=1.5,cex.axis=1.7)
  lines(1:(N+1),score,lwd=2)
  points(c(100,1000,2000,3000,4000,5000),c(score[100],score[1000],score[2000],score[3000],score[4000],score[5000]),col="red",pch=17,cex=2)
  return(c(iteration,decryptsub(codemess,curFunc),mean(a),bestFunc,score,score2))
}

## Instructions to run algorithm ## 

#1. Run all above code
#2. Run the following to get an encrypted text:
message <- "the answer is seventy two to be or not to be that is the question whether tis nobler in the mind to suffer the slings and arrows of outrageous fortune or to take arms against a sea of troubles and by opposing end them"
codemess <- decryptsub(message,sample(1:26))
#3. Run the following to get the look-up matrix (presumes running of RefMatrix.r)
mat <- freqanalysis(reference) 
#4. Run the following
decryptionRoutine(5000, codemess, mat,1)
#5. Watch the decryptions


