library(ggplot2)
library(reshape2)
library(digest)


reference=readLines("WarPeace.txt")
mat=freqanalysis(reference)

freqanalysis <-function(reference){
  reference=toupper(reference)
  mat=matrix(0,27,27)
  rownames(mat)=colnames(mat)=c(toupper(letters),"")
  lastletter=""
  for (ln in 1:length(reference)) {
    if (ln %% 1000 ==0) {cat("Line",ln,"\n")}
    for (pos in 1:nchar(reference[ln])) {
      curletter=substring(reference[ln],pos,pos)
      if (curletter %in% toupper(letters)) {
        mat[rownames(mat)==lastletter,
            colnames(mat)==curletter]=
          mat[rownames(mat)==lastletter,
              colnames(mat)==curletter]+1
        lastletter=curletter
      } else {
        if (lastletter!="") {
          mat[rownames(mat)==lastletter,27]=
            mat[rownames(mat)==lastletter,27]+1
          lastletter=""
        }
      }
    }
    curletter=""
    if (lastletter!="") {
      mat[rownames(mat)==lastletter,27]=
        mat[rownames(mat)==lastletter,27]+1
    }
    lastletter=""
  }
  return(mat)
}

prob.mat=sweep(mat+1,1,rowSums(mat+1),FUN="/")

pdf("freqanal.pdf",width=8,height=8)
ggplot(melt(prob.mat),aes(Var2,Var1))+geom_tile(aes(fill=value))+
  scale_fill_gradient(low="white",high="red",limits=c(0,1))+
  labs(x="Second letter",y="First letter",fill="Prob")+
  scale_y_discrete(limits = rev(levels(melt(prob.mat)$Var1)))+
  coord_equal()+
  ggtitle("Reference Text Letter Relative Frequencies")
dev.off()
