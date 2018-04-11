N <- function(x,beta){
  g=0.5*dnorm(x,0,1/beta)+0.5*dnorm(x,16,1/beta)
  return(g)
}

x=seq(-10,26,length.out=10000)



pdf("tempnorm.pdf",height=8,width=16)
par(mfrow=c(1,1))
plot(x, N(x,1),type="l",ylab="Density",main="Mixture Normal Density Plots",cex.main=2,cex.lab=1.7,cex.axis=1.7)
lines(x, N(x,0.5),col=2)
lines(x, N(x,0.2),col='blue')
lines(x, N(x,0.1),col=3)
legend(-10,0.2,legend=c("1", "0.5","0.2","0.1"),col=c("black", "red","blue","green"), lty=1, cex=1.5,title=expression(beta))
dev.off()
