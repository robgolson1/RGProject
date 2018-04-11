X=seq(-pi/2,pi/2,length.out=1000)
X=append(X,-pi/2,0)
X=append(X,pi/2,length(X))

f=(1/2)*cos(X)

h=seq(1/2,1/2,length.out=1000)
h=append(h,0,0)
h=append(h,0,length(h))

pdf("AcceptReject.pdf",width=16,height=8)
par(mfrow=c(1,1))
plot(X,f,type="l",main="Ch(x) and f(x) plots for Accept-Reject Simulation",xlab="x",ylab="y",cex.main=2,cex.axis=2,cex.lab=2)
lines(X,h,type="l",col="red",lwd=2)
polygon(X,h,col="red",density=3,angle=135)
polygon(X,f,col="white")
polygon(X,f,col="green",density=3,angle=45)
lines(X,f,type="l",lwd=2)
legend(-1.5,0.48,legend=c("Ch(x)", "f(x)"),col=c("red", "black"), lty=1, cex=2,bg="white",lwd=2)
text(0,0.2,"Accept",col=" dark green",cex=2)
text(1.25,0.4,"Reject",col=" dark red",cex=2)
dev.off()