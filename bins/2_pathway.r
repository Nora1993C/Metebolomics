save.mat <- read.table('pathway.txt',header=T,sep='\t')

pdf("BM_pathway.pdf",width=5,height=5);
par(mar=c(4,5,3,4))
logp <- as.numeric(save.mat[,4])
path.impact <- as.numeric(save.mat[,'Impact'])
cPal <- colorRampPalette(c("pink","Firebrick"))
mycolors <- paste0(cPal(length(logp))[as.numeric(cut(logp,breaks = length(logp)))],'')
cexx <- logp*3
cexx[path.impact == 0] <- 1
par(mgp=c(2,0.5,0),mar=c(5,5,4.5,4),cex.lab=1.2,cex.axis=1,lwd=1,font=1,adj=0.5,xaxs = "i", yaxs = "i")
plot(0,0,xlab="Pathway Impact",ylab="-log(p)",axes=F,main='BM',cex.lab=1.2,cex.main=1.5,xlim=c(0,max(path.impact)*1.1),ylim=c(min(logp)*0.9,max(logp)*1.2),col="white")
#plot(0,0,xlab="Pathway Impact",ylab="-log(p)",axes=F,main=name,cex.lab=1.2,cex.main=1.5,xlim=c(0,max(path.impact)*1.1),ylim=c(min(logp)*0.9,4.5),col="white")
#plot(0,0,xlab="Pathway Impact",ylab="-log(p)",axes=F,main=name,cex.lab=1.2,cex.main=1.5,xlim=c(0,max(path.impact)*1.1),ylim=c(min(logp)*0.9,4),col="white")
axis(side=1,at=signif(seq(0,max(path.impact),0.1),2),tck=-0.02)
axis(side=2,at=signif(seq(0,8,0.2),2),las=2,tck=-0.02)

points(path.impact,logp,col=mycolors,pch=16,xpd=T,cex=(cexx))
points(path.impact,logp,col='black',pch=1,lwd=0.00001,xpd=T,cex=(cexx))
text(path.impact[path.impact != 0],logp[path.impact != 0],labels=save.mat[path.impact != 0,1],xpd=T,cex=0.8,font=2)
abline(v=signif(seq(0,max(path.impact),0.1),2),lty=2,col='blue')
abline(h=signif(seq(0,8,0.2),2),lty=2,col='blue')
dev.off()