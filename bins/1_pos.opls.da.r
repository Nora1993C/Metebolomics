library("ropls")
library(car)
library(RColorBrewer)
dataFile <- '../0.0_raw_data/pos.data.txt'
groupFile <- '../bin/pos-grouping.info'
outname <- 'pos.opls.pdf'

grpInfo <- read.table(groupFile,header=T);
Data <- read.table(dataFile,header=T,row.names = 1)
Data <- Data[,as.character(grpInfo[,1])]

groupname <- c();

for(i in 1:length(colnames(Data)))
{
    groupname <- append(groupname,as.character(grpInfo[grpInfo[,1] == colnames(Data)[i],2]));
}

Groups <- levels(as.factor(groupname));
#Groups <- Groups[c(2,1)]
Data.oplsda <- opls(t(Data) ,groupname, predI =1, orthoI = 2,plotL=FALSE, printL =FALSE);

####	Points
dd <- attributes(Data.oplsda)
nn <- attributes(dd)
dd.s <-dd$suppLs
nn.s <- attributes(dd.s)
lab <-dd$summaryDF
#p.info <- read.table(arg[5],header=T,row.names=1)
p.nn <- rownames(dd$scoreMN)
#ii <- intersect(p.nn,rownames(p.info))
#p.info <- p.info[ii,]
#pch <- as.numeric(p.info[,1])
x <- dd$scoreMN[,1]
#y <- dd$scoreMN[,2]
y <- dd$orthoScoreMN[,1]
col= c(rep(brewer.pal(8,"Set1")[2],length(grep(Groups[1],groupname))),rep(brewer.pal(8,"Set1")[1],length(grep(Groups[2],groupname))))
groupname

pdf(outname,w=4,h=4)
par(mgp=c(2,0.5,0),mar=c(6.5,4.5,3,3))
pp <-signif(dd$modelDF[,1],2)*100
plot(x=x,y=y,pch=rep(20,nrow(Data)),col=col,main=paste0(Groups[1]," vs ",Groups[2]),xlab=paste("t1(",pp[1],"%)",sep=""),ylab=paste("to1(",pp[2],"%)",sep=""),cex.lab=1.2,cex.main=1.5,
    ylim=c(min(y)*2,max(y)*2.1),
    xlim=c(min(x)*1.3,max(x)*1.1),xaxt='n',yaxt='n')
abline(h=0,lwd=1,lty=2)
abline(v=0,lwd=1,lty=2)
axis(1,at=seq(-40,40,20),tck=-0.02,cex.axis=1)
axis(2,at=seq(-60,60,20),tck=-0.02,las=2,cex.axis=1)

l1 <- c( "R2X","R2Y","Q2")
l2 <-as.numeric(lab[1:3])
at= seq(min(x),max(x),length.out=3)
mtext(side=1,text=l1,at=at,line=4,adj=0.5,cex=1,font=2)
mtext(side=1,text=l2,at=at,line=5,adj=0.5,cex=1,font=2)
dataEllipse(x[grep(Groups[1],groupname)], y[grep(Groups[1],groupname)],levels = c(0.9), add=TRUE, col = brewer.pal(8,"Set1")[2], lwd = 1,plot.points=FALSE,fill=TRUE,center.cex=0.2)
dataEllipse(x[grep(Groups[2],groupname)], y[grep(Groups[2],groupname)],levels = c(0.9), add=TRUE, col = brewer.pal(8,"Set1")[1], lwd = 1,plot.points=FALSE,fill=TRUE,center.cex=0.2)

legend('bottomright',legend=Groups,cex=1.2,fil=brewer.pal(8,"Set1")[c(2,1)],bty='n')
dev.off()