library(RColorBrewer)
library(tidyverse)
library(beeswarm)
error_bar <- function(x1,y1,x2,y2,length=length,...){
  arrows(x1,y1,x2,y2,angle=90,code=3,length=length,...)
}
list <- read.table('iden.list.txt',sep='\t',header = T)
mz <- read.table('../pos.mz.dat',header = T,row.names = 1)

tt <- mz[mz[,1] %in% list[,1],]
tt <- cbind(tt,name=NA)
for(i in 1:nrow(tt)){
  tt[i,'name'] <- as.character(list[list[,1] == tt[i,1],'name'])
  tt[i,'flag'] <- as.character(list[list[,1] == tt[i,1],'flag'])
}
list <- tt
list <- list[list$flag != '',]
data <- read.table('../0.0_raw_data/pos.data.txt',header=T,row.names=1)
data <- data[rownames(list),]
grpdata <- read.table('../bin/pos-grouping.info',header=T)
rownames(grpdata) <- grpdata[,1]
grpdata <- grpdata[rownames(grpdata) %in% colnames(data),]
Groups <- levels(as.factor(grpdata[,2]))
rownames(data) <- list[rownames(data),3]

stat <- as_tibble(cbind(name=colnames(data),t(data))) %>% 
          gather(metas,value,-name) %>% 
          left_join(as_tibble(grpdata),by=c('name'='name')) %>%select(-GRP2)
stat$metas <- as.factor(stat$metas)
stat$GRP1 <- as.factor(stat$GRP1)
stat$value <- as.numeric(as.matrix(stat$value))

ATs <- sort(c(1:nrow(data)+0.2,1:nrow(data)-0.2))
cols1 <- brewer.pal(8,"Set3")[5:4]

pdf("BM.dots.pdf",w=8,h=4)

par(mar=c(5,4,3,1),mgp=c(2.5,0.5,0))
#beeswarm(log10(value) ~ GRP1 + metas,data=stat,vertical = T,method='center',main='benign vs malignance',xlab='',ylab='log 10(Relative abundance)',at=ATs,pch=16,col=cols1,bty='n',yaxt='n',add=F,cex=0.4,labels=F,ylim=c(-3.9,3.2))
boxplot(log10(value) ~ GRP1 + metas,data=stat,notch=T,range = 1,outline=F,pars = list(boxwex = 0.35, staplewex = 0.3, outwex = 0.3),main='benign vs malignance',xlab='',ylab='log 10(Relative abundance)',at=ATs,col=cols1,bty='n',yaxt='n',add=F,cex=0.4,labels=F,ylim=c(-3.3,3),xaxt='n',yxat='n')

axis(side = 2,at=seq(-5,log10(max(stat$value))*1.2,1),las=2,tck=-0.02)

#segments(x0=ATs-0.15,y0=data.frame(aggregate(log10(value) ~ GRP1 + metas,data=stat,quantile))[,3][,3],x1=ATs+0.15,y1=data.frame(aggregate(log10(value) ~ GRP1 + metas,data=stat,quantile))[,3][,3],col = c('#2185c5','#ff3d2e'),lwd = 2)

#arrows(ATs,aggregate(log10(value) ~ GRP1 + metas,data=stat,quantile)[,3][,2],          ATs,aggregate(log10(value) ~ GRP1 + metas,data=stat,quantile)[,3][,4],col=c('#2185c5','#ff3d2e'),code = 3,angle = 90,length = 0.05,lwd=1.5)
#segments(x0=ATs-0.1,y0=data.frame(aggregate(log10(value) ~ GRP1 + metas,data=stat,quantile))[,3][,2],x1=ATs+0.1,y1=data.frame(aggregate(log10(value) ~ GRP1 + metas,data=stat,quantile))[,3][,2],col = cols1,lwd = 2)
#segments(x0=ATs-0.1,y0=data.frame(aggregate(log10(value) ~ GRP1 + metas,data=stat,quantile))[,3][,4],x1=ATs+0.1,y1=data.frame(aggregate(log10(value) ~ GRP1 + metas,data=stat,quantile))[,3][,4],col = cols1,lwd = 2)

Ylim <- 2.4
for(n in 1:length(levels(stat$metas))){
  tt <- stat[stat$metas == levels(stat$metas)[n],]
  p <- wilcox.test(value~GRP1,data=tt)$p.value
  error_bar(n-0.2,Ylim,n+0.2,Ylim,lwd=1,xpd=T,col='grey80',length = 0.02)
  text(x=c(n-0.4,n-0.3),y=c(Ylim+0.45,Ylim+0.5),labels = c('p',paste0(' = ',signif(p,2))),font = c(3,1),xpd=T,adj=0,cex=0.7)
}
text(labels = levels(stat$metas),x=1:nrow(data)+0.2,y=log10(min(stat$value))-0.5,adj=1,srt=30,xpd=T)
legend(x=4,y=Ylim+1.7,legend = Groups,fill=cols1,ncol=2,bty='n',xpd=T)

dev.off()

