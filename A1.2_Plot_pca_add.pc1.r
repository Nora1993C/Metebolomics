arg <- commandArgs(T)
if(length(arg) != 4){
        cat("Argument: Data_File Group_File Out_Dir Group_column\n")
        quit('no')
}


dataFile <- arg[1];
groupFile <- arg[2];
outDir <- arg[3];
grp_col <- as.numeric(arg[4]);

#dataFile <- "../1.2_raw_Distance/liverCMQU.spearman.txt";
#groupFile <- "../0.0_raw_data/liverCMQU.pos-grouping.info";
#outDir <- "./";
#grp_col <- 2;

library(ade4);
library(fpc);
library(RColorBrewer)
library(ggplot2);
library(aplot)

COLS <- brewer.pal(8,"Set1")[c(2,1,3:8)]
grpInfo <- read.table(groupFile,header=T);

FileName <- strsplit(basename(dataFile),'.',fixed=T)[[1]];
SamID <- FileName[1];
MetID <- FileName[2];

Beta <- as.matrix(data.frame(read.table(dataFile,header=T,row.names = 1,sep='\t')));

Beta <- Beta[order(rownames(Beta),decreasing=F),];
Beta <- Beta[,order(colnames(Beta),decreasing=F)];

groupname <- c();
for(i in 1:length(rownames(Beta)))
{
	groupname <- append(groupname,as.character(grpInfo[grpInfo[,1] == rownames(Beta)[i],grp_col]));
}

rownames(Beta) <- groupname;
colnames(Beta) <- groupname;

Groups <- as.character(unique(grpInfo[,grp_col]))
Beta.Group <- c();
Symbol.Group <- c();
Color.Group <- c();

for(i in 1:length(Groups))
{
	Beta.Group[[i]] <- Beta[grep(Groups[i],rownames(Beta)),grep(Groups[i],colnames(Beta))];
	Symbol.Group <- append(Symbol.Group,as.vector(rep((17+i),nrow(Beta.Group[[i]]))));
  Color.Group <- append(Color.Group,as.vector(rep(COLS[i],nrow(Beta.Group[[i]]))));
}

pc <- princomp(Beta);

######## Plot 2D PCA
test <- data.frame(pc$loading[,1:2],group=groupname)
test$group <- factor(test$group, levels=Groups)

p1 <- ggplot(data= test,(aes(test[,1],test[,2],fill = group,colour = group)))+
  geom_point(size=2)+theme_bw()+
  guides(colour = guide_legend(override.aes = list(size=2)))+
  stat_ellipse(geom="polygon",alpha=0.2)+
  scale_colour_manual(values=COLS)+
  scale_fill_manual(values=COLS)+
  labs(x=paste("PC1(",sprintf("%.1f",pc$sde[1]^2/sum(pc$sde^2)*100),"%)",spe=""),y=paste("PC2(",sprintf("%.1f",pc$sde[2]^2/sum(pc$sde^2)*100),"%)",spe=""))+
  theme(legend.position="none")

p2 <- ggplot(test,(aes(y=test[,1],x=group,fill = group)))+
  geom_boxplot(aes(col = group))+theme_bw()+
  scale_colour_manual(values=COLS)+
  scale_fill_manual(values=COLS)+
  labs(x='',y='',title = 'PC1')+
  theme_bw() +
  theme(legend.position = "none")

p <- p1 %>% insert_right(p2, width = 0.4)
pdf(file = paste(outDir,"/",SamID,".",MetID,".pca.pdf",sep=""), width = 6,height = 4)
p
invisible(dev.off())


#pdf(,width=5,height=4);
#layout(mat=matrix(1:2,nrow = 1),widths = c(2,1))
#x <- pc$loadings[,1]
#y <- pc$loadings[,2]
#par(mar=c(3,4,3,0))
#plot(x,y,col = Color.Group,pch=Symbol.Group,xlab#=paste("PC1(",sprintf("%.1f",pc$sde[1]^2/sum(pc$sde^2)*100),"%)",spe=""),ylab=paste("PC2(",sprintf("%.1f",pc$sde[2]^2/sum(pc$sde^2)*100),"%)",spe=""),cex=0.9,xlim = c(min(x),max(x))*1.5,ylim=c(min(y),max(y))*1.5);


#for(g in 1:length(Groups)){
#  dataEllipse(x[grep(Groups[g],groupname)], y[grep(Groups[g],groupname)],levels = c(0.95), add=TRUE, col = COLS[g], lwd = 1,plot.points=FALSE,fill=TRUE,center.cex=0)

#}
#legend('topleft',legend = Groups,fill = COLS[1:length(Groups)],border = NA,bty = 'n')
#s.class(pc$loadings[,1:2], fac=as.factor(groupname),grid=F, addaxes=F,axesell =T,label=Groups,col=COLS,pch=Symbol.Group,add.plot=T);
#par(mar=c(3,2,3,1))
#boxplot(x~groupname,col=COLS[1:length(Groups)],ylab='',main='PC1',xlab='',bty='o')
#dev.off();
