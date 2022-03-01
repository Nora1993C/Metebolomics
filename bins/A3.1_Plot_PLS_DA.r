library(mixOmics);
library(ade4);
library(vegan);


arg <- commandArgs(T)
if(length(arg) != 4){
        cat("Argument: Data_File Group_File Out_Dir Group_column\n")
        quit('no')
}

dataFile <- arg[1];
groupFile <- arg[2];
outDir <- arg[3];
grp_col <- as.numeric(arg[4]);

#dataFile <- "total.data.txt";
#groupFile <- "total-grouping.info";
#outDir <- "./";
#grp_col <- 2;


grpInfo <- read.table(groupFile,header=T);
FileName <- strsplit(basename(dataFile),'.',fixed=T)[[1]];
SamID <- FileName[1];
Data <- read.table(dataFile,header=T,row.names = 1);
#colnames(Data) <- gsub("X","",colnames(Data))

Data.tm <- t(as.matrix(Data)); 
groupname <- c();

for(i in 1:length(colnames(Data)))
{
	groupname <- append(groupname,as.character(grpInfo[grpInfo[,1] == colnames(Data)[i],grp_col]));
}
Data.tm[Data.tm ==1] <- 0
Data.plsda <- plsda(Data.tm ,groupname,ncomp=2)

pdf(paste(outDir,"/",SamID,".pls-da.pdf",sep=""),width=8,height=6);

plotIndiv(Data.plsda,ind.names = F, ellipse = T, centroid= F, title= "PLS-DA Plot", legend =T,style="ggplot2")


dev.off()
