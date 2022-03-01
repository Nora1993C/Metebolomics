library("ropls")

arg <- commandArgs(T)
if(length(arg) != 4){
        cat("Argument: Data_File Group_File Out_Dir Group_column\n")
        quit('no')
}

dataFile <- arg[1];
groupFile <- arg[2];
outDir <- arg[3];
grp_col <- as.numeric(arg[4]);

#dataFile <- "FT.data.txt";
#groupFile <- "FT-grouping.info";
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

Groups <- levels(as.factor(groupname));

if(length(Groups) == 2){
	Data.oplsda <- try(opls(Data.tm ,groupname, predI =1, orthoI = NA,plotL = FALSE, printL =FALSE),silent=TRUE);
	if ('try-error' %in% class(Data.oplsda))
	{
		Data.oplsda <- opls(Data.tm ,groupname, predI =1, orthoI = 2,plotL = FALSE, printL =FALSE);
	} 
	plot(Data.oplsda,typeVc="summary")
	plot(Data.oplsda,typeVc="x-score",parLabVc=groupname)
	VIP <- getVipVn(Data.oplsda);
	VIP <- VIP[VIP>1];
	write.table(VIP,file=paste(outDir,"/",SamID,".OPLSDA.VIP.txt",sep=""),quote = F,sep="\t",col.names=NA);
}else if(length(Groups) >2){
	Data.oplsda <- opls(Data.tm ,groupname,predI =1, plotL = FALSE, printL =FALSE);
        plot(Data.oplsda,typeVc="summary")
	plot(Data.oplsda,typeVc="x-score",parLabVc=groupname)
        VIP <- getVipVn(Data.oplsda);
        VIP <- VIP[VIP>1];
        write.table(VIP,file=paste(outDir,"/",SamID,".OPLSDA.VIP.txt",sep=""),quote = F,sep="\t",col.names=NA);
}

dev.off()
