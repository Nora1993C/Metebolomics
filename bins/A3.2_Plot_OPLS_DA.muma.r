library("muma")


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
groupname <- c();
Data <- read.table(dataFile,header=T,row.names = 1,sep='\t');
#colnames(Data) <- gsub("X","",colnames(Data))

Data.tm <- t(as.matrix(Data)); 
for(i in 1:length(colnames(Data)))
{
	groupname <- append(groupname,as.numeric(grpInfo[grpInfo[,1] == colnames(Data)[i],grp_col]));
}
Data.muma <- cbind(groupname,Data.tm);
outFile <- paste(SamID,".muma.csv",sep="");
write.csv(Data.muma,file=outFile);

source("/project/CYY/tt/bin/explore.data.lj.R")
source("/project/CYY/tt/bin/oplsda.lj.R")
explore.data(outFile,scaling="P", scal = TRUE, normalize = TRUE, imputation = FALSE, imput=c("NA"));

oplsda("P");

