arg <- commandArgs(T)
if(length(arg) != 5){
        cat("Argument: Data_File Group_File Out_Dir Group_column VIP_File\n")
        quit('no')
}

dataFile <- arg[1];
groupFile <- arg[2];
outDir <- arg[3];
grp_col <- as.numeric(arg[4]);
VIP_File <- arg[5];

#dataFile <- "../0.0_raw_data/liverCMIQ.pos.data.txt";
#groupFile <- "../0.0_raw_data/liverCMIQ.pos-grouping.info";
#outDir <- "./";
#grp_col <- 2;
#VIP_File <- '../3.2_OPLS_DA/liverCM.OPLSDA.VIP.txt';

SamID <- strsplit(basename(dataFile),'.',fixed=T)[[1]][1];

library(vegan);
library(MASS);

grpInfo <- read.table(groupFile,header=T);
Data <- data.frame(read.table(dataFile,header=T,row.names=1));
Ref <- data.frame(read.table(VIP_File,header=T,row.names=1));
colnames(Ref)[1] <- "VIP"
#Data <- data[rownames(Ref),];
####id 
#colnames(Data) <- gsub("X","",colnames(Data))

groupname <- c();
for(i in 1:length(colnames(Data)))
{
	groupname <- append(groupname,as.character(grpInfo[grpInfo[,1] == colnames(Data)[i],grp_col]));
}

colnames(Data) <- groupname;
Groups <- as.character(unique(grpInfo[,grp_col]))
Data.Group <- c();
for(i in 1:length(Groups))
{
	Data.Group[[i]] <- as.matrix(Data[,colnames(Data) == Groups[i]]);
        rownames(Data.Group[[i]]) <- rownames(Data);
}
print(Groups)
#Perform U test
Data.Utest <- c();
data.Utest <- c();
for(k in 1:nrow(Data)){
  Data.Utest[k] <- t.test(Data.Group[[1]][k,],Data.Group[[2]][k,])$p.value
  data.Utest[k] <- t.test(Data.Group[[3]][k,],Data.Group[[2]][k,])$p.value
}
Data.Utest.adjust <- p.adjust(Data.Utest,method='fdr')
data.Utest.adjust <- p.adjust(data.Utest,method='fdr')

stat <- cbind(name=rownames(Data),
              CM_Tp=Data.Utest,
              p_adjust=Data.Utest.adjust,
              C_mean=rowMeans(Data.Group[[1]]),
              M_mean=rowMeans(Data.Group[[2]]),
              Drug_mean=rowMeans(Data.Group[[3]]),
              MD_Tp=data.Utest,
              p_adjust=data.Utest.adjust)

flag <- log(rowMeans(Data.Group[[2]])/rowMeans(Data.Group[[1]]),base=10)*log(rowMeans(Data.Group[[3]])/rowMeans(Data.Group[[2]]),base=10)

stat <- stat[flag < 0,]
stat <- stat[stat[,3] < 0.05,]
stat <- stat[stat[,1] %in% rownames(Ref),]
stat <- cbind(stat,CM_VIP=Ref[stat[,1],1])


write.table(stat,file=paste(outDir,"/",SamID,".T.rev.txt",sep=""),quote = F,sep="\t",row.names = F)
write.table(stat[stat[,8] < 0.05,],file=paste(outDir,"/",SamID,".T.sigrev.txt",sep=""),quote = F,sep="\t",row.names = F)
