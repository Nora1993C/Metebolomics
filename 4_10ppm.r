pos.ref.id <- read.table('Pos.mz.dat',header=T,row.names=1)
neg.ref.id <- read.table('Neg.mz.dat',header=T,row.names=1)
mz.data <- rbind(pos.ref.id[,c('mzmed','mzmin','mzmax')],neg.ref.id[,c('mzmed','mzmin','mzmax')])
rownames(mz.data) <- c(paste0(rownames(pos.ref.id),'_pos'),paste0(rownames(neg.ref.id),'_neg'))
mz.data[1:nrow(pos.ref.id),1] <- mz.data[1:nrow(pos.ref.id),1]-1.0078
mz.data[(nrow(pos.ref.id)+1):nrow(mz.data),1] <- mz.data[(nrow(pos.ref.id)+1):nrow(mz.data),1]+1.0078

for (i in 1:nrow(mz.data))
{
	dd <- mz.data[i,1]*(1/1000000)*10
	mz.data[i,2] <- mz.data[i,1] - dd
	mz.data[i,3] <- mz.data[i,1] + dd
}
write.table(mz.data,'total.mz.dat',quote=F,sep='\t',row.names=T)
