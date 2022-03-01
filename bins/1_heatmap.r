library(pheatmap)
library(RColorBrewer)
library(tidyverse)

F_normalize <- function(x) {
    center <- sweep(x, 1, apply(x, 1, min),'-') #在行的方向上减去最小值
    R <- apply(x, 1, max) - apply(x,1,min)   #算出极差，即列上的最大值-最小值
    x_star<- sweep(center, 1, R, "/")        #把减去均值后的矩阵在行的方向上除以极差向量
    return(x_star)
}

list <- as_tibble(read.table('iden.list.txt',sep='\t',header = T))
mz <- as_tibble(read.table('../pos.mz.dat',header = T))

list <- list %>%  left_join(mz,by=c('mz'='mzmed'))

data <- read.table('../0.0_raw_data/pos.data.txt',header=T,row.names=1)
data <- data[list$name.y,]
grpdata <- read.table('../bin/pos-grouping.info',header=T)
rownames(grpdata) <- grpdata[,1]
grpdata <- grpdata[rownames(grpdata) %in% colnames(data),]

data <- F_normalize(data)

hc <- hclust(dist(data))
rowInd <- hc$order
data <- data[rowInd,]
Groups <- levels(as.factor(grpdata[,2]))
#Groups <- Groups[c(3,1,2)]
tt.data <- list()
for(i in Groups){
	tt <- data[,rownames(grpdata)[grpdata[,2] == i]]
	hc <- hclust(dist(t(tt)))
	colInd <- hc$order
	tt.data[[i]] <- tt[,colInd]
}
plot.data <- cbind(tt.data[[Groups[1]]],tt.data[[Groups[2]]])#,tt.data[[Groups[3]]])
list <- list %>% as.data.frame()
rownames(list) <- list$name.y
add <- rep('',nrow(list))
add[list$flag != ''] <- '*'
list$add <- add
rownames(plot.data) <- paste0(list[rownames(data),3],list[rownames(data),7])

nn <- table(grpdata[,2])[Groups]
nn <- cumsum(nn)
cc <- data.frame(grpdata[,2])
rownames(cc) <- rownames(grpdata)
colnames(cc) <- 'Disease States'
ann_col <- list('Disease States'=c('B'=brewer.pal(8,"Set3")[5],'M'=brewer.pal(8,"Set3")[4]))

pdf("BM.heatmap.pdf",w=7,h=5)
par(mar=c(3,2,3,5))
pheatmap(plot.data,scale = 'row',cluster_rows = T, cluster_cols = FALSE,color = colorRampPalette(c("navy", "white", "firebrick3"))(50),border_color = 'grey80',treeheight_row = 10,main = "benign vs malignance",annotation_col=cc,gaps_col = nn,legend=F,annotation_legend=T,annotation_colors=ann_col,show_colnames=F,fontsize_row = 8)
dev.off()
