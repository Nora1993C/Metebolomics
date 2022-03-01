library(ROCR)
library(pROC)
library(plotrix)
library(RColorBrewer)
library(tidyverse)
library(beeswarm)

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


out <- matrix(NA,ncol=2,nrow=1)
for(n in 1:length(levels(stat$metas))){
  tt <- stat[stat$metas == levels(stat$metas)[n],]
  model <-glm(tt$GRP1~tt$value,family="binomial",control = list(maxit=100)) 
  pre <- predict(model,type="response")
  modelroc <- roc(tt$GRP1,pre)
  modelauc <-modelroc$auc
  print(paste0(levels(stat$name)[n],' AUC=',modelauc))
  out <- rbind(out,c(levels(stat$name)[n],modelauc))
}

cc <- t(combn(nrow(data),2))
for(n in 1:nrow(cc)){
  tt <- stat[stat$metas %in% levels(stat$metas)[cc[n,]],] %>% spread(metas,value)
  model <-glm(tt$GRP1~as.matrix(tt[,3])+as.matrix(tt[,4]),family="binomial",control = list(maxit=100)) 
  pre <- predict(model,type="response")
  modelroc <- roc(tt$GRP1,pre)
  modelauc <-modelroc$auc
  print(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]],' AUC=',modelauc))
  out <- rbind(out,c(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]]),modelauc))
}

cc <- t(combn(nrow(data),3))
for(n in 1:nrow(cc)){
  tt <- stat[stat$metas %in% levels(stat$metas)[cc[n,]],] %>% spread(metas,value)
  model <-glm(tt$GRP1~as.matrix(tt[,3])+as.matrix(tt[,4])+as.matrix(tt[,5]),family="binomial",control = list(maxit=100)) 
  pre <- predict(model,type="response")
  modelroc <- roc(tt$GRP1,pre)
  modelauc <-modelroc$auc
  print(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]],'/',levels(stat$metas)[cc[n,3]],' AUC=',modelauc))
  out <- rbind(out,c(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]],'/',levels(stat$metas)[cc[n,3]]),modelauc))
}

cc <- t(combn(nrow(data),4))
for(n in 1:nrow(cc)){
  tt <- stat[stat$metas %in% levels(stat$metas)[cc[n,]],] %>% spread(metas,value)
  model <-glm(tt$GRP1~as.matrix(tt[,3])+as.matrix(tt[,4])+as.matrix(tt[,5])+as.matrix(tt[,6]),family="binomial",control = list(maxit=100)) 
  pre <- predict(model,type="response")
  modelroc <- roc(tt$GRP1,pre)
  modelauc <-modelroc$auc
  print(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]],'/',levels(stat$metas)[cc[n,3]],'/',levels(stat$metas)[cc[n,4]],' AUC=',modelauc))
  out <- rbind(out,c(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]],'/',levels(stat$metas)[cc[n,3]],'/',levels(stat$metas)[cc[n,4]]),modelauc))
}

cc <- t(combn(nrow(data),5))
for(n in 1:nrow(cc)){
  tt <- stat[stat$metas %in% levels(stat$metas)[cc[n,]],] %>% spread(metas,value)
  model <-glm(tt$GRP1~as.matrix(tt[,3])+as.matrix(tt[,4])+as.matrix(tt[,5])+as.matrix(tt[,6])+as.matrix(tt[,7]),family="binomial",control = list(maxit=100)) 
  pre <- predict(model,type="response")
  modelroc <- roc(tt$GRP1,pre)
  modelauc <-modelroc$auc
  print(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]],'/',levels(stat$metas)[cc[n,3]],'/',levels(stat$metas)[cc[n,4]],'/',levels(stat$metas)[cc[n,5]],' AUC=',modelauc))
  out <- rbind(out,c(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]],'/',levels(stat$metas)[cc[n,3]],'/',levels(stat$metas)[cc[n,4]],'/',levels(stat$metas)[cc[n,5]]),modelauc))
}

cc <- t(combn(nrow(data),6))
for(n in 1:nrow(cc)){
  tt <- stat[stat$metas %in% levels(stat$metas)[cc[n,]],] %>% spread(metas,value)
  model <-glm(tt$GRP1~as.matrix(tt[,3])+as.matrix(tt[,4])+as.matrix(tt[,5])+as.matrix(tt[,6])+as.matrix(tt[,7])+as.matrix(tt[,8]),family="binomial",control = list(maxit=100)) 
  pre <- predict(model,type="response")
  modelroc <- roc(tt$GRP1,pre)
  modelauc <-modelroc$auc
  print(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]],'/',levels(stat$metas)[cc[n,3]],'/',levels(stat$metas)[cc[n,4]],'/',levels(stat$metas)[cc[n,5]],'/',levels(stat$metas)[cc[n,6]],' AUC=',modelauc))
  out <- rbind(out,c(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]],'/',levels(stat$metas)[cc[n,3]],'/',levels(stat$metas)[cc[n,4]],'/',levels(stat$metas)[cc[n,5]],'/',levels(stat$metas)[cc[n,6]]),modelauc))
}

cc <- t(combn(nrow(data),7))
for(n in 1:nrow(cc)){
  tt <- stat[stat$metas %in% levels(stat$metas)[cc[n,]],] %>% spread(metas,value)
  model <-glm(tt$GRP1~as.matrix(tt[,3])+as.matrix(tt[,4])+as.matrix(tt[,5])+as.matrix(tt[,6])+as.matrix(tt[,7])+as.matrix(tt[,8])+as.matrix(tt[,9]),family="binomial",control = list(maxit=100)) 
  pre <- predict(model,type="response")
  modelroc <- roc(tt$GRP1,pre)
  modelauc <-modelroc$auc
  print(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]],'/',levels(stat$metas)[cc[n,3]],'/',levels(stat$metas)[cc[n,4]],'/',levels(stat$metas)[cc[n,5]],'/',levels(stat$metas)[cc[n,6]],'/',levels(stat$metas)[cc[n,7]],' AUC=',modelauc))
  out <- rbind(out,c(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]],'/',levels(stat$metas)[cc[n,3]],'/',levels(stat$metas)[cc[n,4]],'/',levels(stat$metas)[cc[n,5]],'/',levels(stat$metas)[cc[n,6]],'/',levels(stat$metas)[cc[n,7]]),modelauc))
}

cc <- t(combn(nrow(data),8))
for(n in 1:nrow(cc)){
  tt <- stat[stat$metas %in% levels(stat$metas)[cc[n,]],] %>% spread(metas,value)
  model <-glm(tt$GRP1~as.matrix(tt[,3])+as.matrix(tt[,4])+as.matrix(tt[,5])+as.matrix(tt[,6])+as.matrix(tt[,7])+as.matrix(tt[,8])+as.matrix(tt[,9])+as.matrix(tt[,10]),family="binomial",control = list(maxit=100)) 
  pre <- predict(model,type="response")
  modelroc <- roc(tt$GRP1,pre)
  modelauc <-modelroc$auc
  print(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]],'/',levels(stat$metas)[cc[n,3]],'/',levels(stat$metas)[cc[n,4]],'/',levels(stat$metas)[cc[n,5]],'/',levels(stat$metas)[cc[n,6]],'/',levels(stat$metas)[cc[n,7]],'/',levels(stat$metas)[cc[n,8]],' AUC=',modelauc))
  out <- rbind(out,c(paste0(levels(stat$metas)[cc[n,1]],'/',levels(stat$metas)[cc[n,2]],'/',levels(stat$metas)[cc[n,3]],'/',levels(stat$metas)[cc[n,4]],'/',levels(stat$metas)[cc[n,5]],'/',levels(stat$metas)[cc[n,6]],'/',levels(stat$metas)[cc[n,7]],'/',levels(stat$metas)[cc[n,8]]),modelauc))
}

tt <- stat %>% spread(metas,value)
model <-glm(tt$GRP1~as.matrix(tt[,3])+as.matrix(tt[,4])+as.matrix(tt[,5])+as.matrix(tt[,6])+as.matrix(tt[,7])+as.matrix(tt[,8])+as.matrix(tt[,9])+as.matrix(tt[,10])+as.matrix(tt[,11]),family="binomial",control = list(maxit=100)) 
  
pre <- predict(model,type="response")
modelroc <- roc(tt$GRP1,pre)
modelauc <-modelroc$auc
sen <- modelroc$sensitivities
spe <-modelroc$specificities

p.auc <- round(modelauc,4)
p.ci <- round(as.numeric(ci.auc(modelauc))[c(1,3)],4)

t1 <- which.max(sen+spe)
p.sen <-round(sen[t1],4)*100
p.spe <-round(spe[t1],4)*100

pdf('BM.top9.roc.pdf',w=4,h=4)
#layout(matrix(1:2,nrow=1),width=c(2,1))
par(mar=c(3,3,4,0))
plot(modelroc, print.auc=F, main='benign vs malignance',auc.polygon=F, grid=c(0.1, 0.2),
grid.col=c("grey", "grey"), max.auc.polygon=F,
auc.polygon.col="white", print.thres=F,col="red",print.auc.adj=c(0,-8))
text(x=0.05,y=0.36,labels =paste0('AUC = ',p.auc),cex=1.2,adj=1,col='red')
text(x=0.05,y=0.26,labels =paste0('CI = ',p.ci[1],'-',p.ci[2]),adj=1,cex=1.2,col='red')
text(x=0.05,y=0.16,labels =paste0('Sensitivity = ',p.sen),cex=1.2,adj=1,col='red')
text(x=0.05,y=0.06,labels =paste0('Specificity = ',p.spe),adj=1,cex=1.2,col='red')
#par(mar=c(3,0,3,1))
#plot(0,1,xlab="",ylab="",axes=F,xlim=c(0,10),ylim=c(1,10),col="white")
#text(x=0,y=1:10,labels =list10[rev(1:10),3],adj=0,xpd=T,cex=0.8)
dev.off()

out[order(as.numeric(out[,2]),decreasing = T)[1:5],]
write.csv(out,'BM.roc.out.csv')

tt <- stat[stat$metas %in% strsplit(out[order(as.numeric(out[,2]),decreasing = T)[1],1],'/')[[1]],] %>% spread(metas,value)
model <-glm(tt$GRP1~as.matrix(tt[,3])+as.matrix(tt[,4])+as.matrix(tt[,5])+as.matrix(tt[,6])+as.matrix(tt[,7])+as.matrix(tt[,8])+as.matrix(tt[,9])+as.matrix(tt[,10]),family="binomial",control = list(maxit=100)) 

pre <- predict(model,type="response")
modelroc <- roc(tt$GRP1,pre)
modelauc <-modelroc$auc
sen <- modelroc$sensitivities
spe <-modelroc$specificities

p.auc <- round(modelauc,4)
p.ci <- round(as.numeric(ci.auc(modelauc))[c(1,3)],4)

t1 <- which.max(sen+spe)
p.sen <-round(sen[t1],4)*100
p.spe <-round(spe[t1],4)*100

pdf('BM.top.roc.pdf',w=6,h=4)
layout(matrix(1:2,nrow=1),width=c(2,1))
par(mar=c(3,3,4,0))
plot(modelroc, print.auc=F, main='benign vs malignance',auc.polygon=F, grid=c(0.1, 0.2),
     grid.col=c("grey", "grey"), max.auc.polygon=F,
     auc.polygon.col="white", print.thres=F,col="red",print.auc.adj=c(0,-8))
text(x=0.05,y=0.36,labels =paste0('AUC = ',p.auc),cex=1.2,adj=1,col='red')
text(x=0.05,y=0.26,labels =paste0('CI = ',p.ci[1],'-',p.ci[2]),adj=1,cex=1.2,col='red')
text(x=0.05,y=0.16,labels =paste0('Sensitivity = ',p.sen),cex=1.2,adj=1,col='red')
text(x=0.05,y=0.06,labels =paste0('Specificity = ',p.spe),adj=1,cex=1.2,col='red')
par(mar=c(3,0,3,1))
plot(0,1,xlab="",ylab="",axes=F,xlim=c(0,10),ylim=c(1,10),col="white")
text(x=0,y=2:9,labels =strsplit(out[order(as.numeric(out[,2]),decreasing = T)[1],1],'/')[[1]],adj=0,xpd=T,cex=0.8)
dev.off()

