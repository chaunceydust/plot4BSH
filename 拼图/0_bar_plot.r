setwd("D:/Lab/Lab 1207/Lac/pre-Animals/R2/LC MS/Liver")
noteTrans <- function(temp,x,y,z){
    tt <- temp
    tt[temp < x] <- '*'
    tt[temp < y] <- '**'
    tt[temp < z] <- '***'
    tt[temp >= x] <- ''
    return(tt)
}
###data
data <- read.table('GDCA.txt',header=F,sep='\t')
Groups <- unique(data[,1])

stat <- matrix(NA,nrow=length(Groups),ncol=4)
rownames(stat) <- Groups
colnames(stat) <- c('mean','sd','p','fdr.p')
for (i in 1:length(Groups))
{
    stat[i,1] <- mean(as.numeric(data[data[,1] == Groups[i],2]))
    stat[i,2] <- sd(as.numeric(data[data[,1] == Groups[i],2]))/length(as.numeric(data[data[,1] == Groups[i],2]))
}
# for (i in 1:length(Groups))
# {
# 	tt <- wilcox.test(as.numeric(data[data[,1] == Groups[i],2]),as.numeric(data[data[,1] == Groups[1],2]))
#     stat[i,3] <- tt$p.value
# }
# stat[2:length(Groups),4] <- p.adjust(stat[2:length(Groups),3])
tt <- wilcox.test(as.numeric(data[data[,1] == Groups[2],2]),as.numeric(data[data[,1] == Groups[1],2]))
p1 <- tt$p.value

tt <- wilcox.test(as.numeric(data[data[,1] == Groups[4],2]),as.numeric(data[data[,1] == Groups[3],2]))
p2 <- tt$p.value

#write.csv(stat, 'GDCA.stat.csv')

mycol <- c("PaleGreen3","SpringGreen4","DodgerBlue","RoyalBlue3")
Ylim <- 0.00008
pdf("GDCA.pdf",w=5,h=5)
par(mar=c(6,4,3,1),mgp=c(2,0.5,0))
## bar
pos <- barplot(stat[,1],beside=T,main="GDCA",cex.main=2,xlab="",ylab="Concentration(ug/mL)",col=mycol,xaxt='n',yaxt='n',cex.lab=1.5,ylim=c(0,Ylim),plot=T)
axis(side=2,at=seq(0,Ylim,Ylim/2),cex.axis=1.5)
text(x=pos,y=-0.000005,labels=c('Control','C-L.reuteri','TUDCA','T-L.reuteri'),xpd=T,srt = 45, adj = 1,cex=1.5,xpd = TRUE)

segments(x0=pos,y0=stat[,1],x1=pos,y1=stat[,1]+stat[,2],lwd=1,xpd=T)
segments(x0=pos-0.1,y0=stat[,1]+stat[,2],x1=pos+0.1,y1=stat[,1]+stat[,2],lwd=1,xpd=T)

abline(v = mean(pos), 
       lty = 2,
       lwd = 2,
       col = "grey")

flag <- 1.2*max(stat[,2] + stat[,1])
text(x=mean(pos[1:2]), y = flag,
     labels = paste0("p = ", round(p1, digits = 2)))
text(x=mean(pos[3:4]), y = flag,
     labels = paste0("p = ", round(p2, digits = 2)))
# text(x=pos[3:length(Groups)],y=stat[3:length(Groups),1]+stat[3:length(Groups),2]+0.1,labels=noteTrans(stat[3:length(Groups),4],0.05,0.01,0.001),xpd=T)
dev.off()


