setwd("D:/Work/VFs/reviewAnalysis_song_190915")

Data <- read.table("Country.txt",header = F,row.names = 1,sep = '\t')
colnames(Data) <- c("Counts")

pdf("TopCountry.pdf",width = 10,height = 6)
opar <- par(no.readonly = T)
par(mgp=c(3,0.5,0),mar=c(5,10,5,2))
pos <- barplot(rev(as.matrix(t(Data))),xlim=c(0,ceiling(max(Data))*1.2),col.main='#9ea7d0',col.lab='#9ea7d0',col.axis='#9ea7d0',ylab="",xlab = "",xaxt='n',yaxt='n',col = '#9ea7d0',border = NA,width = 0.3,space = 0.9,cex.lab=1.5,horiz = T)

axis(1,at=seq(0,ceiling(max(Data)/10)*10,length.out = 3),labels = seq(0,ceiling(max(Data)/10)*10,length.out = 3),xpd=T ,font = 2,lwd = 2 ,cex.axis=1.2,col = '#9ea7d0',col.axis= '#9ea7d0',)

text(-max(Data)/60,pos,labels = rev(rownames(Data)),font = 1,cex = 1.1,xpd=T,adj = c(1,0.5))
text(-max(Data)/60,pos[length(pos)] + (pos[2] - pos[1])*2,labels = "Top Countrys",font = 2,cex = 2.5,xpd=T,adj = c(0.5,0.5))
dev.off()


Data <- read.table('TopInstitute.txt',header = F,row.names = 1,sep = '\t')
colnames(Data) <- c("Counts")

pdf("TopInstitute.pdf",width = 10,height = 6)
opar <- par(no.readonly = T)
par(mgp=c(3,0.5,0),mar=c(3,17,5,2))
pos <- barplot(rev(as.matrix(t(Data))),xlim=c(0,ceiling(max(Data))*1.2),col.main='#9ea7d0',col.lab='#9ea7d0',col.axis='#9ea7d0',ylab="",xlab = "",xaxt='n',yaxt='n',col = '#9ea7d0',border = NA,width = 0.3,space = 0.9,cex.lab=1.5,horiz = T)

axis(1,at=seq(0,14,length.out = 3),labels = seq(0,14,length.out = 3),xpd=T ,font = 2,lwd = 2 ,cex.axis=1.2,col = '#9ea7d0',col.axis= '#9ea7d0',)

text(-max(Data)/60,pos,labels = rev(rownames(Data)),font = 1,cex = 0.7,xpd=T,adj = c(1,0.5))
text(-max(Data)/3,pos[length(pos)] + (pos[2] - pos[1])*3,labels = "Top Institute",font = 2,cex = 2.5,xpd=T,adj = c(0,0.5))
dev.off()



# Data <- read.table('TopGenes.txt',header = F,row.names = 1,sep = '\t')
# colnames(Data) <- c("Counts")
# 
# pdf("TopGenes.pdf",width = 10,height = 6)
# opar <- par(no.readonly = T)
# par(mgp=c(3,0.5,0),mar=c(3,10,5,2))
# pos <- barplot(rev(as.vector(t(Data))),xlim=c(0,ceiling(max(Data))*1.2),col.main='#9ea7d0',col.lab='#9ea7d0',col.axis='#9ea7d0',ylab="",xlab = "",xaxt='n',yaxt='n',col = '#9ea7d0',border = NA,width = 0.3,space = 0.9,cex.lab=1.5,horiz = T)
# 
# axis(1,at=seq(0,ceiling(max(Data)/10)*10,length.out = 3),labels = seq(0,ceiling(max(Data)/10)*10,length.out = 3),xpd=T ,font = 2,lwd = 2 ,cex.axis=1.2,col = '#9ea7d0',col.axis= '#9ea7d0',)
# 
# text(-max(Data)/60,pos,labels = rev(rownames(Data)),col = '#9ea7d0',font = 1,cex = 0.9,xpd=T,adj = c(1,0.5))
# text(-max(Data)/4,pos[length(pos)] + (pos[2] - pos[1])*3,labels = "Top Gene",col = '#9ea7d0',font = 2,cex = 3,xpd=T,adj = c(0,0.5))
# dev.off()




Data <- read.table('TopDisease.txt',header = F,row.names = 1,sep = '\t')
colnames(Data) <- c("Counts")

pdf("TopDisease.pdf",width = 10,height = 6)
opar <- par(no.readonly = T)
par(mgp=c(3,0.5,0),mar=c(3,13,5,2))
pos <- barplot(rev(as.vector(t(Data))),xlim=c(0,ceiling(max(Data))*1.2),col.main='#9ea7d0',col.lab='#9ea7d0',col.axis='#9ea7d0',ylab="",xlab = "",xaxt='n',yaxt='n',col = '#9ea7d0',border = NA,width = 0.3,space = 0.9,cex.lab=1.5,horiz = T)

axis(1,at=seq(0,ceiling(max(Data)/10)*10,length.out = 3),labels = seq(0,ceiling(max(Data)/10)*10,length.out = 3),xpd=T ,font = 2,lwd = 2 ,cex.axis=1.2,col = '#9ea7d0',col.axis= '#9ea7d0',)

text(-max(Data)/60,pos[2:22],labels = rev(rownames(Data))[2:22],font = 1,cex = 0.9,xpd=T,adj = c(1,0.5))
text(-max(Data)/60,pos[1],labels = rev(rownames(Data))[1],font = 1,cex = 0.75,xpd=T,adj = c(1,0.5))
text(-max(Data)/4,pos[length(pos)] + (pos[2] - pos[1])*3,labels = "Top Disease",font = 2,cex = 3,xpd=T,adj = c(0,0.5))
dev.off()



Data <- read.table('TopTerm.txt',header = F,row.names = 1,sep = '\t')
colnames(Data) <- c("Counts")

pdf("TopTerm.pdf",width = 10,height = 6)
opar <- par(no.readonly = T)
par(mgp=c(3,0.5,0),mar=c(3,10,5,2))
pos <- barplot(rev(as.vector(t(Data))),xlim=c(0,ceiling(max(Data))*1.2),col.main='#9ea7d0',col.lab='#9ea7d0',col.axis='#9ea7d0',ylab="",xlab = "",xaxt='n',yaxt='n',col = '#9ea7d0',border = NA,width = 0.3,space = 0.9,cex.lab=1.5,horiz = T)

axis(1,at=seq(0,ceiling(max(Data)/10)*10,length.out = 3),labels = seq(0,ceiling(max(Data)/10)*10,length.out = 3),xpd=T ,font = 2,lwd = 2 ,cex.axis=1.2,col = '#9ea7d0',col.axis= '#9ea7d0',)

text(-max(Data)/60,pos,labels = rev(rownames(Data)),font = 1,cex = 0.9,xpd=T,adj = c(1,0.5))
text(-max(Data)/4,pos[length(pos)] + (pos[2] - pos[1])*3,labels = "Top Term",font = 2,cex = 3,xpd=T,adj = c(0,0.5))
dev.off()


Data <- read.table('TopJournal.txt',header = F,row.names = 1,sep = '\t')
colnames(Data) <- c("Counts")

pdf("TopJournal.pdf",width = 10,height = 6)
opar <- par(no.readonly = T)
par(mgp=c(3,0.5,0),mar=c(3,10,5,2))
pos <- barplot(rev(as.vector(t(Data))),xlim=c(0,ceiling(max(Data))*1.2),col.main='#9ea7d0',col.lab='#9ea7d0',col.axis='#9ea7d0',ylab="",xlab = "",xaxt='n',yaxt='n',col = '#9ea7d0',border = NA,width = 0.3,space = 0.9,cex.lab=1.5,horiz = T)

axis(1,at=seq(0,ceiling(max(Data)/10)*10,length.out = 3),labels = seq(0,ceiling(max(Data)/10)*10,length.out = 3),xpd=T ,font = 2,lwd = 2 ,cex.axis=1.2,col = '#9ea7d0',col.axis= '#9ea7d0',)

text(-max(Data)/60,pos,labels = rev(rownames(Data)),font = 1,cex = 0.7,xpd=T,adj = c(1,0.5))
text(-max(Data)/4,pos[length(pos)] + (pos[2] - pos[1])*3,labels = "Top Journal",font = 2,cex = 3,xpd=T,adj = c(0,0.5))
dev.off()



Data <- read.table('ImpactFactor.txt',header = F)
pdf("IF.pdf",width = 10,height = 6)
opar <- par(no.readonly = T)
par(mgp=c(3,0.5,0),mar=c(3,5,5,2))


pos <- hist(Data$V1,ylim = c(0,300),xlim = c(0,50),col.main='#9ea7d0',col.lab='#9ea7d0',col.axis='#9ea7d0',ylab="Frequency",xlab = "",yaxt='n',col = '#9ea7d0',border = NA,cex.lab=1.5,main = "",lwd = 2)
axis(2,at=seq(0,300,length.out = 3),labels = seq(0,300,length.out = 3),xpd=T ,font = 2,lwd = 2 ,cex.axis=1.2,col = '#9ea7d0',col.axis= '#9ea7d0')
# text(pos,-max(Data)/60,labels = row.names(Data) ,col = '#9ea7d0',font = 1,cex = 1.1,xpd=T,srt = 35,adj = c(1,1))
text(5,350,labels = "IF distribution",col = '#9ea7d0',font = 2,cex = 3,xpd=T,adj = c(0.5,1))
dev.off()