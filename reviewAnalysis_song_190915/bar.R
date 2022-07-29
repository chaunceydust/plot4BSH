setwd("D:/Work/VFs/reviewAnalysis_song_190915")

Data <- read.table("PublicationCount.txt",header = F,row.names = 1,sep = '\t')
colnames(Data) <- c("Counts")

pdf("Publication.pdf",width = 10,height = 6)
opar <- par(no.readonly = T)
par(mgp=c(3,0.5,0),mar=c(5,5,2,2))
pos <- barplot(as.matrix(t(Data)),ylim=c(0,ceiling(max(Data))*1.2),col.main='#9ea7d0',col.axis='#9ea7d0',ylab="Publications",xlab = "year",xaxt='n',yaxt='n',col = '#9ea7d0',border = NA,width = 0.3,space = 0.9,cex.lab=1.5)
# pos <- barplot(as.matrix(t(Data)),ylim=c(0,max(Data)),col.main='grey80',col.lab='grey80',col.axis='#9ea7d0',ylab="Publications",xlab = "year",xaxt='n',yaxt='n',col = "grey80",border = NA,width = 0.3,space = 0.9,cex.lab=1.5)
# axis(2,at=seq(0,25,length.out = 5),labels = seq(0,25,length.out = 5),xpd=T ,font = 2,lwd = 2 ,cex.axis=1.2,col.axis='#9ea7d0',col='#9ea7d0')
axis(2,at=seq(0,ceiling(max(Data)/10)*10,length.out = 3),labels = seq(0,ceiling(max(Data)/10)*10,length.out = 3),xpd=T ,font = 2,lwd = 2 ,cex.axis=1.2,col = '#9ea7d0',col.axis= '#9ea7d0',)
text(pos,-max(Data)/60,labels = row.names(Data) ,font = 1,cex = 1.1,xpd=T,srt = 35,adj = c(1,1))
text(pos[1]-(pos[2]-pos[1])*1,ceiling(max(Data))*1.2,labels = "Trend",font = 2,cex = 3,xpd=T,adj = c(0.5,0.5))
dev.off()

library(tidyverse)
library("showtext")
library(Cairo)
library("Rmisc")
library(grid)
par(opar)
today <- Sys.Date()
first <- as.double(as.Date(paste(substr(today,1,4), "01","01",sep="/"),format="%Y/%m/%d"))
last <- as.double(as.Date(paste(substr(today,1,4), "12","31",sep="/"),format="%Y/%m/%d"))
today <- as.double(today)

Data_growth <- Data[(nrow(Data)-4):nrow(Data),]
Data_growth[5] <- Data_growth[5]*(last-first)/(today-first)
Data_grow_rate <- c()
for (n in 2:5) {
  Data_grow_rate[n-1] <- (Data_growth[n]-Data_growth[n-1])/Data_growth[n-1];
  ifelse(Data_grow_rate[n-1] >= 1,1,Data_grow_rate[n-1] <- Data_grow_rate[n-1])
}

font_add("myfont","msyh.ttc")
bardata<-seq(from=0,to=270,length=1000)
rectdata<-seq(from=0,to=270,by=27)%>%c(360)

target<-round(mean(Data_grow_rate)*100)/100
assist<-target*270

CairoPDF(file="Growth.pdf",width=8,height=6)
showtext_begin()
ggplot(data=NULL)+
  geom_rect(aes(xmin=rectdata[-12],xmax=rectdata[-1],ymin=5,ymax=10),fill="#F2F2F2",col="white")+
  geom_bar(aes(x=bardata,y=5,col=bardata),stat="identity",fill=NA,size=2)+
  geom_text(aes(x=rectdata[-12],y=-5,label=seq(0,100,by=10)),vjust=.5,hjust=.5,size=5,family="myfont",col="#0F1110")+
  geom_segment(aes(x=assist,y=-50,xend=assist,yend=-10),arrow =arrow(length=unit(0.4,"cm")),size=1.2,col="red")+
  geom_point(aes(x=assist,y=-50),shape=21,fill="white",col="black",size=7)+
  # annotate("text",x=315,y=-30,label="35",size=12,hjust=.5,vjust=.5,family="myfont",col=ifelse(target<.5,"#F32626","#38E968"),fontface="plain")+
  annotate("text",x=315,y=-30,label=as.character(target*100),size=12,hjust=.5,vjust=.5,family="myfont",col=ifelse(target<.5,"black","#38E968"),fontface="plain")+
  annotate("text",x=315,y=-15,label="Growth(%)",size=10,hjust=.5,vjust=.5,family="myfont",col="black")+
  ylim(-50,12)+
  coord_polar(theta="x",start=179.85)+
  scale_colour_gradient(low="#38E968",high="#F32626",guide=FALSE)+
  theme_minimal()+
  theme(
    text=element_blank(),
    line=element_blank(),
    rect=element_blank()
  )
showtext_end()
dev.off()


####  多个仪表盘  ####   

# set.seed(123)
# target<-runif(5,0,1)
# assist<-270*target
# 
# CairoPNG(file="bigdashboard.png",width=1500,height=675)
# showtext.begin()
# grid.newpage()
# pushViewport(viewport(layout=grid.layout(1,5)))
# vplayout<-function(x,y){viewport(layout.pos.row =x,layout.pos.col=y)}
# for(i in 1:5){
#   p<-ggplot(data=NULL)+
#     geom_rect(aes(xmin=rectdata[-12],xmax=rectdata[-1],ymin=5,ymax=12),fill="#F2F2F2",col="white")+
#     geom_bar(aes(x=bardata,y=5,col=bardata),stat="identity",fill=NA,size=2)+
#     geom_text(aes(x=rectdata[-12],y=-5,label=seq(0,100,by=10)),vjust=.5,hjust=.5,size=3.5,family="myfont",col="#0F1110")+
#     geom_segment(aes(x=assist[i],y=-50,xend=assist[i],yend=-10),arrow =arrow(length=unit(0.4,"cm")),size=1.2,col="red")+
#     geom_point(aes(x=assist[i],y=-50),shape=21,fill="white",col="black",size=7)+
#     annotate("text",x=315,y=-30,label=percent(target[i]),size=7.5,hjust=.5,vjust=.5,family="myfont",col=ifelse(target[i]<.5,"#F32626","#38E968"),fontface="plain")+
#     annotate("text",x=315,y=-15,label=paste0("指标",i),size=8.5,hjust=.5,vjust=.5,family="myfont")+
#     ylim(-50,12)+
#     coord_polar(theta="x",start=179.85)+
#     scale_colour_gradient(low="#F32626",high="#38E968",guide=FALSE)+
#     theme_minimal()+
#     theme(
#       text=element_blank(),
#       line=element_blank(),
#       rect=element_blank()
#     )
#   print(p,vp=vplayout(1,i))
# }
# showtext.end()
# dev.off()

