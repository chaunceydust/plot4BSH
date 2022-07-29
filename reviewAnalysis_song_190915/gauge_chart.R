setwd("D:/Work/VFs/reviewAnalysis")
library(tidyverse)
library("showtext")
library(Cairo)
library("Rmisc")
library(grid)

font_add("myfont","msyh.ttc")
bardata<-seq(from=0,to=270,length=1000)
rectdata<-seq(from=0,to=270,by=27)%>%c(360)

target<-0.35
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
  annotate("text",x=315,y=-30,label="35",size=12,hjust=.5,vjust=.5,family="myfont",col=ifelse(target<.5,"black","#38E968"),fontface="plain")+
  annotate("text",x=315,y=-15,label="Growth",size=12,hjust=.5,vjust=.5,family="myfont",col="black")+
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
