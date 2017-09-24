require(ggplot2)
require(reshape2)
d<-read.csv('~/Main/Library/ASTRALIII/data/ASTRALIII/astral.quartetScores.csv',sep=" ",header=F)
d$V4<-factor(d$V4,levels=c("non","0","3","5","7","10","20","25","33","50","75"))



d$V2<-as.factor(d$V2)
d$V3<-as.factor(d$V3)
d$V4<-as.factor(d$V4)

ggplot(data=d[d$V4 != "non",],aes(V4,V6,
      group=interaction(V2,V4),
       fill=interaction(V2)))+
  geom_boxplot()+facet_grid(V3~V5)+theme_bw()+
  scale_fill_brewer(name="",palette = "Paired")+
  theme(legend.position = "bottom")


t<-dcast(data=d,V1+V3+V4+V5~V2,value.var="V6")
names(t)<-c("V1","V3","V4","V5","old","new","diff")
t$diff <- t$old - t$new
t$normDiff <- t$diff / t$old * 100
ggplot(data=t[t$V4!= "non",],aes(V4,normDiff))+
  facet_grid(V3~V5,scales="free_y")+geom_boxplot()+theme_bw()+
  xlab("threshold")+ylab("% normalized diff between versions II and III Q.S")
