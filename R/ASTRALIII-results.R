require(ggplot2)
require(reshape2)

d<-read.csv('data/ASTRALIII/ASTRALIII.compare.csv', sep=" ",header=F)
p<-read.csv('data/ASTRALIII/parameter.log.info',sep=" ",header=T)
g<-read.csv('data/ASTRALIII/gtError.csv',sep=" ",header=F)
d<-d[d$V2 == "ASTRALIII",]
d$V2<-droplevels(d$V2)


tr<-read.csv('data/ASTRALIII/truecompare.csv',sep=" ",header=F)
trT<-dcast(data=tr,V3~.,fun.aggregate=mean,value.var="V7")
names(trT)[3]<-"rf"

po<-read.csv('data/ASTRALIII/polytomystat.csv',sep=" ",header=F)
po$poly<-(po$V10)/(po$V7-3)


g<-g[g$V6 != "-",]
g$V6<-droplevels(g$V6)
g$V5<-droplevels(g$V5)

g$V5<-as.numeric(as.character(g$V5))
g$V6<-as.numeric(as.character(g$V6))
g$V7<-as.numeric(as.character(g$V7))
g$V8<-as.numeric(as.character(g$V8))
g$V9<-as.numeric(as.character(g$V9))
g$V10<-as.numeric(as.character(g$V10))

g$rf<-(g$V6+g$V9)/(g$V5+g$V8)


gT<-dcast(data=g[g$V3=="non",],V1+V2~.,fun.aggregate = mean,value.var = "rf")
names(gT)[3] = "rf"

poT<-dcast(data=po[po$V4 == 1000,],V1+V2+V3~.,fun.aggregate=mean, value.var="poly")
names(poT)[4]<-"poly"

# gT1000$V4<-1000
# gT500<-dcast(data=g[g$V3=="non",],V1+V2+V3~.,fun.aggregate = function(x){mean(head(x,500))},value.var="rf")
# names(gT500)[4] = "rf"
# gT500$V4<-500
# gT200<-dcast(data=g[g$V3=="non",],V1+V2+V3~.,fun.aggregate = function(x){mean(head(x,200))},value.var="rf")
# names(gT200)[4] = "rf"
# gT200$V4<-200
# gT50<-dcast(data=g[g$V3=="non",],V1+V2+V3~.,fun.aggregate = function(x){mean(head(x,50))},value.var="rf")
# names(gT50)[4] = "rf"
# gT50$V4<-50
# gT<-rbind(gT1000,gT500,gT200,gT50)

gT<-merge(x=d,y=gT,by.x=c("V1","V3"),by.y=c("V1","V2"))

k<-merge(x=p,y=gT,by.x="Replicate",by.y="V1")

k$V4<-factor(k$V4,levels=c("non","0","3","5","7","10","20","33","50","75"))


k$meanGtErrorbin<-cut(k$rf,breaks=c(0,1/4,1/3,1/2,1),labels=c("very low (<25%)","low (<33%)","high (<50%)","very high (<100%)"))
#k$meanGtErrorbin<-cut(k$rf,breaks=c(min(k$rf)-0.0001,quantile(k$rf)[2:5]),labels=c("very low gt err","low gt err","high gt err","very high gt err"))

k$AL=as.factor(k$V3)


ggplot(data=k,aes(x=V4,y=V8))+facet_wrap(~V5,scales="free_y")+geom_boxplot()+
  theme_bw()+xlab("contraction")+ylab("FN ratio")
ggsave('figures/ASTRALIII/boxplot-contraction-all-ASTRALIII.pdf',width=8.69, height=8.4)


ggplot(data=k,aes(x=V4,y=V8))+facet_wrap(~V5,scales="free_y")+stat_summary(group=1,geom="point")+
  theme_bw()+xlab("contraction")+ylab("FN ratio")+stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
                                                               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)
ggsave('figures/ASTRALIII/mean-point-contraction-all-ASTRALIII.pdf',width=8.69, height=8.4)


ggplot(data=k,aes(x=V4,y=V8,group=meanGtErrorbin,color=meanGtErrorbin))+
  facet_wrap(~V5,scales="free_y")+stat_summary(geom="line")+
  theme_bw()+xlab("contraction")+ylab("FN ratio")+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
  fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  theme(legend.position = "bottom")+scale_color_brewer(palette = "Dark2",name="")
ggsave('figures/ASTRALIII/mean-point-contraction-gtError-ASTRALIII.pdf',width=8.69, height=8.4)

ggplot(data=k,aes(x=V4,y=V8))+
  stat_summary(aes(group=meanGtErrorbin,color=meanGtErrorbin),geom=c("line","point"),linetype=2)+
  stat_summary(aes(group=meanGtErrorbin,color=meanGtErrorbin),geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.2,linetype=2)+
  stat_summary(aes(group=1),geom="line",linetype=1)+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  facet_wrap(~V5,scales="free_y",nrow=1)+
  theme_bw()+xlab("contraction threshold")+ylab("Species tree error (FN ratio)")+
  theme(legend.position = c(.08,.7))+scale_color_brewer(palette = "Dark2",name="Mean GT error")
ggsave('figures/ASTRALIII/mean-point-contraction-gtError-ASTRALIII-paper.pdf',width=12, height=4)

ggplot(data=k[k$V4!="75",],aes(x=V4,y=V8))+
  stat_summary(aes(group=AL,color=AL),geom=c("line"),linetype=2)+
  stat_summary(aes(group=AL,color=AL),geom=c("point"),size=0.5)+
  #stat_summary(aes(group=AL,color=AL),geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
  #             fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.2,linetype=2)+
  stat_summary(aes(group=1),geom="line",linetype=1)+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  facet_wrap(~V5,scales="free_y",nrow=1)+
  theme_bw()+xlab("contraction threshold")+ylab("Species tree error (FN ratio)")+
  theme(legend.position = c(.08,.8),legend.direction = 2)+scale_color_brewer(palette = "Dark2",name="Seq. len")
ggsave('figures/ASTRALIII/mean-point-contraction-AL-ASTRALIII-paper.pdf',width=12, height=4.1)


ggplot(data=k,aes(x=as.factor(V3),y=V8,fill=V4))+facet_wrap(~V5,scales="free_y")+geom_boxplot(group=1)+
  theme_bw()+xlab("seq length")+ylab("FN ratio")+scale_fill_brewer(palette = "RdBu",name="")+
  theme(legend.position ="bottom")
ggsave('figures/ASTRALIII/boxplot-contraction-seqLength-ASTRALIII.pdf',width=8.69, height=9)


ggplot(data=k,aes(x=meanGtErrorbin,y=V8,fill=V4))+facet_wrap(~V5,scales="free_y")+geom_boxplot(group=1)+
  theme_bw()+xlab("gt Error")+ylab("FN ratio")+scale_fill_brewer(palette = "RdBu",name="")+
  theme(legend.position ="bottom")
ggsave('figures/ASTRALIII/boxplot-contraction-gtError-ASTRALIII.pdf',width=8.69, height=9)


ggplot(data=k,aes(x=as.factor(V5),y=V8,color=V4,group=V4))+
  stat_summary(group=1,fun.y=mean,geom="point")+
  facet_wrap(~meanGtErrorbin,scales="free_y")+theme_bw()+xlab("number of genes")+ylab("FN ratio")+
  scale_fill_brewer(palette = "Set1",name="")+
  theme(legend.position ="bottom")
ggsave('figures/ASTRALIII/point-contraction-gtError-ASTRALIII.pdf',width=8.69, height=9)


