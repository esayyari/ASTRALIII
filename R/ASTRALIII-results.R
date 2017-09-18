require(ggplot2)
require(reshape2)
require(scales)
require(Hmisc)
d<-read.csv('data/ASTRALIII/latest/compare.latest.csv', sep=" ",header=F)

d$V1<-as.numeric(as.character(d$V1))
p<-read.csv('data/ASTRALIII/parameter.log.info',sep=" ",header=T)
g<-read.csv('data/ASTRALIII/gtError.csv',sep=" ",header=F)

tr<-d[d$V4 == "true",]
trT<-dcast(data=tr,V6~.,fun.aggregate=mean,value.var="V9")
names(trT)[2]<-"rf"

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

ggplot(data=gT,aes(x=rf,color=as.factor(V2)))+geom_density(adjust=1.5,alpha=0.5)+
  theme_bw()+theme(legend.position = c(0.88,0.7))+scale_color_brewer(palette = "Set1",name="")+
  xlab("FN rate")+ylab("Density")
ggsave('figures/ASTRALIII/latest/gtError.pdf',width=5.17,height=4.6)

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
d<-d[d$V4 == "estimated",]

gT<-merge(x=d,y=gT,by.x=c("V1","V3"),by.y=c("V1","V2"))

k<-merge(x=p,y=gT,by.x="Replicate",by.y="V1")

k$V5<-factor(k$V5,levels=c("non","0","3","5","7","10","20","33","50","75"))


k$meanGtErrorbin<-cut(k$rf,breaks=c(0,1/4,1/3,1/2,1),labels=c("very low (<25%)","low (<33%)","high (<50%)","very high (<100%)"))
#k$meanGtErrorbin<-cut(k$rf,breaks=c(min(k$rf)-0.0001,quantile(k$rf)[2:5]),labels=c("very low gt err","low gt err","high gt err","very high gt err"))
k$meanGtErrorbin<-cut(k$rf,breaks=c(0,1/2,1),labels=c("Low (<50%)","High (>50%)"))


k$AL=as.factor(k$V3)


ggplot(data=k,aes(x=V5,y=V9,color=V3))+facet_grid(V6~V2,scales="free_y")+geom_boxplot()+
  theme_bw()+xlab("contraction")+ylab("FN ratio")
ggsave('figures/ASTRALIII/latest/boxplot-contraction-all-ASTRALIII.pdf',width=8.69, height=8.4)


ggplot(data=k,aes(x=V5,y=V9))+facet_grid(V6~V2,scales="free_y")+stat_summary(group=1,geom="point")+
  theme_bw()+xlab("contraction")+ylab("FN ratio")+stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
                                                               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)
ggsave('figures/ASTRALIII/latest/mean-point-contraction-all-ASTRALIII.pdf',width=8.69, height=8.4)


ggplot(data=k,aes(x=V5,y=V9,group=meanGtErrorbin,color=meanGtErrorbin))+
  facet_grid(V6~V2,scales="free_y")+stat_summary(geom="line")+
  theme_bw()+xlab("contraction")+ylab("FN ratio")+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
  fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  theme(legend.position = "bottom")+scale_color_brewer(palette = "Dark2",name="")
ggsave('figures/ASTRALIII/latest/mean-point-contraction-gtError-ASTRALIII.pdf',width=8.69, height=8.4)

ggplot(data=k[k$V2 %in% c("ASTRAL.5.5.4"),],aes(x=V5,y=V9))+
  stat_summary(aes(group=meanGtErrorbin,color=meanGtErrorbin),geom=c("line","point"),linetype=2)+
  stat_summary(aes(group=meanGtErrorbin,color=meanGtErrorbin),
               geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.2,linetype=2)+
  stat_summary(aes(group=1),geom="line",linetype=1)+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  facet_wrap(~V6,scales="free_y",nrow=1)+
  theme_bw()+xlab("contraction threshold")+ylab("Species tree error (FN ratio)")+
  theme(legend.position = c(.08,.7))+scale_color_brewer(palette = "Dark2",name="Mean GT error")
ggsave('figures/ASTRALIII/latest/mean-point-contraction-gtError-ASTRALIII-paper.pdf',width=12, height=4)

k$AL <- as.factor(k$AL)
k$AL <- factor(k$AL,levels = c("200","400","800","1600"))

ggplot(data=k[k$V2 %in% c("ASTRAL.5.5.4") & k$V5!="75",],aes(x=V5,y=V9))+
  stat_summary(aes(group=AL,color=AL),geom=c("line"),linetype=2)+
  stat_summary(aes(group=AL,color=AL),geom=c("point"),size=0.5)+
  #stat_summary(aes(group=AL,color=AL),geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
  #             fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.2,linetype=2)+
  stat_summary(aes(group=1),geom="line",linetype=1)+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  facet_wrap(~V6,scales="free_y",nrow=1)+
  theme_bw()+xlab("contraction threshold")+ylab("Species tree error (FN ratio)")+
  theme(legend.position = c(.08,.8),legend.direction = 2)+scale_color_brewer(palette = "Dark2",name="Seq. len")
ggsave('figures/ASTRALIII/latest/mean-point-contraction-AL-ASTRALIII-paper.pdf',width=12, height=4.1)


ggplot(data=k[k$V2 %in% c("ASTRAL.5.5.4"),],aes(x=as.factor(AL),y=V9,fill=V5))+facet_wrap(~V6,scales="free_y")+geom_boxplot(group=1)+
  theme_bw()+xlab("seq length")+ylab("FN ratio")+scale_fill_brewer(palette = "RdBu",name="")+
  theme(legend.position ="bottom")
ggsave('figures/ASTRALIII/latest/boxplot-contraction-seqLength-ASTRALIII.pdf',width=8.69, height=9)


ggplot(data=k[k$V2 %in% c("ASTRAL.5.5.4"),],aes(x=meanGtErrorbin,y=V9,fill=V5))+facet_wrap(~V6,scales="free_y")+geom_boxplot(group=1)+
  theme_bw()+xlab("gt Error")+ylab("FN ratio")+scale_fill_brewer(palette = "RdBu",name="")+
  theme(legend.position ="bottom")
ggsave('figures/ASTRALIII/latest/boxplot-contraction-gtError-ASTRALIII.pdf',width=8.69, height=9)


ggplot(data=k[k$V2 %in% c("ASTRAL.5.5.4"),],aes(x=as.factor(V6),y=V9,color=V5,group=V5))+
  stat_summary(group=1,fun.y=mean,geom="point")+
  facet_wrap(~meanGtErrorbin,scales="free_y")+theme_bw()+xlab("number of genes")+ylab("FN ratio")+
  scale_fill_brewer(palette = "Set1",name="")+
  theme(legend.position ="bottom")
ggsave('figures/ASTRALIII/latest/point-contraction-gtError-ASTRALIII.pdf',width=8.69, height=9)

time<-read.csv('data/ASTRALIII/latest/timing.overall.latest.csv',sep=" ",header=F)
time$V4<-factor(time$V4,levels=c("non","0","3","5","7","10","20","33","50","75"))

ggplot(data=time[time$V2 %in% c("ASTRAL.5.5.4"),],
       aes(x=V4,y=V7,color=as.factor(V5),group=as.factor(V5)))+
         stat_summary(fun.y="mean",geom="line")+theme_bw()+
  stat_summary(fun.y="mean",geom="point")+theme(legend.position =c(.88,.75))+
  scale_color_brewer(palette = "Set1",name="")+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},
               width=0.3)+xlab("contraction")+ylab("Running time (seconds)")
ggsave('figures/ASTRALIII/latest/runningTime.pdf',width=5.71,height=4.62)

f<-read.csv('data/ASTRALIII/species.gtError.csv',sep=" ",header=F)
ggplot(data=f,aes(x=V4))+geom_density(adjustment=1.5)+theme_bw()+xlab("FN rate")
ggsave('figures/ASTRALIII/latest/speciesGTError.pdf',width=5.17,height=4.6)


w<-read.csv('data/ASTRALIII/latest/setx.latest.csv',sep=" ",header=F)
w$V4<-factor(w$V4,levels=c("non","0","3","5","7","10","20","33","50","75"))


ggplot(data=w[],aes(x=V4,y=V7,color=as.factor(V3),group=as.factor(V3)))+
  stat_summary(fun.y="mean",geom="line")+stat_summary(fun.y="mean",geom="point")+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  facet_grid(V2~V5)+theme_bw()+scale_color_brewer(palette = "Set1",name="")+
  theme(legend.position = "bottom")+xlab("contraction")+ylab("Set X cluster size")
ggsave('figures/ASTRALIII/latest/setXsize.pdf',width=8.5,height=8.5)

ggplot(data=time[time$V6 == "estimated",],
       aes(x=V4,y=V7,color=as.factor(V3),group=interaction(V2,as.factor(V3)),linetype=V2))+
  stat_summary(fun.y="mean",geom="line")+theme_bw()+
  stat_summary(fun.y="mean",geom="point")+theme(legend.position ="bottom")+
  scale_color_brewer(palette = "Set1",name="")+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},
               width=0.3)+xlab("contraction")+ylab("Running time (seconds)")+
  facet_wrap(~V5,scales = "free_y")+
  scale_linetype_manual(name="",labels=c("ASTRAL-II","ASTRAL-III"),values=c(2,1))
ggsave("figures/ASTRALIII/latest/time-both.pdf")

ggplot(data=w[w$V6 == "estimated" & w$V3 %in% c("1600","200"),],
       aes(x=V4,y=V7,color=as.factor(V3),group=interaction(V2,as.factor(V3)),linetype=V2))+
  stat_summary(fun.y="mean",geom="line")+stat_summary(fun.y="mean",geom="point")+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
                                  fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  facet_wrap(~V5,nrow=1)+theme_bw()+scale_color_brewer(palette = "Set1",name="")+
  theme(legend.position = "bottom",axis.text = element_text(size=12,color="black"),
        axis.title = element_text(size=12,color="black"),
        strip.text.x = element_text(size=12,color="black"),
        legend.text=element_text(size=12,color="black"))+xlab("contraction")+ylab("Set X cluster size")+
  scale_linetype_manual(name="",labels=c("ASTRAL-II","ASTRAL-III"),values=c(2,1))
ggsave("figures/ASTRALIII/latest/setX-both.pdf",width=14 , height= 5)
       




htmp<-read.csv('data/ASTRALIII/weightcalculations.csv',sep=" ",header=F)
h<-read.csv('data/ASTRALIII/latest/weightcalctime.latest.csv',sep=" ",header=F)

h$V4<-factor(h$V4,levels=c("non","0","3","5","7","10","20","33","50","75"))
h$avgWeghtTime<-(h$V8-h$V7)/h$V9

ggplot(data=h[h$V6 == "estimated", ],aes(x=V4,y=avgWeghtTime,color=V2,group=interaction(V2,as.factor(V3)),linetype=as.factor(V3)))+
  stat_summary(fun.y="mean",geom="line")+#stat_summary(fun.y="mean",geom="point")+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.3,linetype=1,size=0.3)+
  facet_wrap(~V5,nrow=1)+theme_bw()+scale_color_brewer(palette = "Set1",name="",labels=c("ASTRAL-II","ASTRAL-III"))+
  theme(legend.position = c(.1,.7),legend.direction = 2)+xlab("contraction")+ylab("Avg weight calculation time (seconds)")+
  scale_linetype_manual(name="",values=c(4,3,2,1))+
  scale_y_continuous(trans = 'log2',breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))
ggsave("figures/ASTRALIII/latest/weightCalc.pdf",width=12,height = 4.2)

ggplot(data=h[h$V4 %in% c("non","0","3","5","7"),],aes(x=V4,y=avgWeghtTime,color=as.factor(V3),group=interaction(V2,as.factor(V3)),linetype=V2))+
  stat_summary(fun.y="mean",geom="line")+stat_summary(fun.y="mean",geom="point")+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  facet_wrap(~V5,scales="free_y")+theme_bw()+scale_color_brewer(palette = "Set1",name="")+
  theme(legend.position = "bottom")+xlab("contraction")+ylab("Avg weight calculation time (log2)")+
  scale_linetype_manual(name="",labels=c("ASTRAL-II","ASTRAL-III"),values=c(2,1))+
  scale_y_continuous(trans = 'log2',breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))
ggsave("figures/ASTRALIII/latest/weightCalc-zoomed.pdf")





d<-read.csv('data/ASTRALIII/latest/compare.latest.csv', sep=" ",header=F)
d$V5<-factor(d$V5,levels=c("non","0","3","5","7","10","20","33","50","75"))

d$V3<-factor(d$V3,levels=c("200","400","800","1600"))
levels(d$V3)<-list("200bp"="200","400bp"="400","800bp"="800","1600bp"="1600")
h<-dcast(d[d$V4 == "estimated",],V1+V3+V6+V5~V2, value.var = c("V9"))

h$diff<-(h$ASTRAL.5.5.4-h$ASTRAL.4.11.2)
ggplot(aes(x=V5,y=diff),data=h)+geom_boxplot(outlier.size=0.5,outlier.alpha=0.2)+
      xlab("contraction")+ylab("Change in FN rates since ASTRAL-II")+
  facet_grid(V3~V6,scales="free")+ stat_summary(fun.y=mean,color="red",geom="point",size=0.8)+theme_classic()+
  theme(axis.text.x  = element_text(hjust=1,angle = 90 ,color="black",size=10),
        axis.text.y = element_text(hjust=1,color="black",size=12),
        text= element_text(color="black",size=14))
 
ggsave("figures/ASTRALIII/latest/comparison_astral.5.5.4_and_astral.4.11.2.pdf")




h<-dcast(w[w$V6 == "estimated",],V1+V3+V4+V5~V2, value.var ="V7")
h$V4<-factor(h$V4,levels=c("non","0","3","5","7","10","20","33","50","75"))
h$V3<-factor(h$V3,levels=c("200","400","800","1600"))
levels(h$V3)<-list("200bp"="200","400bp"="400","800bp"="800","1600bp"="1600")

h$diff<-(h$ASTRAL.5.5.4-h$ASTRAL.4.11.2)/h$ASTRAL.4.11.2*100


ggplot(aes(x=V4,y=diff),data=h)+geom_boxplot(outlier.size=0.5,outlier.alpha=0.2)+
  xlab("contraction")+ylab("Percent change of search spaces (|X|) since ASTRAL-II")+
  facet_grid(V3~V5,scales="free")+ 
  stat_summary(fun.y=mean,color="red",geom="point",size=0.8)+theme_classic()+
  theme(axis.text.x  = element_text(hjust=1,angle = 90 ,color="black",size=10),
        axis.text.y = element_text(hjust=1,color="black",size=12),
        text= element_text(color="black",size=14))

ggsave("figures/ASTRALIII/latest/comparison_setx.astral.5.5.4_and_astral.4.11.2.pdf")




q<-read.csv("data/ASTRALIII/latest/quartetscore.latest.csv",header=F,sep = " ")
h<-dcast(q[q$V6 == "estimated",],V1+V3+V4+V5~V2, value.var ="V7")
h$V4<-factor(h$V4,levels=c("non","0","3","5","7","10","20","33","50","75"))
h$V3<-factor(h$V3,levels=c("200","400","800","1600"))
levels(h$V3)<-list("200bp"="200","400bp"="400","800bp"="800","1600bp"="1600")

h$diff<-(h$ASTRAL.5.5.4-h$ASTRAL.4.11.2)/h$ASTRAL.4.11.2*100

ggplot(aes(x=V4,y=diff),data=h)+geom_boxplot(outlier.size=0.5,outlier.alpha=0.2)+
  xlab("contraction")+ylab("Percent change of quartet scores since ASTRAL-II")+
  facet_grid(V3~V5,scales="free")+ 
  stat_summary(fun.y=mean,color="red",geom="point",size=0.8)+theme_classic()+
  theme(axis.text.x  = element_text(hjust=1,angle = 90 ,color="black",size=10),
        axis.text.y = element_text(hjust=1,color="black",size=12),
        text= element_text(color="black",size=14))

ggsave("figures/ASTRALIII/latest/comparison_qscore.astral.5.5.4_and_astral.4.11.2.pdf")



d5<-d[d$V2 == "ASTRAL.5.5.4",]
d5$V3 <- factor(d5$V3,levels=c("200","400","800","1600"))
levels(d5$V3)<-list("200bp"="200","400bp"="400","800bp"="800","1600bp"="1600")
d5sum<-dcast(data=d5,V6+V3~V5,fun.aggregate = function(x){mean(x)*100},value.var = c("V9"))
latex(format.df(d5sum,2),file = "figures/ASTRALIII/latest/S100FN.tex")
dt<-d[d$V4 == "true",]
format.df(dcast(data=dt,V6~.,fun.aggregate = function(x){mean(x)*100},value.var="V9"),digits = 2)




h<-dcast(d[d$V4 == "estimated" & d$V5== "non",],V1+V3+V6+V5~V2, value.var = c("V9"))

h$diff<-(h$ASTRAL.5.5.4-h$ASTRAL.4.11.2)

h1<-dcast(w[w$V6 == "estimated" & w$V4=="non",],V1+V3+V4+V5~V2, value.var ="V7")
h1$V4<-factor(h1$V4,levels=c("non","0","3","5","7","10","20","33","50","75"))
h1$V3<-factor(h1$V3,levels=c("200","400","800","1600"))
levels(h1$V3)<-list("200bp"="200","400bp"="400","800bp"="800","1600bp"="1600")

h1$diff<-(h1$ASTRAL.5.5.4-h1$ASTRAL.4.11.2)/h1$ASTRAL.4.11.2*100





q<-read.csv("data/ASTRALIII/latest/quartetscore.latest.csv",header=F,sep = " ")
h2<-dcast(q[q$V6 == "estimated" & q$V4 == "non",],V1+V3+V4+V5~V2, value.var ="V7")
h2$V4<-factor(h2$V4,levels=c("non","0","3","5","7","10","20","33","50","75"))
h2$V3<-factor(h2$V3,levels=c("200","400","800","1600"))
levels(h2$V3)<-list("200bp"="200","400bp"="400","800bp"="800","1600bp"="1600")

h2$diff<-(h2$ASTRAL.5.5.4-h2$ASTRAL.4.11.2)/h2$ASTRAL.4.11.2*100


hsum<-merge(x=h,y=h1,by.x=c("V1","V3","V6","V5"),by.y=c("V1","V3","V5","V4"))

hsum<-merge(x=hsum,y=h2,by.x=c("V1","V3","V6","V5"),by.y=c("V1","V3","V5","V4"))

hsum<-hsum[,c(1,2,3,7,10,13)]
names(hsum)<-c("V1","V3","V6","FN","SpaceX","quartetScore")
nrow(hsum)

stderr<-function(x){sd(x)/sqrt(length(x))}
finalt1<-cbind(dcast(data=hsum,V3+V6~.,fun.aggregate=mean,value.var = "FN"),
      dcast(data=hsum,V3+V6~.,fun.aggregate=stderr,value.var = "FN"))[,c(1,2,3,6)]

finalt1$FN<-paste(format.df(finalt1$.,digits=5),format.df(finalt1$..1,digits=2),sep=" (")
finalt1$FN<-paste(finalt1$FN,")",sep="")

finalt2<-cbind(dcast(data=hsum,V3+V6~.,fun.aggregate=mean,value.var = "SpaceX"),
      dcast(data=hsum,V3+V6~.,fun.aggregate=stderr,value.var = "SpaceX"))[,c(1,2,3,6)]
finalt2$SpaceX<-paste(format.df(finalt2$.,digits=3),format.df(finalt2$..1,digits=1),sep=" (")
finalt2$SpaceX<-paste(finalt2$SpaceX,")",sep="")


finalt3<-cbind(dcast(data=hsum,V3+V6~.,fun.aggregate=mean,value.var = "quartetScore"),
      dcast(data=hsum,V3+V6~.,fun.aggregate=stderr,value.var = "quartetScore"))[,c(1,2,3,6)]
finalt3$quartetScore<-paste(format.df(finalt3$.,digits=5),format.df(finalt3$..1,digits=2),sep=" (")
finalt3$quartetScore<-paste(finalt3$quartetScore,")",sep="")

finalt<-merge(x=finalt1,y=finalt2,by.x=c("V3","V6"),by.y=c("V3","V6"))
finalt<-merge(x=finalt,y=finalt3,by.x=c("V3","V6"),by.y=c("V3","V6"))
finalt<-finalt[,c(1,2,5,8,11)]
finalt$model<-paste(finalt$V6,finalt$V3,sep="-")
finalt<-finalt[,c(6,3,4,5)]
latex(finalt,file = "figures/ASTRALIII/latest/finaltable.tex")
