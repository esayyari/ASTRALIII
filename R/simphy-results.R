require(ggplot2)
require(reshape2)

sizes=c(6.37,7.19)
d<-read.csv('data/simphy/total.compare.csv',sep=" ", header=F)
d$V3<-factor(d$V3,levels=c("non","0","3","5","7","10","20","25","33","75"))
d$V4<-factor(d$V4,levels=c("50","200","500","1000","2000"))
d<-d[d$V4 != "1000",]

d$V4<-droplevels(d$V4)

<<<<<<< HEAD
p<-read.csv('data/simphy/parameter.1.300.csv',sep=" ")
=======
p<-read.csv('data/parameter.1.300.csv',sep=" ")
>>>>>>> 4ac20ae93100e94d692d2d88f8d9b76036131174

d<-d[d$V3 %in% c("non","0","5","10","20","33","75"),]
d$V3<-droplevels(d$V3)

k<-merge(x=p,y=d[d$V2%in% "ASTRALIII",],by.x="Replicate",by.y="V1")
k$binedLeaves<-cut(k$Number.of.leaves, breaks = c(min(k$Number.of.leaves)-1,quantile(k$Number.of.leaves)[2:5]))


<<<<<<< HEAD
w<-read.csv('data/simphy/gtError.csv',sep=" ",header=F)
=======
w<-read.csv('data/gtError.1-300.csv',sep=" ",header=F)
>>>>>>> 4ac20ae93100e94d692d2d88f8d9b76036131174

w<-w[w$V3 != "-",]
w$V3<-droplevels(w$V3)

w$V2<-as.numeric(as.character(w$V2))
w$V3<-as.numeric(as.character(w$V3))
w$V4<-as.numeric(as.character(w$V4))
w$V5<-as.numeric(as.character(w$V5))
w$V6<-as.numeric(as.character(w$V6))
w$V7<-as.numeric(as.character(w$V7))


w$V8<-(w$V3+w$V6)/(w$V2+w$V5)
wm2000<-dcast(data=w,V1~.,fun.aggregate=mean,value.var="V8")
wm2000$V9<-2000
names(wm2000)[2]<-"meanGtError"

wm500<-dcast(data=w,V1~.,fun.aggregate = function(x){mean(head(x,500))},value.var="V8")
wm500$V9<-500
names(wm500)[2]<-"meanGtError"

wm200<-dcast(data=w,V1~.,fun.aggregate = function(x){mean(head(x,200))},value.var="V8")
wm200$V9<-200
names(wm200)[2]<-"meanGtError"

wm50<-dcast(data=w,V1~.,fun.aggregate = function(x){mean(head(x,50))},value.var="V8")
wm50$V9<-50
names(wm50)[2]<-"meanGtError"


wm<-rbind(wm2000,wm500,wm200,wm50)
names(wm)[2]<-"meanGtError"
k<-merge(x=wm,y=k,by.x="V1",by.y="Replicate")
k$meanGtErrorbin<-cut(k$meanGtError,breaks=c(min(k$meanGtError)-0.0001,quantile(k$meanGtError)[2:5]),labels=c("very low gt err","low gt err","high gt err","very high gt err"))



ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7,fill=V3))+theme_bw()+geom_boxplot()+facet_wrap(~V4,scale="free_y")+
  scale_fill_brewer(palette = "RdBu",name="")+xlab('contract threshold')+ylab('FN rates')+theme(legend.position="bottom")
ggsave('figures/simphy/boxplot-overall.pdf',width=sizes[1],height=sizes[2])



ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7,fill=V3))+theme_bw()+geom_violin()+facet_wrap(~V4)+
  scale_fill_brewer(palette = "RdBu",name="")+xlab('contract threshold')+ylab('FN rates')+theme(legend.position="bottom")
ggsave('figures/simphy/violin-overall.pdf',width=sizes[1],height=sizes[2])

ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7,fill=V3))+theme_bw()+facet_wrap(~V4)+
  stat_summary(fun.y="mean",geom="bar",group=1)+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.6)+
  xlab('contract threshold')+
  ylab('FN rates')+theme(legend.position="bottom")+scale_fill_brewer(name="",palette = 'RdBu')
ggsave('figures/simphy/mean-bar-overall.pdf',width=sizes[1],height=sizes[2])



ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7))+theme_bw()+facet_wrap(~V4)+
  stat_summary(fun.y="median",geom="line",group=1)+
  xlab('contract threshold')+
  ylab('FN rates')+theme(legend.position="bottom")
ggsave('figures/simphy/median-line-overall.pdf',width=6.37,height= 6.78)


ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7))+theme_bw()+facet_wrap(~V4)+
  stat_summary(fun.y="mean",geom="line",group=1)+
  xlab('contract threshold')+
  ylab('FN rates')+theme(legend.position="bottom")
ggsave('figures/simphy/mean-line-overall.pdf',width=6.37,height= 6.78)



ggplot(data=k,aes(x=binedLeaves,y=V7,fill=V3))+geom_boxplot(outlier.alpha = 0.5)+theme_bw()+facet_wrap(~V4,scale="free_y")+
  xlab("#taxa")+ylab('FN rates')+theme(legend.position ="bottom")+scale_fill_brewer(palette = "RdBu",name="")
ggsave('figures/simphy/boxplot-species.pdf',width=sizes[1],height=sizes[2])



ggplot(data=k,aes(x=binedLeaves,y=V7,fill=V3))+geom_boxplot(outlier.alpha = 0.5)+theme_bw()+facet_wrap(~V4,scale="free_y")+
  xlab("#taxa")+ylab('FN rates')+theme(legend.position ="bottom")+scale_fill_brewer(palette = "RdBu",name="")
ggsave('figures/simphy/boxplot-species.pdf',width=sizes[1],height=sizes[2])

k$normgen<-k$Generations/k$Haploid.efective.population.size
k$genpop<-cut(k$normgen, breaks = c(min(k$normgen)-0.001,quantile(k$normgen)[2:5]))

ggplot(data=k,aes(x=genpop,y=V7,fill=V3))+geom_boxplot(outlier.alpha = 0.5)+theme_bw()+facet_wrap(~V4,scale="free_y")+
  xlab("#Generations/#population (CU)")+ylab('FN rates')+theme(legend.position ="bottom")+scale_fill_brewer(palette = "RdBu",name="")
ggsave('figures/simphy/boxplot-generations.pdf',width=sizes[1],height=sizes[2])

k$binseq<-cut(k$mean.seq.length,breaks=c(min(k$mean.seq.length)-1,quantile(k$mean.seq.length)[2:5]))
ggplot(data=k,aes(x=binseq,y=V7,fill=V3))+geom_boxplot(outlier.alpha = 0.5)+theme_bw()+facet_wrap(~V4,scale="free_y")+
  xlab("SeqLength (bp)")+ylab('FN rates')+theme(legend.position ="bottom")+scale_fill_brewer(palette = "RdBu",name="")
ggsave('figures/simphy/boxplot-seqLength.pdf',width=sizes[1],height=sizes[2])


ggplot(data=k,aes(x=meanGtErrorbin,y=V7,fill=V3))+geom_boxplot(outlier.alpha = 0.5)+theme_bw()+facet_wrap(~V4,scale="free_y")+
  xlab("gt Error")+ylab('FN rates')+theme(legend.position ="bottom")+scale_fill_brewer(palette = "RdBu",name="")
ggsave('figures/simphy/boxplot-gtError.pdf',width=8.69,height=8.39)


ggplot(data=k,aes(x=V3,y=V7,color=V4,group=V4))+stat_summary(fun.y="mean",geom="line")+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.3)+
  facet_wrap(~meanGtErrorbin,scales="free_y")+theme_bw()+
  ylab('FN rates')+xlab('bootstrap contract threshold')+
  theme(legend.position = "bottom")+scale_color_brewer(palette="Dark2",name="")
ggsave('figures/simphy/lines-facet-gt-errror-setfree.pdf',width=8.42,height= 8.39 )


ggplot(data=k,aes(x=V3,y=V7,color=V4,group=V4))+stat_summary(fun.y="mean",geom="line")+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.3)+
  facet_wrap(~meanGtErrorbin)+theme_bw()+
  ylab('FN rates')+xlab('bootstrap contract threshold')+
  theme(legend.position = "bottom")+scale_color_brewer(palette="Dark2",name="")
ggsave('figures/simphy/lines-facet-gt-errror.pdf',width=8.42,height= 8.39 )
