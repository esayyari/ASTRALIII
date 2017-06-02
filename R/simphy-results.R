require(ggplot2)
require(reshape2)

sizes=c(6.37,7.19)
d<-read.csv('data/total.compare.csv',sep=" ", header=F)
d$V3<-factor(d$V3,levels=c("non","0","3","5","7","10","20","25","33","75"))
d$V4<-factor(d$V4,levels=c("50","200","500","1000","2000"))
d<-d[d$V4 != "1000",]

d$V4<-droplevels(d$V4)
ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7,fill=V3))+theme_bw()+geom_boxplot()+facet_wrap(~V4,scale="free_y")+
  scale_fill_brewer(palette = "RdBu",name="")+xlab('contract threshold')+ylab('FN rates')+theme(legend.position="bottom")
ggsave('figures/boxplot-overall.pdf',width=sizes[1],height=sizes[2])



ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7,fill=V3))+theme_bw()+geom_violin()+facet_wrap(~V4)+
  scale_fill_brewer(palette = "RdBu",name="")+xlab('contract threshold')+ylab('FN rates')+theme(legend.position="bottom")
ggsave('figures/violin-overall.pdf',width=sizes[1],height=sizes[2])

ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7,fill=V3))+theme_bw()+facet_wrap(~V4)+
  stat_summary(fun.y="mean",geom="bar",group=1)+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.6)+
  xlab('contract threshold')+
  ylab('FN rates')+theme(legend.position="bottom")+scale_fill_brewer(name="",palette = 'RdBu')
ggsave('figures/mean-bar-overall.pdf',width=sizes[1],height=sizes[2])



ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7))+theme_bw()+facet_wrap(~V4)+
  stat_summary(fun.y="median",geom="line",group=1)+
  xlab('contract threshold')+
  ylab('FN rates')+theme(legend.position="bottom")
ggsave('figures/median-line-overall.pdf',width=6.37,height= 6.78)


ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7))+theme_bw()+facet_wrap(~V4)+
  stat_summary(fun.y="mean",geom="line",group=1)+
  xlab('contract threshold')+
  ylab('FN rates')+theme(legend.position="bottom")
ggsave('figures/mean-line-overall.pdf',width=6.37,height= 6.78)


p<-read.csv('data/parameter.1.300.csv',sep=" ")


k<-merge(x=p,y=d[d$V2%in% "ASTRALIII",],by.x="Replicate",by.y="V1")
k$binedLeaves<-cut(k$Number.of.leaves, breaks = c(min(k$Number.of.leaves)-1,quantile(k$Number.of.leaves)[2:5]))
ggplot(data=k,aes(x=V3,y=V7,fill=V3,group=V3))+geom_boxplot()+facet_wrap(~binedLeaves,scale="free")
