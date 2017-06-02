require(ggplot2)
require(reshape2)

sizes=c(5.57,6.51)
d<-read.csv('data/total.compare.csv',sep=" ", header=F)
d$V3<-factor(d$V3,levels=c("non","0","3","5","7","10","20","25","33","75"))

ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7,fill=V3))+theme_bw()+geom_boxplot()+
  scale_fill_brewer(palette = "RdBu",name="")+xlab('contract threshold')+ylab('FN rates')+theme(legend.position="bottom")
ggsave('figures/boxplot-overall.pdf',width=sizes[1],height=sizes[2])



ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7,fill=V3))+theme_bw()+geom_violin()+
  scale_fill_brewer(palette = "RdBu",name="")+xlab('contract threshold')+ylab('FN rates')+theme(legend.position="bottom")
ggsave('figures/violin-overall.pdf',width=sizes[1],height=sizes[2])

ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7,fill=V3))+theme_bw()+
  stat_summary(fun.y="mean",geom="bar",group=1)+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.6)+
  xlab('contract threshold')+
  ylab('FN rates')+theme(legend.position="bottom")+scale_fill_brewer(name="",palette = 'RdBu')
ggsave('figures/mean-bar-overall.pdf',width=sizes[1],height=sizes[2])



ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7))+theme_bw()+
  stat_summary(fun.y="median",geom="line",group=1)+
  xlab('contract threshold')+
  ylab('FN rates')+theme(legend.position="bottom")
ggsave('figures/median-line-overall.pdf',width=5.57,height=4.72)


ggplot(data=d[d$V2 %in% "ASTRALIII",], aes(x=V3,y=V7))+theme_bw()+
  stat_summary(fun.y="mean",geom="line",group=1)+
  xlab('contract threshold')+
  ylab('FN rates')+theme(legend.position="bottom")
ggsave('figures/mean-line-overall.pdf',width=5.57,height=4.72)


p<-read.csv('data/parameter.1.300.csv',sep=" ")


k<-merge(x=p,y=d,by.x="Replicate",by.y="V1")

