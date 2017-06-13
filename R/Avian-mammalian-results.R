require(ggplot2)
require(reshape2)
sizes=c(4.5,4.5)
d<-read.csv('data/Avian-Mammalian/avian-contraction.csv',sep="\t",header = T)

d$contraction.rate<-factor(d$contraction.rate,levels=c("ori","t0","t3","t5","t7","t10","t20", "t33", "t50","t75"))
levels(d$contraction.rate)<-list("non"="ori","0"="t0","3"="t3","5"="t5","7"="t7",
                                 "10"="t10","20"="t20","33"="t33","50"="t50","75"="t75")

ggplot(data=d[d$seq.length == "500" & d$contraction.rate != "75",],
       aes(contraction.rate,Error.Rate))+
  stat_summary(aes(group=input.type,color=input.type),
               geom=c("line","point"),linetype=2)+
  stat_summary(aes(group=input.type,color=input.type),
               geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
  fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4,linetype=2)+
  stat_summary(aes(group=1),geom="line",linetype=1)+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  xlab('contraction')+ylab('FN rate')+theme_bw()+
  theme(legend.position = c(.3,0.91),legend.direction = "horizontal")+scale_color_brewer(palette = "Dark2",name="")

ggsave('figures/Avian-Mammalian/avian-mean-point-line-contraction-seq500-paper.pdf',width=sizes[1],height=sizes[2])

ggplot(data=d[d$input.type == "1X" & d$contraction.rate != "75",],
       aes(contraction.rate,Error.Rate))+
  stat_summary(aes(group=as.factor(seq.length),color=as.factor(seq.length)),
               geom=c("line","point"),linetype=2)+
  stat_summary(aes(group=as.factor(seq.length),color=as.factor(seq.length)),
               geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4,linetype=2)+
  stat_summary(aes(group=1),geom="line",linetype=1)+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  xlab('contraction')+ylab('FN rate')+theme_bw()+
theme(legend.position = c(.35,0.91),legend.direction = "horizontal")+scale_color_brewer(palette = "Set1",name="")

ggsave('figures/Avian-Mammalian/avian-mean-point-line-contraction-type1X-paper.pdf',width=sizes[1],height=sizes[2])



ggplot(data=d[d$seq.length == "500",],
       aes(contraction.rate,Error.Rate))+
  stat_summary(aes(group=input.type,color=input.type),
               geom=c("line","point"),linetype=2)+
  stat_summary(aes(group=input.type,color=input.type),
               geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4,linetype=2)+
  stat_summary(aes(group=1),geom="line",linetype=1)+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  xlab('contraction')+ylab('FN rate')+theme_bw()+
  theme(legend.position = c(.3,0.91),legend.direction = "horizontal")+scale_color_brewer(palette = "Dark2",name="")

ggsave('figures/Avian-Mammalian/avian-mean-point-line-contraction-seq500-ALL-paper.pdf',width=sizes[1],height=sizes[2])

ggplot(data=d[d$input.type == "1X" ,],
       aes(contraction.rate,Error.Rate))+
  stat_summary(aes(group=as.factor(seq.length),color=as.factor(seq.length)),
               geom=c("line","point"),linetype=2)+
  stat_summary(aes(group=as.factor(seq.length),color=as.factor(seq.length)),
               geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4,linetype=2)+
  stat_summary(aes(group=1),geom="line",linetype=1)+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  xlab('contraction')+ylab('FN rate')+theme_bw()+
  theme(legend.position = c(.35,0.91),legend.direction = "horizontal")+scale_color_brewer(palette = "Set1",name="")

ggsave('figures/Avian-Mammalian/avian-mean-point-line-contraction-ALL-type1X.pdf',width=sizes[1],height=sizes[2])






# d<-read.csv('data/Avian-Mammalian/mammalian-contraction.csv',sep="\t",header=T)
# d$contraction.rate<-factor(d$contraction.rate,levels=c("ori","t0","t3","t5","t7","t10","t20", "t33", "t50","t75"))
# levels(d$contraction.rate)<-list("non"="ori","0"="t0","3"="t3","5"="t5","7"="t7",
#                                  "10"="t10","20"="t20","33"="t33","50"="t50","75"="t75")
# 
# 
# 
# ggplot(data=d[d$seq.length == "500",],aes(contraction.rate,Error.Rate,group=input.type,color=input.type))+
#   stat_summary(geom="line",fun.y="mean")+stat_summary(geom="point",fun.y="mean")+
#   theme_bw()+stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
#                           fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
#   theme(legend.position = "bottom")+scale_color_brewer(palette = "Dark2",name="")+
#   xlab('contraction')+ylab('FN rate')
# ggsave('figures/Avian-Mammalian/mammalian-mean-point-line-contraction-seq500.pdf',width=3.5,height=3.5)
# 
# ggplot(data=d[d$input.type == "1X",],aes(contraction.rate,Error.Rate,group=as.factor(seq.length) ,color=as.factor(seq.length)))+
#   stat_summary(geom="line",fun.y="mean")+stat_summary(geom="point",fun.y="mean")+
#   theme_bw()+stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
#                           fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
#   theme(legend.position = "bottom")+scale_color_brewer(palette = "Dark2",name="")+
#   xlab('contraction')+ylab('FN rate')
# ggsave('figures/Avian-Mammalian/mammalian-mean-point-line-contraction-type1X.pdf',width=3.5,height=3.5)
# 
# 
# 


t<-read.csv('data/Avian-Mammalian/avian-0.5X-1000-500-speed-compare.csv',sep="\t",header=F)
t$V2<-factor(t$V2,levels=c("ori","t0","t3","t5","t7","t10","t20", "t33", "t50","t75"))
levels(t$V2)<-list("non"="ori","0"="t0","3"="t3","5"="t5","7"="t7",
                                 "10"="t10","20"="t20","33"="t33","50"="t50","75"="t75")

levels(t$V3)<-list("ASTRALIII"="log3","ASTRALII"="log2")
t$V4<-as.numeric(as.character(t$V4))
ggplot(data=t,aes(x=V2,y=V4,group=V3,color=V3))+stat_summary(fun.y="mean",geom="line")+
  stat_summary(geom="point",fun.y="mean")+
  stat_summary(geom="errorbar",fun.ymin=function(x) {mean(x)-sd(x)/sqrt(length(x))},
               fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.4)+
  xlab("contraction")+ylab("Time (seconds)")+
  theme_bw()+scale_color_brewer(palette = "Set1",name="")+
  theme(legend.position  = c(0.4,0.1),legend.direction = "horizontal")
ggsave('figures/Avian-Mammalian/avian-0.5X-1000-500-time.pdf',width=sizes[1],height = sizes[2])
