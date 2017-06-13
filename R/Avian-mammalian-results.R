require(ggplot2)
require(reshape2)
require(scales)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

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


time<-read.csv('data/Avian-Mammalian/different_k_result.csv',sep="\t",header=F)

time$V4<-as.numeric(as.character(time$V4))
time$V6m=time$V6/60

ggplot(data=time[time$V1 == "1X" & time$V6<150000,],aes(x=V4,y=V6m,color=V5,group=V5))+
  theme_bw()+scale_color_brewer(palette = "Set1",name="") + 
  scale_y_continuous(trans = log2_trans(),breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  scale_x_continuous(trans ='log2',breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  theme(legend.position = c(0.85,0.2))+
  facet_wrap(~V2)+geom_smooth(method="lm")+ylab("Running time (minutes)")+stat_summary(group=1,fun.y="mean",geom="point")+xlab("#Genes")
ggsave('figures/Avian-Mammalian/avia-1000-500-log-log-time.pdf',width=7.47,height=4.33)

ggplot(data=time[time$V1 == "1X" & time$V6<150000,],aes(x=V4,y=V6m,color=V5,group=V5))+
  theme_bw()+scale_color_brewer(palette = "Set1",name="") + 
  #scale_y_continuous(trans = log2_trans(),breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  #scale_x_continuous(trans ='log2',breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  theme(legend.position = c(0.15,0.8))+
  facet_wrap(~V2)+geom_smooth(se=F)+ylab("Running time (minutes)")+stat_summary(group=1,fun.y="mean",geom="point")+xlab("#Genes")
ggsave('figures/Avian-Mammalian/avia-1000-500-time.pdf',width=7.47,height=4.33)

pdf("figures/Avian-Mammalian/avia-1000-500-both-scale-time.pdf",width=7.5,height=4)
multiplot(
ggplot(data=time[time$V1 == "1X" & time$V2 == "1500" & time$V6<150000,],aes(x=V4,y=V6m,color=V5,group=V5))+
  theme_bw()+
  scale_color_brewer(palette = "Set1",name="") +
  scale_y_continuous(trans = log2_trans(),breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  scale_x_continuous(trans ='log2',breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  theme(legend.position = c(0.75,0.2))+
  facet_wrap(~V2)+
  geom_smooth(se=F,method="lm")+
  ylab("Running time (minutes)")+stat_summary(group=1,fun.y="mean",geom="point")+xlab("#Genes")+
  annotate(x=2^9,y=2.5,geom="text",label=format(lm(log(V6m)~log(V4),data=time[time$V2=="1500"&time$V5=="ASTRAL2",])$coefficients[[2]],digits=3),color="tomato")+
  annotate(x=2^11,y=5,geom="text",label=format(lm(log(V6m)~log(V4),data=time[time$V2=="1500"&time$V5=="ASTRAL3",])$coefficients[[2]],digits=3),color="steelblue")
,
ggplot(data=time[time$V1 == "1X" & time$V2 == "1500" & time$V6<150000,],aes(x=V4,y=V6m,color=V5,group=V5))+
  theme_bw()+
  scale_color_brewer(palette = "Set1",name="") +
  theme(legend.position = "none")+
  scale_x_continuous(breaks=c(2^8,2^11,2^12,2^13,2^14),labels = trans_format("log2", math_format(2^.x)))+
  facet_wrap(~V2)+
  geom_smooth(se=F)+
  ylab("Running time (minutes)")+stat_summary(group=1,fun.y="mean",geom="point")+xlab("#Genes"),
cols=2
)
dev.off()

ggplot(data=time[time$V1 == "1X" & time$V6<150000,],aes(x=V4,y=V6,color=V5,group=V5))+
  theme_bw()+scale_color_brewer(palette = "Set1",name="") + 
  scale_y_continuous(trans = log2_trans(),breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  scale_x_continuous(trans ='log2',breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  theme(legend.position = c(0.85,0.2))+geom_smooth(method="lm")+
  facet_wrap(~V2)+ylab("Running time (seconds)")+xlab("#Genes")
  ggsave('figures/Avian-Mammalian/avia-1000-500-log-log-smooth-time.pdf',width=7.47,height=4.33)
  