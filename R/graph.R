library(stats)

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
d<-read.csv('exp2result_formated.csv',sep="\t",header = T)

d$contraction.rate<-factor(d$contraction.rate,levels=c("ori","t0","t3","t5","t7","t10","t20", "t33", "t50","t75"))
levels(d$contraction.rate)<-list("non"="ori","0"="t0","3"="t3","5"="t5","7"="t7",
                                 "10"="t10","20"="t20","33"="t33","50"="t50","75"="t75")

pdf("avian-mean-point-line-contraction-both-paper.pdf",width=7.5,height=4)
multiplot(
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
  ,
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
    theme(legend.position = c(.4,0.91),legend.direction = "horizontal")+scale_color_brewer(palette = "Set1",name=""),
  cols=2
)
dev.off()

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

ggsave('avian-mean-point-line-contraction-seq500-paper.pdf',width=sizes[1],height=sizes[2])

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

ggsave('avian-mean-point-line-contraction-type1X-paper.pdf',width=sizes[1],height=sizes[2])

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

ggsave('avian-mean-point-line-contraction-seq500-ALL-paper.pdf',width=sizes[1],height=sizes[2])

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

ggsave('avian-mean-point-line-contraction-ALL-type1X.pdf',width=sizes[1],height=sizes[2])

time<-read.csv('exp1result_formated.csv',sep="\t",header=F)

time$V4<-as.numeric(as.character(time$V4))
time$V6m=time$V6/60

ggplot(data=time[time$V1 == "1X" & time$V6<150000,],aes(x=V4,y=V6m,color=V5,group=V5))+
  theme_bw()+scale_color_brewer(palette = "Set1",name="") + 
  scale_y_continuous(trans = log2_trans(),breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  scale_x_continuous(trans ='log2',breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  theme(legend.position = c(0.85,0.2))+
  facet_wrap(~V2)+geom_smooth(method="lm")+ylab("Running time (minutes)")+stat_summary(group=1,fun.y="mean",geom="point")+xlab("#Genes")+
  geom_text(data=data.frame(x=2^9,y=3), color="tomato", aes(x,y,label=c(format(lm(log(V6m)~log(V4),data=time[time$V2=="500"&time$V5=="ASTRAL2",])$coefficients[[2]],digits=3), format(lm(log(V6m)~log(V4),data=time[time$V2=="1500"&time$V5=="ASTRAL2",])$coefficients[[2]],digits=3))), inherit.aes=F)+
  geom_text(data=data.frame(x=2^11,y=4.5), color="steelblue", aes(x,y,label=c(format(lm(log(V6m)~log(V4),data=time[time$V2=="500"&time$V5=="ASTRAL3",])$coefficients[[2]],digits=3), format(lm(log(V6m)~log(V4),data=time[time$V2=="1500"&time$V5=="ASTRAL3",])$coefficients[[2]],digits=3))), inherit.aes=F)
ggsave('avian-1000-500-log-log-time.pdf',width=7.47,height=4.33)

ggplot(data=time[time$V1 == "1X" & time$V6<150000,],aes(x=V4,y=V6m,color=V5,group=V5))+
  theme_bw()+scale_color_brewer(palette = "Set1",name="") + 
  #scale_y_continuous(trans = log2_trans(),breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  #scale_x_continuous(trans ='log2',breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  theme(legend.position = c(0.15,0.8))+
  facet_wrap(~V2)+geom_smooth(se=F)+ylab("Running time (minutes)")+stat_summary(group=1,fun.y="mean",geom="point")+xlab("#Genes")
ggsave('avian-1000-500-time.pdf',width=7.47,height=4.33)

pdf("avian-1000-500-both-scale-time.pdf",width=7.5,height=4)
multiplot(
  ggplot(data=time[time$V1 == "1X" & time$V2 == "1500" & time$V6<150000,],aes(x=V4,y=V6m,color=V5,group=V5))+
    theme_bw()+
    scale_color_brewer(palette = "Set1",name="",labels=c("ASTRAL-II","ASTRAL-III")) +
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
  facet_wrap(~V2)+ylab("Running time (seconds)")+xlab("#Genes")+
  geom_text(data=data.frame(x=2^9,y=2^8), color="tomato", aes(x,y,label=c(format(lm(log(V6m)~log(V4),data=time[time$V2=="500"&time$V5=="ASTRAL2",])$coefficients[[2]],digits=3), format(lm(log(V6m)~log(V4),data=time[time$V2=="1500"&time$V5=="ASTRAL2",])$coefficients[[2]],digits=3))), inherit.aes=F)+
  geom_text(data=data.frame(x=2^11,y=2^7.5), color="steelblue", aes(x,y,label=c(format(lm(log(V6m)~log(V4),data=time[time$V2=="500"&time$V5=="ASTRAL3",])$coefficients[[2]],digits=3), format(lm(log(V6m)~log(V4),data=time[time$V2=="1500"&time$V5=="ASTRAL3",])$coefficients[[2]],digits=3))), inherit.aes=F)
ggsave('avian-1000-500-log-log-smooth-time.pdf',width=7.47,height=4.33)

setx<-read.csv('clustervsk.csv',sep="\t",header=F)

setx$V4<-as.numeric(as.character(setx$V4))

ggplot(data=setx[setx$V1 == "1X",],aes(x=V4,y=V6,color=V5,group=V5))+
  theme_bw()+scale_color_brewer(palette = "Set1",name="") + 
  scale_y_continuous(trans = log2_trans(),breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  scale_x_continuous(trans ='log2',breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  theme(legend.position = c(0.85,0.2))+
  facet_wrap(~V2)+geom_smooth(method="lm")+ylab("size of X")+stat_summary(group=1,fun.y="mean",geom="point")+xlab("#Genes")+
  geom_text(data=data.frame(x=2^9.5,y=2^15), color="tomato", aes(x,y,label=c(format(lm(log(V6m)~log(V4),data=time[time$V2=="500"&time$V5=="ASTRAL2",])$coefficients[[2]],digits=3), format(lm(log(V6m)~log(V4),data=time[time$V2=="1500"&time$V5=="ASTRAL2",])$coefficients[[2]],digits=3))), inherit.aes=F)+
  geom_text(data=data.frame(x=2^11.5,y=2^14.5), color="steelblue", aes(x,y,label=c(format(lm(log(V6m)~log(V4),data=time[time$V2=="500"&time$V5=="ASTRAL3",])$coefficients[[2]],digits=3), format(lm(log(V6m)~log(V4),data=time[time$V2=="1500"&time$V5=="ASTRAL3",])$coefficients[[2]],digits=3))), inherit.aes=F)
ggsave('avian-1000-500-log-log-setx.pdf',width=7.47,height=4.33)

ggplot(data=setx[setx$V1 == "1X",],aes(x=V4,y=V6,color=V5,group=V5))+
  theme_bw()+scale_color_brewer(palette = "Set1",name="") + 
  #scale_y_continuous(trans = log2_trans(),breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  #scale_x_continuous(trans ='log2',breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
  theme(legend.position = c(0.15,0.8))+
  facet_wrap(~V2)+geom_smooth(se=F)+ylab("size of X")+stat_summary(group=1,fun.y="mean",geom="point")+xlab("#Genes")
ggsave('avian-1000-500-setx.pdf',width=7.47,height=4.33)

pdf("avian-1000-500-both-scale-setx.pdf",width=7.5,height=4)
multiplot(
  ggplot(data=setx[setx$V1 == "1X" & setx$V2 == "1500",],aes(x=V4,y=V6,color=V5,group=V5))+
    theme_bw()+
    scale_color_brewer(palette = "Set1",name="",labels=c("ASTRAL-II","ASTRAL-III")) +
    scale_y_continuous(trans = log2_trans(),breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
    scale_x_continuous(trans ='log2',breaks = trans_breaks("log2", function(x) 2^x),labels = trans_format("log2", math_format(2^.x)))+
    theme(legend.position = c(0.75,0.2))+
    facet_wrap(~V2)+
    geom_smooth(se=F,method="lm")+
    ylab("size of X")+stat_summary(group=1,fun.y="mean",geom="point")+xlab("#Genes")+
    annotate(x=2^9,y=2^14.5,geom="text",label=format(lm(log(V6)~log(V4),data=setx[setx$V2=="1500"&setx$V5=="ASTRAL2",])$coefficients[[2]],digits=3),color="tomato")+
    annotate(x=2^10,y=2^13.5,geom="text",label=format(lm(log(V6)~log(V4),data=setx[setx$V2=="1500"&setx$V5=="ASTRAL3",])$coefficients[[2]],digits=3),color="steelblue")
  ,
  ggplot(data=setx[setx$V1 == "1X" & setx$V2 == "1500",],aes(x=V4,y=V6,color=V5,group=V5))+
    theme_bw()+
    scale_color_brewer(palette = "Set1",name="") +
    theme(legend.position = "none")+
    scale_x_continuous(breaks=c(2^8,2^11,2^12,2^13,2^14),labels = trans_format("log2", math_format(2^.x)))+
    facet_wrap(~V2)+
    geom_smooth(se=F)+
    ylab("size of X")+stat_summary(group=1,fun.y="mean",geom="point")+xlab("#Genes"),
  cols=2
)
dev.off()
