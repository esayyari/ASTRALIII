require(ggplot2)
require(reshape2)
require(scales)
require(Hmisc)



d<-read.csv('data/ASTRALIII/latest/compare.latest.csv', sep=" ",header=F)
d$V5<-factor(d$V5,levels=c("non","0","3","5","7","10","20","33","50","75"))
d$V3<-factor(d$V3,levels=c("200","400","800","1600"))
levels(d$V3)<-list("200bp"="200","400bp"="400","800bp"="800","1600bp"="1600")

h<-dcast(d[d$V4 == "estimated" & d$V5 == "non",],V1+V3+V6+V5~V2, value.var = c("V9"))
h$diff<-(h$ASTRAL.5.5.4-h$ASTRAL.4.11.2)



w<-read.csv('data/ASTRALIII/latest/setx.latest.csv',sep=" ",header=F)
w$V4<-factor(w$V4,levels=c("non","0","3","5","7","10","20","33","50","75"))
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

stderr<-function(x){sd(x)/sqrt(length(x))}
finalt1<-cbind(dcast(data=hsum,V3+V6~.,fun.aggregate=mean,value.var = "FN"),
               dcast(data=hsum,V3+V6~.,fun.aggregate=stderr,value.var = "FN"))[,c(1,2,3,6)]

finalt1$FN<-paste(format.df(finalt1$.,dec=3),format.df(finalt1$..1,dec=3),sep=" (")
finalt1$FN<-paste(finalt1$FN,")",sep="")

finalt2<-cbind(dcast(data=hsum,V3+V6~.,fun.aggregate=mean,value.var = "SpaceX"),
               dcast(data=hsum,V3+V6~.,fun.aggregate=stderr,value.var = "SpaceX"))[,c(1,2,3,6)]
finalt2$SpaceX<-paste(format.df(finalt2$.,dec=0),"%",sep="")
finalt2$SpaceX<-paste(finalt2$SpaceX,format.df(finalt2$..1,dec=0),sep=" (")
finalt2$SpaceX<-paste(finalt2$SpaceX,")",sep="")


finalt3<-cbind(dcast(data=hsum,V3+V6~.,fun.aggregate=mean,value.var = "quartetScore"),
               dcast(data=hsum,V3+V6~.,fun.aggregate=stderr,value.var = "quartetScore"))[,c(1,2,3,6)]
finalt3$quartetScore<-paste(format.df(finalt3$.,dec=2),"%",sep="")
finalt3$quartetScore<-paste(finalt3$quartetScore,format.df(finalt3$..1,dec=2),sep=" (")
finalt3$quartetScore<-paste(finalt3$quartetScore,")",sep="")

finalt<-merge(x=finalt1,y=finalt2,by.x=c("V3","V6"),by.y=c("V3","V6"))
finalt<-merge(x=finalt,y=finalt3,by.x=c("V3","V6"),by.y=c("V3","V6"))
finalt<-finalt[,c(1,2,5,8,11)]
finalt$model<-paste(finalt$V6,finalt$V3,sep="-")
finalt<-finalt[,c(6,3,4,5)]
finalt$dataset<-"ASTRALIII"
astral3<-finalt[,c(5,1,2,3,4)]


########### ASTRALII-maryam
d<-read.csv('data/ASTRALII/FN.all',sep=" ",header=F)
d<-d[d$V4=="original",]
d<-d[!d$V5 %in% c(8,49,15),]
h1<-dcast(data=d,V1+V3+V4+V5~V2,value.var="V6")
h1$diff <- h1$`5.5.4`-h1$`4.11.2`

w<-read.csv('data/ASTRALII/setx.all',sep=" ",header=F)
w<-w[w$V4=="original",]
w<-w[!w$V5 %in% c(8,49,15),]
h2<-dcast(data=w,V1+V3+V4+V5~V2,value.var="V6")

h2$diff <- (h2$`5.5.4`-h2$`4.11.2`)/h2$`4.11.2`*100


q<-read.csv('data/ASTRALII/qstat.all',sep=" ",header=F)
q<-q[!q$V5 %in% c(8,49,15),]

q<-q[q$V4=="original",]
h3<-dcast(data=q,V1+V3+V4+V5~V2,value.var="V6")
h3$diff <- (h3$`5.5.4`-h3$`4.11.2`)/h3$`4.11.2`*100

hsum<-merge(x=h1,y=h2,by.x=c("V1","V3","V4","V5"),by.y=c("V1","V3","V4","V5"))
hsum<-merge(x=hsum,y=h3,by.x=c("V1","V3","V4","V5"),by.y=c("V1","V3","V4","V5"))
hsum<-hsum[,c(1,2,4,7,10,13)]
names(hsum)<-c("V1","V3","V5","FN","SpaceX","quartetScore")
hsum$model<-paste(hsum$V1,hsum$V3,sep="-")

p<-cbind(dcast(hsum,model~.,value.var="FN",fun.aggregate=mean),dcast(hsum,model~.,value.var="FN",fun.aggregate=stderr))[,c(1,2,4)]
p2<-cbind(dcast(hsum,model~.,value.var="SpaceX",fun.aggregate=mean),dcast(hsum,model~.,value.var="SpaceX",fun.aggregate=stderr))[,c(1,2,4)]
p3<-cbind(dcast(hsum,model~.,value.var="quartetScore",fun.aggregate=mean),dcast(hsum,model~.,value.var="quartetScore",fun.aggregate=stderr))[,c(1,2,4)]

p$final<-paste(format.df(p$.,dec=3),format.df(p$..1,dec=3),sep=" (")
p$final2<-paste(p$final,")",sep="")



p2$final<-paste(format.df(p2$.,dec=0),"%",sep="")
p2$final<-paste(p2$final,format.df(p2$..1,dec=0),sep=" (")
p2$final2<-paste(p2$final,")",sep="")

p3$final<-paste(format.df(p3$.,dec=2),"%",sep="")
p3$final<-paste(p3$final,format.df(p3$..1,dec=2),sep=" (")
p3$final2<-paste(p3$final,")",sep="")


astral2<-data.frame(cbind(p$model,p$final2,p2$final2,p3$final2))
astral2$dataset<-"ASTRALII"
names(astral2)<-c("model","FN","SpaceX","quartetScore","dataset")

astral2<-astral2[,c(5,1,2,3,4)]


######## chao


d2<-read.csv('data/Avian-Mammalian/avian-comparison.csv',sep="\t",header=T)
d2$FN<-d2$fn3-d2$fn2
d2$SpaceX<-(d2$cluster3-d2$cluster2)/d2$cluster2*100
d2$quartetScore<-(d2$score3 - d2$score2)/d2$score2*100
d2$dataset<-"Avian"
d2$model<-paste(d2$distance,d2$length,sep="-")
d<-read.csv('data/Avian-Mammalian/avian-0_5X-1000-500_comparison.csv',sep="\t",header=T)

d<-d[d$threshold=="ori",]
d$FN<-d$new.FN-d$old.FN
d$quartetScore <- (d$new.score-d$old.score)/d$old.score*100
d$SpaceX<-(d$new.set.X-d$old.set.X)/d$old.set.X*100

d$dataset<-"Avian"
d$model<-"0.5X-1000-500"
d2$dataset<-"Avian"
d<-d[,c(2,15,14,11,12,13)]
d2<-d2[,c(3,17,16,13,15,14)]
d<-rbind(d,d2)
h1<-cbind(dcast(d,model+dataset~.,fun.aggregate = mean,value.var = "FN"),dcast(d,model+dataset~.,fun.aggregate = stderr,value.var = "FN"))[,c(1,2,3,6)]
h3<-cbind(dcast(d,model+dataset~.,fun.aggregate = mean,value.var = "SpaceX"),dcast(d,model+dataset~.,fun.aggregate = stderr,value.var = "SpaceX"))[,c(1,2,3,6)]

h1$val<-paste(format.df(h1$.,dec=3),format.df(h1$..1,dec=3),sep=" (")
h1$val<-paste(h1$val,")",sep="")

h2<-cbind(dcast(d,model+dataset~.,fun.aggregate = mean,value.var = "quartetScore"),dcast(d,model+dataset~.,fun.aggregate = stderr,value.var = "quartetScore"))[,c(1,2,3,6)]
h2$val<-paste(format.df(h2$.,dec=2),"%",sep="")
h2$val<-paste(h2$val,format.df(h2$..1,dec=2),sep=" (")
h2$val<-paste(h2$val,")",sep="")


h3<-cbind(dcast(d,model+dataset~.,fun.aggregate = mean,value.var = "SpaceX"),dcast(d,model+dataset~.,fun.aggregate = stderr,value.var = "SpaceX"))[,c(1,2,3,6)]
h3$val<-paste(format.df(h3$.,dec=0),"%",sep="")
h3$val<-paste(h3$val,format.df(h3$..1,dec=0),sep=" (")
h3$val<-paste(h3$val,")",sep="")

avian<-data.frame(cbind(h1,h3,h2))
avian<-avian[,c(2,1,5,10,15)]
names(avian)<-c("dataset","model","FN","SpaceX","quartetScore")

alltogether<-rbind(avian,astral2,astral3)
latex(alltogether[,c(2,3,4,5)],file="figures/table-comparison.tex",rowname="",rowlabel="",rgroup = c("Avian","S200","S100"),n.rgroup=c(7,6,16))

######### tests


d<-read.csv('data/ASTRALIII/latest/compare.latest.csv', sep=" ",header=F)
d$V5<-factor(d$V5,levels=c("non","0","3","5","7","10","20","33","50","75"))
d$V3<-factor(d$V3,levels=c("200","400","800","1600"))
levels(d$V3)<-list("200bp"="200","400bp"="400","800bp"="800","1600bp"="1600")

#astral3<-dcast(d[d$V4 == "estimated" & d$V5 == "non",],V1+V3+V6+V5~V2, value.var = c("V9"))
#astral3$diff<-(h$ASTRAL.5.5.4-h$ASTRAL.4.11.2)
astral3<-d
astral3$dataset<-"S100"
astral3$model<-paste(astral3$V3,astral3$V6,sep="-")
astral3<-astral3[astral3$V4=="estimated" & astral3$V5=="non",]
d<-read.csv('data/ASTRALII/FN.all',sep=" ",header=F)
d<-d[d$V4=="original",]
d<-d[!d$V5 %in% c(8,49,15),]
astral2<-d
#astral2<-dcast(data=d,V1+V3+V4+V5~V2,value.var="V6")
#astral2$diff <- astral2$`5.5.4`-astral2$`4.11.2`
astral2$dataset<-"S200"
astral2$model<-paste(astral2$V3,astral2$V1,sep="-")

d2<-read.csv('data/Avian-Mammalian/avian-comparison.csv',sep="\t",header=T)
d2$FN<-d2$fn3-d2$fn2
d2$SpaceX<-(d2$cluster3-d2$cluster2)/d2$cluster2*100
d2$quartetScore<-(d2$score3 - d2$score2)/d2$score2*100
d2$dataset<-"avian"
d2$model<-paste(d2$distance,d2$length,sep="-")
avian<-d2
avian$model<-paste(avian$distance,avian$length,sep='-')

astral3<-astral3[,c(11,10,1,)]


