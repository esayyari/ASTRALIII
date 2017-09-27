require('ggplot2')
require('scales')
require('Hmisc')
d<-read.csv('data/Avian-Mammalian/exp2result_formated.csv',sep="\t",header=T)
d$contraction.rate<-factor(d$contraction.rate,levels=c("ori","t0","t3","t5","t7","t10","t20", "t33", "t50","t75"))
levels(d$contraction.rate)<-list("non"="ori","0"="t0","3"="t3","5"="t5","7"="t7",
                                 "10"="t10","20"="t20","33"="t33","50"="t50","75"="t75")




d$seq.length<-factor(d$seq.length)

summary(aov(Error.Rate~contraction.rate*input.type*seq.length,
            data=d[d$contraction.rate%in% c("0","non"),]))
summary(aov(Error.Rate~contraction.rate*input.type*seq.length,
            data=d[d$contraction.rate%in% c("3","non"),]))
summary(aov(Error.Rate~contraction.rate*input.type*seq.length,
            data=d[d$contraction.rate%in% c("5","non"),]))









d<-read.csv('data/ASTRALIII/species.comparison.results.csv', sep=" ",header=F)
d$V1<-as.numeric(as.character(d$V1))
p<-read.csv('data/ASTRALIII/parameter.log.info',sep=" ",header=T)
g<-read.csv('data/ASTRALIII/gtError.csv',sep=" ",header=F)


tr<-read.csv('data/ASTRALIII/truecompare.csv',sep=" ",header=F)
trT<-dcast(data=tr,V3~.,fun.aggregate=mean,value.var="V7")
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

poT<-dcast(data=po[po$V4 == 1000,],V1+V2+V3~.,fun.aggregate=mean, value.var="poly")
names(poT)[4]<-"poly"
gT<-merge(x=d,y=gT,by.x=c("V1","V3"),by.y=c("V1","V2"))

k<-merge(x=p,y=gT,by.x="Replicate",by.y="V1")

k$V4<-factor(k$V4,levels=c("non","0","3","5","7","10","20","33","50","75"))


k$meanGtErrorbin<-cut(k$rf,breaks=c(0,1/4,1/3,1/2,1),labels=c("very low (<25%)","low (<33%)","high (<50%)","very high (<100%)"))
#k$meanGtErrorbin<-cut(k$rf,breaks=c(min(k$rf)-0.0001,quantile(k$rf)[2:5]),labels=c("very low gt err","low gt err","high gt err","very high gt err"))

k$AL=as.factor(k$V3)

summary(aov(V8~V4*(V3+V5),
            data=d[d$V4%in% c("0","non") & d$V2 == "ASTRAL.5.2.5",]))
summary(aov(V8~V4*(V3+V5),
            data=d[d$V4%in% c("3","non") & d$V2 == "ASTRAL.5.2.5",]))
summary(aov(V8~V4*(V3+V5),
            data=d[d$V4%in% c("5","non") & d$V2 == "ASTRAL.5.2.5",]))
