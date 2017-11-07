library(snht)
library(reshape2)
library(ggplot2)
library(mvtnorm)

set.seed(2)
Cor<-rbind(c(0.5,0.8,0.8,0.8,0.8),c(0,0.5,0.8,0.5,0.6),
           c(0,0,0.5,0.8,0.5),c(0,0,0,0.5,0.6),c(0,0,0,0,0.5))
Cor<-t(Cor)+Cor
baseData<-rmvnorm(mean=rep(0,5),sig=Cor,n=1000)+cos(1:1000*2*pi/200)
baseData[401:1000,1]<-baseData[401:1000,1]+0.5
dist<-matrix(0,5,5)
dist<-dist(rbind(c(1,0),c(1,1),c(0,0),c(1,-1),c(2,0)))
dist<-as.matrix(dist)
colnames(dist)<-rownames(dist)<-1:5
colnames(baseData) <-"1":"5"
baseData <- data.frame(time = 1:1000, baseData)
baseData <- melt(baseData, id.vars = "time", variable.name = "location",
                 value.name = "data")
baseData$location<-gsub("X","",baseData$location)
out1 <- pairwiseSNHT(baseData, dist, k=3, period=200,
                     crit=qchisq(1-0.05/600,df=1), returnStat=T)
pairs <- colnames(out1)
pairs

out1[300:302, ]

out2 <- pairwiseSNHT(baseData, dist, k=3, period=200,
                     crit=qchisq(1-0.05/600,df=1), returnStat=F)
out2$breaks
str(out2$data)

, the SNHT statistic does no longer exceed the critical value.
newPair2 <- out2$data
outNew1 <- pairwiseSNHT(newPair2,dist,k=3,period=200,
                        crit=qchisq(1-0.05/600,df=1),returnStat=T)
