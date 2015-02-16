


setwd("/home/tim/git/InequalityAudit/InequalityAudit")

Results <- local(get(load("Data/Results.Rdata")))


k 				<- seq(0,1,length.out = 17)[-1]
library(reshape2)
acast()
qs <- seq(0,1,by=.1)
Limits <- matrix(nrow = length(k),ncol = length(qs),dimnames=list(k,qs))
for (i in 1:length(k)){
	Limits[i,] <- quantile(Results[Results[,"k"] == k[i],"Corr"],prob=qs,na.rm=TRUE)
}

plot(k,Limits[,"0.5"],type='l', ylim=c(.5,1))
lines(k,Limits[,"0"])
lines(k,Limits[,"1"])
polygon(c(k,rev(k)),c(Limits[,"0.1"],rev(Limits[,"0.9"])),col="#00000020",border=NA)
polygon(c(k,rev(k)),c(Limits[,"0.2"],rev(Limits[,"0.8"])),col="#00000020",border=NA)
polygon(c(k,rev(k)),c(Limits[,"0.3"],rev(Limits[,"0.7"])),col="#00000020",border=NA)
polygon(c(k,rev(k)),c(Limits[,"0.4"],rev(Limits[,"0.6"])),col="#00000020",border=NA)



LimitsA <- matrix(nrow = length(k),ncol = length(qs),dimnames=list(k,qs))
for (i in 1:length(k)){
	LimitsA[i,] <- quantile(Results[Results[,"k"] == k[i],"m_avg"],prob=qs,na.rm=TRUE)
}

plot(k,LimitsA[,"0.5"],type='l', ylim=c(0,100))
lines(k,LimitsA[,"0"])
lines(k,LimitsA[,"1"])
polygon(c(k,rev(k)),c(LimitsA[,"0.1"],rev(LimitsA[,"0.9"])),col="#00000020",border=NA)
polygon(c(k,rev(k)),c(LimitsA[,"0.2"],rev(LimitsA[,"0.8"])),col="#00000020",border=NA)
polygon(c(k,rev(k)),c(LimitsA[,"0.3"],rev(LimitsA[,"0.7"])),col="#00000020",border=NA)
polygon(c(k,rev(k)),c(LimitsA[,"0.4"],rev(LimitsA[,"0.6"])),col="#00000020",border=NA)

head(DHS)

