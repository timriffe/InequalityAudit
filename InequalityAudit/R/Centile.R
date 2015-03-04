


setwd("/home/tim/git/InequalityAudit/InequalityAudit")

Results <- local(get(load("/home/tim/Dropbox/Graficos (1)/Results.Rdata")))
Results2 <- local(get(load("/home/tim/Dropbox/Graficos (1)/Results2.Rdata")))


k <- sort(unique(Results[,"k"]))


plot.centile <- function(Res, k, qs,variable = "Corr", ylim = NULL){
	Limits <- matrix(nrow = length(k),ncol = length(qs),dimnames=list(k,qs))
	for (i in 1:length(k)){
		Limits[i,] <- quantile(Res[Res[,"k"] == k[i],variable],prob=qs,na.rm=TRUE)
	}
	if (is.null(ylim)){
		ylim <- range(pretty(Limits))
	}
	plot(k,Limits[,"0.5"],type='l', ylim=ylim, xlab = "k", ylab = "Corr", main = "CC_Poor Correlation")
	lines(k,Limits[,"0"],col=gray(.5))
	lines(k,Limits[,"1"],col=gray(.5))
	polygon(c(k,rev(k)),c(Limits[,"0.1"],rev(Limits[,"0.9"])),col="#00000020",border=NA)
	polygon(c(k,rev(k)),c(Limits[,"0.2"],rev(Limits[,"0.8"])),col="#00000020",border=NA)
	polygon(c(k,rev(k)),c(Limits[,"0.3"],rev(Limits[,"0.7"])),col="#00000020",border=NA)
	polygon(c(k,rev(k)),c(Limits[,"0.4"],rev(Limits[,"0.6"])),col="#00000020",border=NA)
}
pdf("/home/tim/Dropbox/Graficos (1)/CentileCCpoorCorr.pdf")
plot.centile(Results, k , qs, "Corr",ylim=c(.6,1))
dev.off()
pdf("/home/tim/Dropbox/Graficos (1)/CentileCCpoorm_avg.pdf")
plot.centile(Results, k , qs, "m_avg", ylim = c(0,80))
dev.off()
pdf("/home/tim/Dropbox/Graficos (1)/CentileCCpoor2Corr.pdf")
plot.centile(Results2, k , qs, "Corr",ylim=c(.6,1))
dev.off()
pdf("/home/tim/Dropbox/Graficos (1)/CentileCCpoor2m_avg.pdf")
plot.centile(Results2, k , qs, "m_avg",ylim=c(0,80))
dev.off()
qs <- seq(0,1,by=.1)
Limits <- matrix(nrow = length(k),ncol = length(qs),dimnames=list(k,qs))
for (i in 1:length(k)){
	Limits[i,] <- quantile(Results[Results[,"k"] == k[i],"Corr"],prob=qs,na.rm=TRUE)
}

pdf("/home/tim/Dropbox/Graficos (1)/CC_Poor_Centile.pdf")
plot(k,Limits[,"0.5"],type='l', ylim=c(.5,1), xlab = "k", ylab = "Corr", main = "CC_Poor Correlation")
lines(k,Limits[,"0"],col=gray(.5))
lines(k,Limits[,"1"],col=gray(.5))
polygon(c(k,rev(k)),c(Limits[,"0.1"],rev(Limits[,"0.9"])),col="#00000020",border=NA)
polygon(c(k,rev(k)),c(Limits[,"0.2"],rev(Limits[,"0.8"])),col="#00000020",border=NA)
polygon(c(k,rev(k)),c(Limits[,"0.3"],rev(Limits[,"0.7"])),col="#00000020",border=NA)
polygon(c(k,rev(k)),c(Limits[,"0.4"],rev(Limits[,"0.6"])),col="#00000020",border=NA)
dev.off()


LimitsA <- matrix(nrow = length(k),ncol = length(qs),dimnames=list(k,qs))
for (i in 1:length(k)){
	LimitsA[i,] <- quantile(Results[Results[,"k"] == k[i],"m_avg"],prob=qs,na.rm=TRUE)
}

plot(k,LimitsA[,"0.5"],type='l', ylim=c(0,100))
lines(k,LimitsA[,"0"],col=gray(.5))
lines(k,LimitsA[,"1"],col=gray(.5))
polygon(c(k,rev(k)),c(LimitsA[,"0.1"],rev(LimitsA[,"0.9"])),col="#00000020",border=NA)
polygon(c(k,rev(k)),c(LimitsA[,"0.2"],rev(LimitsA[,"0.8"])),col="#00000020",border=NA)
polygon(c(k,rev(k)),c(LimitsA[,"0.3"],rev(LimitsA[,"0.7"])),col="#00000020",border=NA)
polygon(c(k,rev(k)),c(LimitsA[,"0.4"],rev(LimitsA[,"0.6"])),col="#00000020",border=NA)

head(DHS)

