setwd("/home/tim/git/InequalityAudit/InequalityAudit")

Results <- local(get(load("/home/tim/Dropbox/Graficos (1)/Results.Rdata")))
Results2 <- local(get(load("/home/tim/Dropbox/Graficos (1)/Results2.Rdata")))

# cut down to just needed elements of k.
allk <- sort(unique(Results[,"k"]))

pk <- allk[c(20,40,60,80,100)]

source("R/TernaryTest.R")

rownames(Results) <- NULL
rownames(Results2) <- NULL

Results  <- Results[Results[,"k"] %in% pk, ]
Results2 <- Results2[Results2[,"k"] %in% pk, ]


colbrks <- function(x,breaks,b.pal="YlGnBu"){
	colRamp <- colorRampPalette(RColorBrewer::brewer.pal(9,b.pal),space="Lab")
	n <- length(breaks) - 1
	as.character(cut(
					x, 
					breaks = breaks, 
					labels = colRamp(n),
					include.lowest = TRUE))
}

Ncolors <- 100
breaks <- seq(.6,1,length.out=Ncolors+1)

Results          <- data.frame(Results)
Results2          <- data.frame(Results2)
Results$col      <- colbrks(Results$Corr,breaks=breaks,b.pal="YlOrRd")
Results2$col      <- colbrks(Results2$Corr,breaks=breaks,b.pal="YlOrRd")

w_all 			 <- getTernTriangles(n=50)


plotk <- function(Res, w_all, .k){
	w_all$col <- Res$col[Res$k == .k]
	par(mai=c(.15,.15,.15,.15), xaxs = "i", yaxs = "i", xpd = TRUE)
	plot(c(0,1,.5),c(0,0,sqrt(3)/2),type="n", axes = FALSE, 
			xlab = "", ylab = "", asp = 1, cex = 1.7)
	plotTernPolygons(w_all)
	plotTernBorder(border = gray(.7))
	plotTernLabels(labs=c("E","H","S"))
	
	#text(.1,sqrt(3)/2,.k,cex=1.7)
}
graphics.off()
#

#pdf("Figures/PanelCorr.pdf",width=15/2.54,height=21/2.54)
pdf("/home/tim/Dropbox/Graficos (1)/PanelCorr.pdf",width=15/2.54,height=21/2.54)
#pdf("Figures/PanelTest_Corr1.pdf")
# 21 * 29.7
pp <- layout(mat=matrix(c(1,2,2,2,2,2,1,3,4,5,6,7,1,8,9,10,11,12,1,13,13,13,13,13), 6, 4, byrow=FALSE), 
		widths =  c(.5,1.5,1.5,1), rep(.5,1.2,1.2,1.2,1.2,1.2),FALSE)
#layout.show(pp)
par(xaxs="i",yaxs="i",oma=c(0,0,0,0), mar=c(0,0,0,0))
plot(NULL, type= "n", xlim=c(0,1),ylim=c(0,1), axes = FALSE, xlab="",ylab="")
text(c(.25,.6),c(.5,.5),c("CC Poor", "CC Poor II"),cex=1.5)


plot(NULL, type= "n", xlim=c(0,1),ylim=c(0,5), axes = FALSE, xlab="",ylab="")
text(.5,0:4+.7,paste0("k = ",rev(pk)),cex=1.5)

for (.k in pk){
	plotk(Results, w_all, .k)
}
for (.k in pk){
	plotk(Results2, w_all, .k)
}
#plotTernAxes(5,tl=.03, labels=TRUE,cex=.8,off=.08)

legbrks <- seq(.6,1,by=.05)
coldec <- legbrks[-length(legbrks)] + diff(legbrks) /2
legcols <- colbrks(coldec,breaks=legbrks,b.pal="YlOrRd")


plot(NULL, type="n", xlim = c(0,1),ylim=c(0,1), xlab = "", ylab = "", axes = FALSE)
yat <- seq(.3,.7,length=9)
xat <- c(.2,.5)
x <- seq(0, 1.0, 0.1); y <- c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)
cbind(x, sprintf("%a", x), sprintf("%a", y))
rect(xat[1],yat[-9],xat[2],yat[-1],col = legcols,border=NA)
rect(xat[1],min(yat),xat[2],max(yat),border=gray(.4))
text(xat[2],yat,sprintf("%.2f",legbrks),pos=4)

dev.off()
#################################################################################3

##################################################################################
range(c(Results[,"m_avg"],Results2[,"m_avg"]))
breaks <- seq(0,60,length.out=Ncolors+1)


Results$col      <- colbrks(Results$m_avg,breaks=breaks,b.pal="YlOrRd")
Results2$col      <- colbrks(Results2$m_avg,breaks=breaks,b.pal="YlOrRd")
graphics.off()

pdf("/home/tim/Dropbox/Graficos (1)/Panelm_avg.pdf",width=15/2.54,height=21/2.54)
#pdf("Figures/PanelTest_Corr1.pdf")
# 21 * 29.7
pp <- layout(mat=matrix(c(1,2,2,2,2,2,1,3,4,5,6,7,1,8,9,10,11,12,1,13,13,13,13,13), 6, 4, byrow=FALSE), 
		widths =  c(.5,1.5,1.5,1), rep(.5,1.2,1.2,1.2,1.2,1.2),FALSE)
#layout.show(pp)
par(xaxs="i",yaxs="i",oma=c(0,0,0,0), mar=c(0,0,0,0))
plot(NULL, type= "n", xlim=c(0,1),ylim=c(0,1), axes = FALSE, xlab="",ylab="")
text(c(.25,.6),c(.5,.5),c("CC Poor", "CC Poor II"),cex=1.5)


plot(NULL, type= "n", xlim=c(0,1),ylim=c(0,5), axes = FALSE, xlab="",ylab="")
text(.5,0:4+.7,paste0("k = ",rev(pk)),cex=1.5)

for (.k in pk){
	plotk(Results, w_all, .k)
}
for (.k in pk){
	plotk(Results2, w_all, .k)
}
#plotTernAxes(5,tl=.03, labels=TRUE,cex=.8,off=.08)

legbrks <- seq(0,60,by=10)
coldec <- legbrks[-length(legbrks)] + diff(legbrks) /2
legcols <- colbrks(coldec,breaks=legbrks,b.pal="YlOrRd")


plot(NULL, type="n", xlim = c(0,1),ylim=c(0,1), xlab = "", ylab = "", axes = FALSE)
yat <- seq(.3,.7,length=7)
xat <- c(.2,.5)

rect(xat[1],yat[-7],xat[2],yat[-1],col = legcols,border=NA)
rect(xat[1],min(yat),xat[2],max(yat),border=gray(.4))
text(xat[2],yat,sprintf("%.0f",legbrks),pos=4)

dev.off()