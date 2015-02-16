setwd("/home/tim/git/InequalityAudit/InequalityAudit")

Results <- local(get(load("Data/Results.Rdata")))

dim(Results)
head(Results)
source("R/TernaryTest.R")

rownames(Results) <- NULL
Results          <-  data.frame(Results)
Results$col      <- val2col(Results$Corr,n=100)
w_all 			 <- getTernTriangles(n=50)
head(Results)

plotk <- function(Results, w_all, .k){
	w_all$col <- Results$col[Results$k == .k]
	par(mai=c(.2,.2,.2,.2), xaxs = "i", yaxs = "i", xpd = TRUE)
	plot(c(-.05,1.05,.5),c(0,0,sqrt(3)/2),type="n", axes = FALSE, 
			xlab = "", ylab = "", asp = 1, cex = 1.7)
	plotTernPolygons(w_all)
	plotTernBorder(border = gray(.7))
	plotTernLabels(labs=c("E","H","S"))
	text(.1,sqrt(3)/2,.k,cex=1.7)
}
graphics.off()
#dev.new(width=6,height=7)
getwd()
pdf("Figures/PanelTest_Corr1.pdf")
pp <- layout(mat=matrix(c(1:16,17,17,17,17), 5, 4, byrow=TRUE), widths =  c(1.5,1.5,1.5,1.5), c(1.5,1.5,1.5,1.5,1), T)
#layout.show(pp)
par(xaxs="i",yaxs="i",oma=c(0,1,0,0))
k 				<- seq(0,1,length.out = 17)[-1]

for (.k in k){
	plotk(Results, w_all, .k)
}
plot(NULL, type="n", xlim = c(0,1),ylim=c(0,1))
text(.5,.5,"Legend goes here")
dev.off()

dev.new(width=1.5,height=9)

pp <- layout(mat=matrix(c(1:9), 9,1, byrow=TRUE), widths =  c(rep(1,9)), rep(.9,9), T)
#layout.show(pp)
par(xaxs="i",yaxs="i",oma=c(0,1,0,0))
k 				<- seq(0,1,length.out = 17)[-1]

for (.k in k[1:9]){
	plotk(Results, w_all, .k)
}


#####################################################
# centile plots

