

setwd("/home/tim/Dropbox/Graficos (1)")
# work computer:
#setwd("/hdir/0/triffe/COMMONS/Dropbox/Graficos (1)")
DHS <- read.csv("DHS_Triangles.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)


#head(DHS)
#dim(DHS)
#
#w <- c(1,1,1)/3
#k <- 1/3
#names(w) <- c("E","H","S")
#expandw <- function(w){
#	reps <- c(2,2,6)
#	names(reps) <- c("E","H","S")
#	unlist(mapply(function(w.,r.){
#				rep(w.,r.)/r.
#			},w,reps))
#}


#Households <- as.matrix(DHS[,c("E1","E2","H1","H2","S1","S2","S3","S4","S5","S6")]) 

# get c for each household:


#cgivenw <- function(Households,w){
#	colSums(t(Households) * expandw(w))
#}
library(data.table)
# get a decent grid of w:
# work compouter
#source("/data/commons/triffe/git/InequalityAudit/InequalityAudit/R/TernaryTest.R")

# change this path!
source("R/TernaryTest.R")
w_all 			<- getTernTriangles(n=50)$ternmid
colnames(w_all) <- c("E","H","S")
w_all 			<- as.list(data.frame(t(w_all)))
# -------
#w <- c(1, 1, 1) / 3
#k <- 1 / 3
# k: this will do a 4x4 grid of k
#k 				<- seq(0,1,length.out = 17)[-1]

# do a huge computation (better for centile plots)
# then pick out the values of k that are useful for ternary plots
#k <- seq(.01,1,by=.01)
w_iteration <- function(w, DHS, k, CCvar = "CC_Poor2"){
	
	# 2) expand w to account for nr of dimensions
	reps                <- c(2, 2, 6)
	names(reps)         <- c("E", "H", "S")
	we                  <- unlist(mapply(function(w., r.){
						    rep(w., r.) / r.
					       }, w, reps))
	Households          <- DHS[,c("E1","E2","H1","H2","S1","S2","S3","S4","S5","S6")]
	cvec 				<- zapsmall(colSums(t(Households) * we))
	
	AF_Poor 			<- outer(cvec,k,">=")
	colnames(AF_Poor) 	<- k

# step 6: do poor measures agree?
	AFCCagree 			<- AF_Poor != DHS[,CCvar]
	# one-time test:
	#AFCCagree 			<- AF_Poor != DHS$Test_poor
	colnames(AFCCagree) <- k
# a convenience object:
	DHSvars             <- DHS[, c("country","pes")]
	
# 7) get the weighted percent in agreement per country (AF_Poor)
# for k-dimensional objects
# data.table idiom preferred for speed
	agreeGrouping       <- data.table::data.table(cbind(DHSvars, AFCCagree * DHSvars$pes))
	
	applycols 			<- c(as.character(k),"pes")
	Sumsi     			<- agreeGrouping[,lapply(.SD,sum),by=country,.SDcols = applycols]
	denom               <- unlist(Sumsi[,ncol(Sumsi),with=FALSE]) # we recycle this...
	mi       			<- 100 * as.matrix(Sumsi[,as.character(k),with=FALSE]) / denom
	
# step 8:
	AFcen        		<- AF_Poor * cvec * DHS$pes
	CCcen        		<- DHS[,CCvar] * cvec * DHS$pes
	# a one-time test
	#CCcen <-  DHS$Test_poor * cvec * DHS$pes
	P_AFgrouping 		<- data.table::data.table(cbind(DHSvars, AFcen))
	P_CCgrouping 		<- data.table::data.table(cbind(DHSvars, CCcen))
	setnames(P_AFgrouping, c(colnames(DHSvars),k))

	Sumsii     	 		<- P_AFgrouping[,lapply(.SD,sum),by=country,.SDcols = as.character(k)]
	Sumsiii    	 		<- P_CCgrouping[,lapply(.SD,sum),by=country,.SDcols = "CCcen"]
	
	P_AFi        		<- 100 * as.matrix(Sumsii[,as.character(k),with=FALSE]) / denom
	P_CCi        		<- 100 * as.matrix(Sumsiii[,"CCcen",with=FALSE]) / denom
#write.table(data.frame(Sumsii$country,P_CCi, mi, stringsAsFactors =FALSE),file="copyandpaste.csv",sep=",")
	# the two summary variables we care about:
	Corr  				<- c(suppressWarnings(cor(P_AFi, P_CCi)))
	m_out 				<- colMeans(mi)

# spit the goodies back
    invisible(cbind(w1 = w[1], w2 = w[2], w3 = w[3], k, Corr = Corr, m_avg = m_out))
}

Test <- w_iteration(w=c(1,1,1)/3,DHS, k = .3, CCvar = "CC_Poor")

library(parallel)

RUN <- FALSE
if (RUN){
#w_iteration_c <- compiler::cmpfun(w_iteration) # compiled version doesn't run faster.
# could take approx 25 min to run on 4 cores, depends
system.time(Results <- mclapply(w_all, FUN = w_iteration, DHS=DHS, k=k, CCvar = "CC_Poor", mc.cores= detectCores()))
Results <- do.call(rbind, Results)
#1520.432 /60
#length(w_all)
dim(Results)
save(Results,file="Results.Rdata")
#save(Results, file = "/home/tim/git/InequalityAudit/InequalityAudit/Data/Results.Rdata")

system.time(Results2 <- mclapply(w_all, FUN = w_iteration, DHS=DHS, k=k, CCvar = "CC_Poor2", mc.cores= detectCores()))
Results2 <- do.call(rbind, Results2)
#1520.432 /60
#length(w_all)
dim(Results2)
save(Results2,file="Results2.Rdata")
#save(Results2, file = "/home/tim/git/InequalityAudit/InequalityAudit/Data/Results2.Rdata")
}

Results <- local(get(load("Results.Rdata")))
Results2 <- local(get(load("Results2.Rdata")))
head(Results)
mean(Results[,"m_avg"])
mean(Results2[,"m_avg"])