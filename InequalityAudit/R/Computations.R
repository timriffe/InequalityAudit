

setwd("/home/tim/Dropbox/Graficos (1)")

DHS <- read.csv("DHS_Triangles.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
head(DHS)
dim(DHS)

w <- c(1,1,1)/3
w
names(w) <- c("E","H","S")
expandw <- function(w){
	reps <- c(2,2,6)
	names(reps) <- c("E","H","S")
	unlist(mapply(function(w.,r.){
				rep(w.,r.)/r.
			},w,reps))
}



Households <- as.matrix(DHS[,c("E1","E2","H1","H2","S1","S2","S3","S4","S5","S6")]) 

# get c for each household:

cgivenw <- function(Households,w){
	colSums(t(Households) * expandw(w))
}

# get a decent grid of w:
w_all <- getTernTriangles(n=50)$ternmid
colnames(w_all) <- c("E","H","S")

# -------

# k 
k <- seq(0,1,length.out = 17)[-1]

w_iteration <- function(w, DHS, k){
	
	# 2) expand w to account for nr of dimensions
	reps                <- c(2, 2, 6)
	names(reps)         <- c("E", "H", "S")
	we                  <- unlist(mapply(function(w., r.){
						    rep(w., r.) / r.
					       }, w, reps))
	Households          <- DHS[,c("E1","E2","H1","H2","S1","S2","S3","S4","S5","S6")]
	cvec 				<- colSums(t(Households) * we)
	
	AF_Poor 			<- outer(cvec,k,">")
	colnames(AF_Poor) 	<- k
	
# step 6: do poor measures agree?
	AFCCagree 			<- AF_Poor != DHS$CC_Poor
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
	CCcen        		<- DHS$CC_Poor * cvec * DHS$pes
	
	P_AFgrouping 		<- data.table::data.table(cbind(DHSvars, AFcen))
	Sumsi     	 		<- P_AFgrouping[,lapply(.SD,sum),by=country,.SDcols = as.character(k)]
	P_AFi        		<- 100 * as.matrix(Sumsi[,as.character(k),with=FALSE]) / denom
	P_CCi        		<- 100 * tapply(CCcen,DHS$country,sum) / denom
	
# the two summary variables we care about:
	Corr  				<- c(suppressWarnings(cor(P_AFi, P_CCi)))
	m_out 				<- colMeans(mi)

# spit the goodies back
    invisible(cbind(w1 = w[1], w2 = w[2], w3 = w[3], k, Corr = Corr, m_avg = m_out))
}

library(parallel)
w
A <- system.time(w_iteration(w, DHS, k))

cl <- makeCluster(getOption("cl.cores", 4))
Results <- parRapply(cl = cl, w_all, FUN = w_iteration, DHS=DHS, k=k)
stopCluster(cl)


