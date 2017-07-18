#data(burn); x <- burn; colTime <- "T1"; colDelta <- "D1"; colCov <- 2:4; tdCov <- "Z1"; outputModel <- TRUE; outputDF <- FALSE; verbose <- TRUE
timeDepCoxph <-
function(x, colTime, colDelta, colCov, tdCov, transformation=log,
outputModel=TRUE, outputDF=FALSE, verbose=TRUE, ...){
	if( length(colCov) > 1){
		cov.names <- colnames(x[,colCov])
	} else {
		if(is.numeric(colCov)){
			cov.names <- colnames(x)[colCov]
		} else {
			cov.names <- colCov
		}
	}
	surv      <- x[ ,colTime]
	obs       <- x[ ,colDelta]
	nOldRows  <- dim(x)[1]
	nColCov   <- length(colCov)
	max.event <- as.integer( max(surv)+1 )
	n.entries <- sum(as.integer( surv-1 )) + nOldRows
	t.cov     <- matrix(0, n.entries, length(colCov)+4)
	colnames(t.cov) <- c("start", "stop", "observ", cov.names, "timeDepCov")
	row             <- 0
	hold            <- 0
	for(i in 1:nOldRows){
		t     <- 1:surv[i]
		these <- row + t
		t.cov[these,'start']  <- 0:(surv[i]-1)
		t.cov[these,'stop']   <- t
		t.cov[these,"observ"] <- 1
		hold                  <- tail(these,1)
		t.cov[hold,"observ"]  <- obs[i]
		hold                  <- as.numeric(x[i,colCov])
		t.cov[these,cov.names]    <- matrix(hold, surv[i], nColCov, by.row=TRUE)
		t.cov[these,"timeDepCov"] <- x[i,tdCov]*transformation(t)
		row <- row + surv[i]
		if(verbose && any((these %% 1000) == 0)){
			cat("Completed", row, "rows out of", n.entries, "\n")
		}
	}
	t.cov <- as.data.frame(t.cov)
	if(outputModel){
		cat('The covariates in the output will be of the\nsame order as specified in "colCov".\n')
		if(verbose){
			cat("\n*** Fitting model ***\n\n")
		}
		t.        <- as.matrix(t.cov[,c(cov.names,"timeDepCov")])
		coxph.fit <- coxph(Surv(t.cov$start, t.cov$stop, t.cov$observ) ~ t., data=t.cov, ...)
		hold      <- coxph.fit
		if(outputDF){
			hold["time.dep.df"] <- t.cov
			cat('Access the time-dependent data.frame via\n the "time.def.df" attribute.\n')
		}
		return(hold)
	} else if(outputDF){
		return(t.cov)
	}
}
