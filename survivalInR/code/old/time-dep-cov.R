# a data frame d.f that has right censored data and covariates
# col.time is the number (or name) of the column in d.f with the event/censor times
# col.delta is the number/name of the column in d.f indicating if the event was observed
# col.cov is a vector of the numbers/names of the columns of d.f with covariates of interest
# td.cov is the number/name of the column of d.f that is to be made time dependent
# transform will be the transform of time multiplied by the covariate td.cov to create a time-dependent covariate
#  (the new covariate will be of the form transformation(time)*td.cov); default of log
# method is the method used in the coxph function; default of 'efron'
# output.model is whether a model should be given with the output; default of TRUE
# output.data.frame is whether the data frame used in the model should be output; default of FALSE
# verbose sets whether the user is notified about how far along the function is to completion
#  (this is useful when it will take more than a few seconds for computations)
timeDepCoxph <- function(d.f, col.time, col.delta, col.cov, td.cov, transformation=log,
		method='efron', output.model=TRUE, output.data.frame=FALSE, verbose=TRUE){
	if( length(col.cov) > 1){
		cov.names <- colnames(d.f[,col.cov])
	} else {
		if(is.numeric(col.cov)){
			cov.names <- colnames(d.f)[col.cov]
		} else {
			cov.names <- col.cov
		}
	}

	surv <- d.f[ ,col.time]
	obs <- d.f[ ,col.delta]
	n.old.rows <- dim(d.f)[1]
	max.event <- as.integer( max(surv)+1 )
	n.entries <- sum(as.integer( surv-1 )) + n.old.rows
	t.cov <- matrix(0, n.entries, length(col.cov)+4)
	colnames(t.cov) <- c("start", "stop", "observ", cov.names, "time.dep.cov")
	row <- 0
	hold <- 0
	for(i in 1:n.old.rows){
	  for(j in 1:max.event){
		if(surv[i] > (j-1)){
			row <- row+1
			t.cov[row,"start"] <- j-1
			t.cov[row,"stop"] <- j
			if(surv[i] <= t.cov[row,"stop"])
				t.cov[row,"observ"] <- obs[i]
			t.cov[row,cov.names] <- as.numeric(d.f[i,col.cov])
			t.cov[row,"time.dep.cov"] <- d.f[i,td.cov]*transformation(j)
		}
	  }
	  if(verbose){
	    temp <- as.integer(100*row/n.entries)
		if(temp > hold+4 & temp < 100){
			cat("data frame is ",temp,"% complete.\n", sep="")
			hold <- temp
		}
	  }
	}
	t.cov <- as.data.frame(t.cov)
	if(verbose) cat("Data frame completed.\n")
	if(output.model){
		cat("The covariates in the output will be of the same order as specified in 'col.cov'.\n")
		if(verbose) cat("\nFitting model.\n\n")
		. <- as.matrix(t.cov[,c(cov.names,"time.dep.cov")])
		coxph.fit <- coxph(Surv(t.cov$start, t.cov$stop, t.cov$observ) ~ ., data=t.cov, method=method)
		if(!output.data.frame){
			coxph.fit
		} else {
			list(coxph.fit=coxph.fit, time.dep.df=t.cov)
		}
	} else if(output.data.frame) t.cov
}