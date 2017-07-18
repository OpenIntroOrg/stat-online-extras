set.seed(2)
N     <- 300
Z1    <- sample(0:1, N, replace=TRUE)
inter <- sample(2:100, N, replace=TRUE)

event <- rep(0, N)
delta <- rep(FALSE, N)
for(i in 1:N){
	for(j in 1:150){
		if(!delta[i]){
			event[i]  <- j
			Z2        <- (inter[i] < j) # intervention
			haz       <- 0.01*exp(-0.0001*j - 0.2*Z1[i] - 0.4*Z2)
			if(haz > runif(1)){
				delta[i] <- TRUE
			}
		}
	}
}
inter[event < inter] <- NA
relapse <- data.frame(event  = event,
                      delta  = delta,
                      gender = Z1,
                      inter  = inter)
relapse

#====> Initialize new variables <====#
N  <- dim(relapse)[1]
t1 <- rep(0, N+sum(!is.na(relapse$int)))  # Initialize start time at 0
t2 <- rep(-1, length(t1))                 # Build vector for end times
e  <- rep(-1, length(t1))                 # Whether event was censored
g  <- rep(-1, length(t1))                 # Gender covariate
i  <- rep(FALSE, length(t1))              # Initialize intervention at FALSE

#====> Cycle through each patient <====#
j  <- 1
for(ii in 1:dim(relapse)[1]){
  if(is.na(relapse$int[ii])){      # no intervention, copy survival record
    t2[j] <- relapse$time[ii]
    e[j]  <- relapse$event[ii]
    g[j]  <- relapse$gender[ii]
    j <- j+1
  } else {                         # intervention, split records
    g[j+0:1] <- relapse$gender[ii] # gender is same for each time
    e[j]     <- 0                  # no relapse observed pre-intervention
    e[j+1]   <- relapse$event[ii]  # relapse occur post-intervention?
    i[j+1]   <- TRUE               # Intervention covariate, post-intervention
    t2[j]    <- relapse$int[ii]-1  # End of pre-intervention
    t1[j+1]  <- relapse$int[ii]-1  # Start of post-intervention
    t2[j+1]  <- relapse$time[ii]   # End of post-intervention
    j <- j+2                       # Two records added
  }
}


(mySurv <- Surv(t1, t2, e))
(myCPH  <- coxph(mySurv ~ g + i))
