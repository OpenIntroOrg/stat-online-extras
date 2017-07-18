#ask = function(){
#    eval(parse(prompt = paste("Enter confidence coefficent", ": ", sep="")))
#}

#x    <- my.surv
#type <- 'hall'
#tL   <- 100
#tU   <- 600
#confType  <- 'plain'
#confLevel <- 0.95

confBands <- function(x, confType=c('plain', 'log-log', 'asin-sqrt'), confLevel=c(0.90, 0.95, 0.99), type=c('ep', 'hall'), tL=NULL, tU=NULL){
	fit  <- survfit(x ~ 1)
	n    <- length(x)/2
	time <- summary(fit)$time
	std.err <- summary(fit)$std.err
	surv    <- summary(fit)$surv
	if(is.null(tL)){
		t.L <- time[1]
	} else {
		temp <- time[time <= tL]
		t.L  <- temp[length(temp)]
	}
	if(is.null(tU)){
		t.U <- time[length(time)]
	} else {
		temp <- time[time <= tU]
		t.U  <- temp[length(temp)]
	}
	t.L.pos <- which(time == t.L)
	t.U.pos <- which(time == t.U)
	sigma.2 <- ( std.err / surv )^2
	a.L     <- n*sigma.2[time == t.L]/(1+n*sigma.2[time == t.L])
	a.U     <- n*sigma.2[time == t.U]/(1+n*sigma.2[time == t.U])
	
	#=====> this has to go <=====#
#	cat("a_L and a_U are", a.L,"and", a.U, "\n")
#	input <- eval(parse(prompt = paste("Enter confidence coefficent", ": ", sep="")))
	
	#=====> pull value from table <====#
	aU <- format(c(round(50*a.U)/50,0.01))[1]
	aL <- format(c(round(50*a.L)/50,0.01))[1]
	if(type[1] == 'hall'){
		if(confLevel[1] == 0.90){
			input <- hw.k10[aU,aL]
		} else if(confLevel[1] == 0.95){
			input <- hw.k05[aU,aL]
		} else if(confLevel[1] == 0.99){
			input <- hw.k01[aU,aL]
		} else {
			stop("Confidence levels of 0.90, 0.95 and 0.99 are allowed.")
		}
		m.fact <- input*(1+n*sigma.2)/sqrt(n)
	} else if(type[1] == 'ep'){
		if(confLevel[1] == 0.90){
			input <- ep.c10[aU,aL]
		} else if(confLevel[1] == 0.95){
			input <- ep.c05[aU,aL]
		} else if(confLevel[1] == 0.99){
			input <- ep.c01[aU,aL]
		} else {
			stop("Confidence levels of 0.90, 0.95 and 0.99 are allowed.")
		}
		m.fact <- input*sqrt(sigma.2)
	} else {
		stop('The "type" variable is not recognized.')
	}
	CI <- matrix(NA, length(surv), 2)
	if(confType[1] == 'log-log'){
		theta  <- exp(m.fact/log(surv))
		CI[,1] <- (surv)^(1/theta)
		CI[,2] <- (surv)^(theta)
	} else if(confType[1] == 'asin-sqrt'){
		temp   <- asin(sqrt(surv)) - 0.5*m.fact*sqrt(surv/(1-surv))
		lower  <- apply(cbind(rep(0, length(temp)), temp), 1, max)
		CI[,1] <- (sin(lower))^2
		temp   <- asin(sqrt(surv)) + 0.5*m.fact*sqrt(surv/(1-surv))
		upper  <- apply(cbind(rep(pi/2, length(temp)), temp), 1, min)
		CI[,2] <- (sin(upper))^2
	} else {
		CI[,1] <- surv*(1-m.fact)
		CI[,2] <- surv*(1+m.fact)
	}
	CI[CI[,1] < 0, 1] <- 0
	CI[CI[,2] < 0, 2] <- 1
	return(list(time=time, lower=CI[,1], upper=CI[,2]))
}