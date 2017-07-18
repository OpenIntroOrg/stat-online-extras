Cbind <- function(m1, m2){
	if(is.vector(m1)){
		m1 <- matrix(m1)
	}
	if(is.vector(m2)){
		m2 <- matrix(m2)
	}
	l1 <- dim(m1)[1]
	l2 <- dim(m2)[1]
	d1 <- dim(m1)[2]
	d2 <- dim(m2)[2]
	if(l1 > l2){
		m2 <- rbind(m2, matrix(NA, l1-l2, d2))
	}
	if(l1 < l2){
		m1 <- rbind(m1, matrix(NA, l2-l1, d1))
	}
	cbind(m1, m2)
}

mendMatrix <- function(m){
	d <- dim(m)
	for(i in 1:d[2]){
		temp <- m[,i]
		temp <- temp[!is.na(temp)]
		lt   <- length(temp)
		skip <- d[1] - lt
		m[,i] <- NA
		m[skip+1:lt,i] <- temp
	}
	m
}

