pdf('randomPointsSelectionForExercise.pdf', 6, 3)

set.seed(1)
par(mfrow=c(1,2), mar=c(1,1,1,1))

x <- matrix(NA, 400, 2)
k <- 0
for(i in 1:20){
	for(j in 1:20){
		k <- k + 1
		x[k,1] <- i
		x[k,2] <- j
	}
}
these <- sample(400, 20)
plot(x, pch='.', axes=FALSE)
points(x[these,])
box()

these <- c(26, 57, 72, 81, 106, 133, 145, 151, 176, 199, 225, 235, 240, 273, 275, 307, 317, 342, 351, 374)

#these <- seq(10, 390, 20) + sample(-9:10, 20, TRUE)
plot(x, pch='.', axes=FALSE)
points(x[these,])
box()



dev.off()