library(openintro)
library(OIdata)
library(maps)

data(states)
# smokerTotal
# murder
# coal2005

MapVariableLegend <- function(mapVar){
	par(mar=rep(0,4))
	plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
	#R <- range(mapVar$x)
	#y <- seq(R[1], R[2], length.out=100)
	#for(i in 1:100){
	#	rect(c(0,0.25), c((i-1)/100, i/100), border="#00000000", col=mapVar$R[i])
	#}
	#text(0.25, 0.05, format(R[1], digits=3), pos=4)
	#text(0.25, 0.95, format(R[2], digits=3), pos=4)
	par(mar=c(3.9, 3.9, 1, 1))
}

myPDF("threeStateMaps.pdf", 5, 7.5, mfrow=c(3,1), call=2, mar=rep(0,4))
mat <- matrix(1:6, 3, 2, TRUE)
layout(mat, c(4,1))
range(states$smokerTotal)
mv <- mapVariable(states$smokerTotal, states$state, RGB=c(0,1,1))
mtext("Smoking rates (11.5% to 28.7%)", 1, 0.36)
MapVariableLegend(mv)
#text(-79, 49, "Smoking rates (11.5% to 28.7%)")
these <- states$abbr != 'DC'
range(states$murder[these])
mv <- mapVariable(states$murder[these], states$state[these], RGB=c(1,0.5,0.5))
#text(#-111, 26
#	-79, 49, "Murder (1.1 to 9.9 per 100k)")
mtext("Murder (1.1 to 9.9 per 100k)", 1, 0.36)
MapVariableLegend(mv)
range(states$coal2005)
mv <- mapVariable(states$coal2005, states$state, RGB=c(1,1,0))
#text(-79, 49, "Energy from coal (0% to 98%)")
mtext("Energy from coal (0% to 98%)", 1, 0.36)
MapVariableLegend(mv)
dev.off()


