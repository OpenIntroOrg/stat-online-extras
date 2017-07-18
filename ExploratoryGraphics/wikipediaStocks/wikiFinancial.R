library(openintro)
library(stockPortfolio)

load("wikiFinancial-120126.RData")

myPDF("wikipediaStocks.pdf", 7.5, 4, mar=c(3.2, 5.3, 0.5, 4.8), mgp=c(3.2, 0.55, 0), call=2)
#par(mar=c(4,5,2,5))
plot(critraffic, type="n", xlab="", ylab="Daily number of views for\nthe Wikipedia Financial Crisis page")
segments(critraffic[,1], rep(0, nrow(critraffic)), critraffic[,1], critraffic[,2], col="#335588AA")
mtext("Time", 1, 2)
R <- dji$R[,1]
x <- cumprod(1+rev(R))*7000

lines(as.numeric(rev(as.Date(row.names(dji$R)))), x, lwd=1.3)
xmax <- 13241
at   <- c(0,5,10,15)*1000/xmax*x[1]
label <- c(0,5,10,15)*1000

axis(4, at, label)
par(las=0)
mtext("Dow Jones Industrial Average", 4, line=3.65)
dev.off()

