library(openintro)
library(xtable)
data(COL)
set.seed(3)
data(ToothGrowth)
tg <- ToothGrowth
tg <- tg[tg$dose > 0.5, ]
tg$level <- factor(paste0(tg$supp, ", ", tg$dose, "mg"))

myPDF("interaction.pdf", 7, 4, mar=c(3.2, 4, 0.5, 0.5),
      mgp=c(1.7, 0.55, 0))
boxPlot(tg$len, tg$level, pch=19, col=COL[1,2], xlab="", ylab="")
par(las=0)
mtext("Tooth Length", 2, 2.8)
g <- lm(len ~ I(supp == "VC") + dose, tg)
xtable(g)
dev.off()

myPDF("interaction-noint-w-mean.pdf", 7, 4,
      mar=c(3.2, 4, 0.5, 0.5),
      mgp=c(1.7, 0.55, 0))
boxPlot(tg$len, tg$level, pch=19, col=COL[1,2], xlab="", ylab="")
L <- levels(tg$level)
segments(1:4 - 0.27, unique(predict(g)), 1:4+0.27, lwd=4, col=COL[4], lty=3)
par(las=0)
mtext("Tooth Length", 2, 2.8)
dev.off()

g. <- lm(len ~ supp * dose, tg)
xtable(g.)
predict(g.)

aggregate(len ~ supp + dose, tg, mean)
aggregate(len ~ supp + dose, tg, sd)




