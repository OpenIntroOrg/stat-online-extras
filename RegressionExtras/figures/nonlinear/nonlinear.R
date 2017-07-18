library(openintro)
library(xtable)
data(COL)
set.seed(3)

myPDF("nonlinear.pdf", 7.5, 1.5*1.8, mfrow=c(1,2), mar=c(1.5, 2.5, 0.5, 1.5), mgp=c(1.7, 0.55, 0))
x <- runif(50, 5, 15)^1.5
f <- function(x, sd=0.5) {
  exp(1 + 0.08*x + rnorm(length(x), sd=sd))
}
y <- f(x)
plot(x, y, pch=19, col=COL[1,2], xlab="", ylab="")
x <- seq(0, 25, 0.05)^1.5
y <- f(x, 0)
#lines(x, y)

x <- runif(100, -5, 28)
f <- function(x) {
  2 - 1.3*x + 0.1*x^2 - 0.002*x^3
}
y <- f(x) + rnorm(100, sd=2)
plot(x, y, pch=19, col=COL[1,2], xlab="", ylab="")
x <- seq(-10, 35, 0.1)
lines(x, f(x), col="#00000000")

# x <- 2*runif(50, -5, 28) + 12
# f <- function(x, sd=0.5) {
  # x <- x/2 - 12
  # exp(-0.3*(2 - 1.3*x + 0.1*x^2 - 0.002*x^3) + rnorm(length(x), sd=sd))
# }
# y <- f(x)
# plot(x, y, pch=19, col=COL[1,2], xlab="", ylab="")
# x <- 2*seq(-10, 35, 0.1) + 12
# lines(x, f(x, 0), col="#00000000")

dev.off()




set.seed(3)
myPDF("nonlinear1-1.pdf", 2*7.5/3, 2*1.8, mar=c(3.2, 3.5, 0.5, 1.5), mgp=c(2.1, 0.55, 0))
x1 <- runif(50, 5, 15)^1.5
f <- function(x, sd=0.5) {
  exp(1 + 0.08*x + rnorm(length(x), sd=sd))
}
y1 <- f(x1)
plot(x1, y1, pch=19, col=COL[1,2], xlab="x", ylab="")
mtext("y", 2, 2.8)
x <- seq(0, 25, 0.05)^1.5
y <- f(x, 0)
#lines(x, y)

dev.off()

myPDF("nonlinear1-2.pdf", 2*2*7.5/3, 2*1.8, mar=c(3.2, 3.5, 0.5, 1.5), mgp=c(2.1, 0.55, 0), mfrow=c(1,2))
histPlot(x1, xlab="x", ylab="", col=COL[1])
histPlot(y1, xlab="y", ylab="", col=COL[1])
dev.off()

myPDF("nonlinear1-3.pdf", 2*7.5/3, 2*1.8, mar=c(3.2, 3.5, 0.5, 1.5), mgp=c(2.1, 0.55, 0))
plot(x1, log(y1), pch=19, col=COL[1,2], xlab="x", ylab="")
par(las=0)
mtext("y* = log(y)", 2, 2.45)
lm(log(y1) ~ x1)
dev.off()

myPDF("nonlinear1-4.pdf", 2*7.5/3, 2*1.8, mar=c(3.2, 3.5, 0.5, 1.5), mgp=c(2.1, 0.55, 0))
plot(x1, y1, pch=19, col=COL[1,2], xlab="x", ylab="")
mtext("y", 2, 2.8)
lines(x, y)
dev.off()



#_____ Set 2 _____#
x2 <- runif(100, -5, 28)
f <- function(x) {
  2 - 1.3*x + 0.1*x^2 - 0.002*x^3
}
y2 <- f(x2) + rnorm(100, sd=2)
x <- seq(-10, 35, 0.1)
y <- f(x)


myPDF("nonlinear2-1.pdf", 2*7.5/3, 2*1.8, mar=c(3.2, 3.5, 0.5, 1.5), mgp=c(2.1, 0.55, 0))
plot(x2, y2, pch=19, col=COL[1,2], xlab="x", ylab="")
mtext("y", 2, 2.8)
#lines(x, y)
dev.off()

myPDF("nonlinear2-2.pdf", 2*7.5/3, 2*1.8, mar=c(3.2, 3.5, 0.5, 1.5), mgp=c(2.1, 0.55, 0))
plot(x2, y2, pch=19, col=COL[1,2], xlab="x", ylab="")
mtext("y", 2, 2.8)
#lines(x, y)
(g <- lm(y2 ~ x2))
xtable(g)
abline(g)
dev.off()

myPDF("nonlinear2-3.pdf", 2*7.5/3, 2*1.8, mar=c(3.2, 3.5, 0.5, 1.5), mgp=c(2.1, 0.55, 0))
plot(x2, y2, pch=19, col=COL[1,2], xlab="x", ylab="")
mtext("y", 2, 2.8)
#lines(x, y)
x2. <- x2^2
(g <- lm(y2 ~ x2 + x2.))
xtable(g)
lines(x, predict(object=g, data.frame(x2=x, x2.=x^2)))
dev.off()

myPDF("nonlinear2-4.pdf", 2*7.5/3, 3.3*1.8, mar=c(3.7, 3.5, 1.5, 1.5), mgp=c(2.1, 0.55, 0))
layout(1:2, 1, c(2, 1.3))
plot(x2, y2, pch=19, col=COL[1,2], xlab="x", ylab="")
mtext("y", 2, 2.8)
#lines(x, y)
x2.. <- x2^3
(g <- lm(y2 ~ x2 + x2. + x2..))
xtable(g)
lines(x, predict(object=g, data.frame(x2=x, x2.=x^2, x2..=x^3)))
plot(x2, g$res, pch=19, col=COL[1,2], xlab="x", ylab="", ylim=c(-6, 6), axes=FALSE)
mtext("residuals", 2, 2.8)
abline(h=0, col=COL[6], lty=2)
box()
axis(1)
axis(2, at=c(-6, -3, 0, 3, 6))
dev.off()








