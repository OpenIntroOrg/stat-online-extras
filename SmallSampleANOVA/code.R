library(openintro)
data(COL)

x <- matrix(c(15,1,3,10,4,9),2)
mix <- function (x) {
  x1 <- rep(c("a","b","c"), apply(x, 2, sum))
  x2 <- rep(1:2, apply(x, 1, sum))
  x. <- table(sample(x1), sample(x2))
  return(chisq.test(x.)$statistic)
}

x
fisher.test(x)
chisq.test(x)



X2 <- chisq.test(x)$statistic

set.seed(50)
N  <- 1e4
XS <- rep(NA, N)
for (i in 1:N) {
  XS[i] <- mix(x)
}
sum(XS > X2) / N

histPlot(XS, col=COL[1,4], breaks=50)
