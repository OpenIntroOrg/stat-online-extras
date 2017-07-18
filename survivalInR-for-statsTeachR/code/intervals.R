library(KMsurv)

#===> survival object <===#
data(tongue); attach(tongue)
mso1 <- Surv(time, delta)
mso1
detach(tongue)


#===> survival object <===#
data(tongue); attach(tongue)
t1 <- c() #rep(NA, sum(time))
t2 <- c() #rep(NA, sum(time))
de <- c() #rep(NA, sum(time))
for(i in 1:max(time)){
	t1 <- append(t1, rep(i-1, length(time)-sum(time < i)))
	t2 <- append(t2, rep(i, length(time)-sum(time < i)))
	te <- delta[time >= i] == 1 & time[time >= i] == i
	de <- append(de, te)
}
mso2 <- Surv(t1, t2, de)
mso2
detach(tongue)


plot(survfit(mso1 ~ 1))
plot(survfit(mso2 ~ 1))

