rm(list=ls())

load('../data/ep-c01.rda')
ep.c01 <- hold

load('../data/ep-c05.rda')
ep.c05 <- hold

load('../data/ep-c10.rda')
ep.c10 <- hold

load('../data/hw-k01.rda')
hw.k01 <- hold

load('../data/hw-k05.rda')
hw.k05 <- hold

load('../data/hw-k10.rda')
hw.k10 <- hold

rm(hold)

save(ep.c01, ep.c05, ep.c10, hw.k01, hw.k05, hw.k10, file='../data/tables.rda')
