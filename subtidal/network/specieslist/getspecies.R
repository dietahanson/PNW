allsp = read.csv("allspecies_raw.csv", header = F)
allsp = allsp[!allsp$V1=="",]
allsp$V2 = NULL
allsp = unique(allsp)
write.csv(allsp , file = "allsp.csv", row.names = F, col.names = F)
