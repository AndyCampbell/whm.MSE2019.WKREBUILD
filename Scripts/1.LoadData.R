#load data

#WGWIDE 2018 assessment
load(file=file.path(RData.dir,"WGWIDE2018.RData"))
WG18 <- WHOM.WGWIDE2018
name(WG18) <- stockName
#clean up
rm(WHOM.WGWIDE2018)
