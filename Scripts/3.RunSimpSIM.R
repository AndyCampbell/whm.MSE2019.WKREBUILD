#execute the simulation

## Clean slate
source(file = file.path(getwd(),"Scripts","0.setup.R"))
source(file = file.path(getwd(),"Scripts","1.LoadData.R"))
#SRR fits
load(file=file.path(RData.dir,"SRR.RData"))

Fvals <- c(0.08,0.10,0.12,0.14,0.16)