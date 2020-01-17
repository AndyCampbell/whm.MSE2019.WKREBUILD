#initial environment setup

#install.packages("devtools")
#install.packages("FLCore", repo = "http://flr-project.org/R")
#install.packages(c("ggplotFL"), repos="http://flr-project.org/R")
#library(devtools)
#install_github("ices-tools-prod/msy")

rm(list=ls())
gc()
try(dev.off(),silent=TRUE)

library(devtools)
#install_github("msy", "einarhjorleifsson", ref = "master")

library(FLCore)
library(msy)
library(tidyverse)
library(Hmisc)
library(xtable)
library(scales)
library(gplots)
library(Cairo)
library(reshape2)
library(stringr)

#locations
Drive <- "C:"
Base.dir <- file.path(Drive,"Stocks","hom_27_2a4a5b6a7a-ce-k8")
Assessment.Dir <- file.path(Base.dir,"Assessment")
MSE.dir <- file.path(Base.dir,"MP_MSE","MSE 2019","whm.MSE2019.WKREBUILD")
Data.dir <- file.path(MSE.dir,"Data")              #this is where the results of the 1000 assessment runs for initialisation of the MSE are saved
RData.dir <- file.path(MSE.dir,"RData")            #historic assessment outputs, stock-recruit fits
Source.dir <- file.path(Base.dir,"Source")         #R source code

# Source SimpSIM,statistics and other functions
#source(file.path(MSE.dir,"Source","SimpSIM_v3.R"))
source(file.path(MSE.dir,"Source","SimpSIM_WHM.R"))   #WHM version
source(file.path(MSE.dir,"Source","SimpSIM_additional_Funcs.r"))
source(file.path(MSE.dir,"Source","MSE_StatPlot_Funcs_2016.r"))
source(file.path(MSE.dir,"Source","MSE_StatTable_Funcs.R"))
source(file.path(MSE.dir,"Source","SimpSIM_HCR.R"))

#stock name
stockName <- "WHM"

#iterations to run
nits <- 1000
#years to project
nyr <- 30
#start,end years for simulation
yStart <- 2017
yEnd <- yStart + nyr - 1
simYears <- ac(seq(yStart,yEnd))

#statistical periods for reporting
lStatPer <- list()

#load in the assessment FLStock object
load(file=file.path(RData.dir,"WGWIDE2018.RData"))
Ass <- WHOM.WGWIDE2018
name(Ass) <- stockName
rm(WHOM.WGWIDE2018)
minObsYear <- range(Ass)["minyear"]
maxObsYear <- range(Ass)["maxyear"]
obsYears <- ac(seq(minObsYear,maxObsYear))

#reference points (as set 2017 WKWIDE)
Blim <- 661917
Bpa <- 911587
Fpa <- 0.108
Flim <- 0.151
Fmsy <- 0.108
MSYBtrigger <- 911587

#create a list for the output statistical periods
#annual statistics, for each historic and simulated year
for (y in seq(min(as.integer(dimnames(Ass)$year)),yEnd)){lStatPer[[ac(y)]]<-c(y,y)}
#Short (first 5), Medium (next 5) and Long Term (next 20)
lStatPer[['ST']] <- c(yStart,yStart+4)
lStatPer[['MT']] <- c(yStart+5,yStart+9)
lStatPer[['LT']] <- c(yStart+10,yStart+29)

#load SRR objects
load(file=file.path(RData.dir,"SRR.RData"))
