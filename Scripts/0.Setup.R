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

#globals

#reference points (as set 2017 WKWIDE)
Blim <- 661917
Bpa <- 911587
Fpa <- 0.108
Flim <- 0.151
Fmsy <- 0.108
MSYBtrigger <- 911587

#locations
Drive <- "C:"
Base.dir <- file.path(Drive,"Stocks","hom_27_2a4a5b6a7a-ce-k8")
Assessment.Dir <- file.path(Base.dir,"Assessment")
MSE.dir <- file.path(Base.dir,"MP_MSE","MSE 2019","whm.MSE2019.WKREBUILD")
Data.dir <- file.path(MSE.dir,"Data")
RData.dir <- file.path(MSE.dir,"RData")
Source.dir <- file.path(Base.dir,"Source")

# Source SimpSIM,statistics and other functions
#source(file.path(MSE.dir,"Source","SimpSIM_v3.R"))
source(file.path(MSE.dir,"Source","SimpSIM_WHM.R"))   #WHM version
source(file.path(MSE.dir,"Source","SimpSIM_additional_Funcs.r"))
source(file.path(MSE.dir,"Source","MSE_StatPlot_Funcs_2016.r"))

#stock name
stockName <- "WHM"

#statistical periods for reporting
lStatPer <- list()
#annual stats
#for (y in seq(1980,2047)){
#  lStatPer[[ac(y)]]<-c(y,y)
#}
#MSY runs
for (y in seq(1980,2216)){
  lStatPer[[ac(y)]]<-c(y,y)
}

#Short, Medium and Long Term
lStatPer[['ST']] <- c(2017,2021)
lStatPer[['MT']] <- c(2022,2026)
lStatPer[['LT']] <- c(2027,2046)
lStatPer[['ET']] <- c(2167,2216)

#iterations to run
nits <- 10000
#years to project
nyr <- 35
