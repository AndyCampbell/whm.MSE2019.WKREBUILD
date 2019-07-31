#execute the simulation

## Clean slate
source(file = file.path(getwd(),"Scripts","0.setup.R"))
source(file = file.path(getwd(),"Scripts","1.LoadData.R"))
#SRR fits
load(file=file.path(RData.dir,"SRR.RData"))

Fvals <- c(0.08,0.10,0.12,0.14,0.16)

## define reference points here
refPts <- list()
refPts[["Fpa"]] <- Fpa
refPts[["Flim"]] <- Flim
refPts[["Fmsy"]] <- Fmsy
refPts[["Bpa"]] <- Bpa
refPts[["Blim"]] <- Blim
refPts[["BmsyTrig"]] <- NA  

#runame, HCR settings
runName <- stockName

HCR <- 1

#operating model
OM <- "OM1"
origstock <- stock <- WG18

#defaults
refPts[["BmsyTrig"]] <- NA        #no trigger point
minTAC <- 0                       #no minimum TAC
TACchng <- NA                     #no TAC change limits

#Starting F 
firstF <- as.numeric(fbar(WG18)[,'2017']) #F in 2017 from WG2018 assessment

#latest assessment year
assyearNum <- 2018

#Trim off last year of the stock object (why??)
minYear <- range(stock)["minyear"]
maxYear <- range(stock)["maxyear"]

#set the Bloss reference point
refPts[["Bloss"]] <- min(ssb(stock))

# Starting population numbers
dfWHMParam <- read_delim(file = file.path(MSE.dir,"Data","MSE2019_WHM_Params.dat"),
                         delim=",")
dfStartN <- dfWHMParam[,c(paste0("Num_2017_",seq(0,14)),"PG.2017")]

#Weights and selectivity
#Number of years for averaging when calculating weights
#biology
numAvgYrsB <- 3
#selection
numAvgYrsS <- 3

# resample from years (FALSE) or use average for all (TRUE)
constantVals <- TRUE 

#Observation Model - Assessmnt Forecast errors
#need to work these out
cvF  <- 0;	phiF <-	0; cvSSB <- 0; phiSSB <- 0

#flag for application of autocorrelation (TRUE=Yes)
#autocorrelation was shown to be significant in RP estimation 
recAR <- TRUE

#Maximum residual
#this is the residual corresponding to the maximum estimated recruitment
#this is used to potentially trim out unreasonably high recruitments in the simulation
#this is mirrored when trimming (i.e. [maxRecRes,-maxRecRes])
maxRecRes <- 1.5 # Default in SimpSIM is 3

SIM <- SimpSIM(fit = FIT,
               dfstartN = dfstartN,
               intF = firstF,
               bio.years = c(assyearNum-(numAvgYrsB-1), assyearNum),
               bio.const = constantVals,
               sel.years = c(assyearNum-(numAvgYrsB-1), assyearNum),
               sel.const = constantVals,
               Fcv = cvF,
               Fphi = phiF,
               SSBcv = cvSSB,
               SSBphi = phiSSB,
               rhologRec = recAR,
               Btrigger = refPts[["BmsyTrig"]],
               Blim = refPts[["Blim"]],
               Bpa = refPts[["Bpa"]],
               TACchange = NA,
               chngB = 0,
               catTac = 1,
               Fscan = Fvals,
               recruitment.trim = c(maxRecRes,-maxRecRes),
               Nrun = nyr,
               process.error = TRUE,
               verbose = TRUE,
               HCR = HCR,
               minTAC = minTAC)
