#cell color function
fRiskColour <- function(x) cut(x, breaks = c(0, 4.999999, Inf), 
                               labels=c("green", "red"),
                               include.lowest = TRUE, right = TRUE)


#stats tabulation function

fTabulateStats <- function(i,data){
  
  require(xtable)
  require(tools)
  require(dplyr)
  
  ac <- function(x){as.character(x)}
  
  cat(i,"\n")
  
  n <- names(data)[[i]]
  d <- data[[n]]
  t <- d$SSB$val
  
  #MSY run?
  MSY <- sum(unlist(dimnames(t)['year'])=="2216")==1
  
  OM <- d$OM
  HCR <- d$HCR
  
  #filenames
  #tex file
  fTex <- file.path(MSE.Dir,"Results",OM,paste0("HCR",HCR),paste0(n,".tex"))
  #pdf file
  fpdf <- file.path(MSE.Dir,"Results",OM,paste0("HCR",HCR),paste0(n,".pdf"))
  #working directory pdf
  wdfpdf <- file.path(getwd(),paste0(n,".pdf"))
  
  #clean any old files
  if (file.exists(fTex)) file.remove(fTex)
  if (file.exists(fpdf)) file.remove(fpdf)
  
  #short,medium
  #2017 - 2026
  #median SSB and CV
  SSB.rep <- data.frame('2017' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2017',,,,])/1e3,digits=1)),
                                   ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2017',,,,]),digits=0))),
                        '2018' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2018',,,,])/1e3,digits=1)),
                                   ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2018',,,,]),digits=0))),
                        '2019' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2019',,,,])/1e3,digits=1)),
                                   ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2019',,,,]),digits=0))),
                        '2020' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2020',,,,])/1e3,digits=1)),
                                   ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2020',,,,]),digits=0))),
                        '2021' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2021',,,,])/1e3,digits=1)),
                                   ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2021',,,,]),digits=0))),
                        '2022' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2022',,,,])/1e3,digits=1)),
                                   ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2022',,,,]),digits=0))),
                        '2023' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2023',,,,])/1e3,digits=1)),
                                   ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2023',,,,]),digits=0))),
                        '2024' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2024',,,,])/1e3,digits=1)),
                                   ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2024',,,,]),digits=0))),
                        '2025' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2025',,,,])/1e3,digits=1)),
                                   ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2025',,,,]),digits=0))),
                        '2026' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2026',,,,])/1e3,digits=1)),
                                   ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2026',,,,]),digits=0))),
                        'ST' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'ST',,,,])/1e3,digits=1)),
                                 ac(round(100.*as.numeric(d$SSB$val[c('CV'),'ST',,,,]),digits=0))),
                        'MT' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'MT',,,,])/1e3,digits=1)),
                                 ac(round(100.*as.numeric(d$SSB$val[c('CV'),'MT',,,,]),digits=0))),
                        stringsAsFactors = FALSE)
  
  colnames(SSB.rep)<-c(as.character(seq(2017,2026)),"ST","MT")
  rownames(SSB.rep)<-c("SSB (kt)","SSB CV")

  #2027 - 2036
  SSB.rep2 <- data.frame('2027' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2027',,,,])/1e3,digits=1)),
                                     ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2027',,,,]),digits=0))),
                          '2028' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2028',,,,])/1e3,digits=1)),
                                     ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2028',,,,]),digits=0))),
                          '2029' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2029',,,,])/1e3,digits=1)),
                                     ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2029',,,,]),digits=0))),
                          '2030' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2030',,,,])/1e3,digits=1)),
                                     ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2030',,,,]),digits=0))),
                          '2031' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2031',,,,])/1e3,digits=1)),
                                     ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2031',,,,]),digits=0))),
                          '2032' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2032',,,,])/1e3,digits=1)),
                                     ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2032',,,,]),digits=0))),
                          '2033' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2033',,,,])/1e3,digits=1)),
                                     ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2033',,,,]),digits=0))),
                          '2034' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2034',,,,])/1e3,digits=1)),
                                     ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2034',,,,]),digits=0))),
                          '2035' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2035',,,,])/1e3,digits=1)),
                                     ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2035',,,,]),digits=0))),
                          '2036' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2036',,,,])/1e3,digits=1)),
                                     ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2036',,,,]),digits=0))),
                          'LT' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'LT',,,,])/1e3,digits=1)),
                                  ac(round(100.*as.numeric(d$SSB$val[c('CV'),'LT',,,,]),digits=0))),
                         stringsAsFactors = FALSE)

  colnames(SSB.rep2)<-c(as.character(seq(2027,2036)),'LT')
  rownames(SSB.rep2)<-c("SSB (kt)","SSB CV")
  
  #FBar median, 5th and 95th percentiles
  #2017 - 2026
  FBar.rep <- data.frame('2017' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2017',,,,]),digits=2)),
                         '2018' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2018',,,,]),digits=2)),
                         '2019' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2019',,,,]),digits=2)),
                         '2020' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2020',,,,]),digits=2)),
                         '2021' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2021',,,,]),digits=2)),
                         '2022' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2022',,,,]),digits=2)),
                         '2023' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2023',,,,]),digits=2)),
                         '2024' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2024',,,,]),digits=2)),
                         '2025' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2025',,,,]),digits=2)),
                         '2026' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2026',,,,]),digits=2)),
                         'ST' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'ST',,,,]),digits=2)),
                         'MT' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'MT',,,,]),digits=2)),
                         stringsAsFactors = FALSE)
  
  colnames(FBar.rep)<-c(as.character(seq(2017,2026)),'ST','MT')
  rownames(FBar.rep)<-c("FBar","FBar (5%)","FBar (95%)")
  
  #2027-2036
  FBar.rep2 <- data.frame('2027' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2027',,,,]),digits=2)),
                           '2028' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2028',,,,]),digits=2)),
                           '2029' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2029',,,,]),digits=2)),
                           '2030' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2030',,,,]),digits=2)),
                           '2031' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2031',,,,]),digits=2)),
                           '2032' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2032',,,,]),digits=2)),
                           '2033' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2033',,,,]),digits=2)),
                           '2034' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2034',,,,]),digits=2)),
                           '2035' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2035',,,,]),digits=2)),
                           '2036' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2036',,,,]),digits=2)),
                           'LT' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'LT',,,,]),digits=2)),
                           stringsAsFactors = FALSE)
  
  colnames(FBar.rep2)<-c(as.character(seq(2027,2036)),'LT')
  rownames(FBar.rep2)<-c("FBar","FBar (5%)","FBar (95%)")
  
  #Yield 2017 - 2026
  Yld.rep <- data.frame('2017' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2017',,,,])/1e3,digits=1)),
                        '2018' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2018',,,,])/1e3,digits=1)),
                        '2019' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2019',,,,])/1e3,digits=1)),
                        '2020' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2020',,,,])/1e3,digits=1)),
                        '2021' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2021',,,,])/1e3,digits=1)),
                        '2022' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2022',,,,])/1e3,digits=1)),
                        '2023' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2023',,,,])/1e3,digits=1)),
                        '2024' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2024',,,,])/1e3,digits=1)),
                        '2025' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2025',,,,])/1e3,digits=1)),
                        '2026' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2026',,,,])/1e3,digits=1)),
                        'ST' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'ST',,,,])/1e3,digits=1)),
                        'MT' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'MT',,,,])/1e3,digits=1)),
                        stringsAsFactors = FALSE)
  
  colnames(Yld.rep)<-c(as.character(seq(2017,2026)),'ST','MT')
  rownames(Yld.rep)<-c("Yld (kt)","Yld (5%)","Yld (95%)")

  #Yield 2027 - 2036
  Yld.rep2 <- data.frame('2027' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2027',,,,])/1e3,digits=1)),
                        '2028' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2028',,,,])/1e3,digits=1)),
                        '2029' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2029',,,,])/1e3,digits=1)),
                        '2030' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2030',,,,])/1e3,digits=1)),
                        '2031' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2031',,,,])/1e3,digits=1)),
                        '2032' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2032',,,,])/1e3,digits=1)),
                        '2033' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2033',,,,])/1e3,digits=1)),
                        '2034' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2034',,,,])/1e3,digits=1)),
                        '2035' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2035',,,,])/1e3,digits=1)),
                        '2036' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2036',,,,])/1e3,digits=1)),
                        'LT' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'LT',,,,])/1e3,digits=1)),
                        stringsAsFactors = FALSE)
  
  colnames(Yld.rep2)<-c(as.character(seq(2027,2036)),'LT')
  rownames(Yld.rep2)<-c("Yld (kt)","Yld (5%)","Yld (95%)")
  
  #Inter Annual Variability
  #2017 - 2026
  IAV.rep <- data.frame('2017' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2017',,,,])/1e3,digits=1)),
                        '2018' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2018',,,,])/1e3,digits=1)),
                        '2019' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2019',,,,])/1e3,digits=1)),
                        '2020' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2020',,,,])/1e3,digits=1)),
                        '2021' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2021',,,,])/1e3,digits=1)),
                        '2022' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2022',,,,])/1e3,digits=1)),
                        '2023' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2023',,,,])/1e3,digits=1)),
                        '2024' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2024',,,,])/1e3,digits=1)),
                        '2025' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2025',,,,])/1e3,digits=1)),
                        '2026' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2026',,,,])/1e3,digits=1)),
                        'ST' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'ST',,,,])/1e3,digits=1)),
                        'MT' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'MT',,,,])/1e3,digits=1)),
                        stringsAsFactors = FALSE)
  
  colnames(IAV.rep)<-c(as.character(seq(2017,2026)),'ST','MT')
  rownames(IAV.rep)<-c("IAV (kt)","IAV (5%)","IAV (95%)")

  
  #2027- 2036
  IAV.rep2 <- data.frame('2027' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2027',,,,])/1e3,digits=1)),
                        '2028' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2028',,,,])/1e3,digits=1)),
                        '2029' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2029',,,,])/1e3,digits=1)),
                        '2030' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2030',,,,])/1e3,digits=1)),
                        '2031' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2031',,,,])/1e3,digits=1)),
                        '2032' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2032',,,,])/1e3,digits=1)),
                        '2033' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2033',,,,])/1e3,digits=1)),
                        '2034' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2034',,,,])/1e3,digits=1)),
                        '2035' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2035',,,,])/1e3,digits=1)),
                        '2036' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2036',,,,])/1e3,digits=1)),
                        'LT' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'LT',,,,])/1e3,digits=1)),
                        stringsAsFactors = FALSE)
  
  colnames(IAV.rep2)<-c(as.character(seq(2027,2036)),'LT')
  rownames(IAV.rep2)<-c("IAV (kt)","IAV (5%)","IAV (95%)")
  
    
  Risk.rep <- data.frame('2017' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2017',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2017',,,,]),digits=0))),
                         '2018' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2018',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2018',,,,]),digits=0))),
                         '2019' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2019',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2019',,,,]),digits=0))),
                         '2020' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2020',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2020',,,,]),digits=0))),
                         '2021' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2021',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2021',,,,]),digits=0))),
                         '2022' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2022',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2022',,,,]),digits=0))),
                         '2023' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2023',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2023',,,,]),digits=0))),
                         '2024' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2024',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2024',,,,]),digits=0))),
                         '2025' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2025',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2025',,,,]),digits=0))),
                         '2026' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2026',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2026',,,,]),digits=0))),
                         'ST' = c(ac(round(100*as.numeric(d$pBlim$val[c('max'),'ST',,,,]),digits=0)),
                                  ac(round(100*as.numeric(d$pBpa$val[c('max'),'ST',,,,]),digits=0))),
                         'MT' = c(ac(round(100*as.numeric(d$pBlim$val[c('max'),'MT',,,,]),digits=0)),
                                  ac(round(100*as.numeric(d$pBpa$val[c('max'),'MT',,,,]),digits=0))),
                         stringsAsFactors = FALSE)

  colnames(Risk.rep)<-c(as.character(seq(2017,2026)),'ST','MT')
  rownames(Risk.rep)<-c("Blim (%)","Bpa (%)")

  
  Risk.rep2 <- data.frame('2027' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2027',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2027',,,,]),digits=0))),
                         '2028' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2028',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2028',,,,]),digits=0))),
                         '2029' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2029',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2029',,,,]),digits=0))),
                         '2030' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2030',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2030',,,,]),digits=0))),
                         '2031' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2031',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2031',,,,]),digits=0))),
                         '2032' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2032',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2032',,,,]),digits=0))),
                         '2033' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2033',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2033',,,,]),digits=0))),
                         '2034' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2034',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2034',,,,]),digits=0))),
                         '2035' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2035',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2035',,,,]),digits=0))),
                         '2036' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2036',,,,]),digits=0)),
                                    ac(round(100*as.numeric(d$pBpa$val[c('median'),'2036',,,,]),digits=0))),
                         'LT' = c(ac(round(100*as.numeric(d$pBlim$val[c('max'),'LT',,,,]),digits=0)),
                                  ac(round(100*as.numeric(d$pBpa$val[c('max'),'LT',,,,]),digits=0))),
                         stringsAsFactors = FALSE)
  
  colnames(Risk.rep2)<-c(as.character(seq(2027,2036)),'LT')
  rownames(Risk.rep2)<-c("Blim (%)","Bpa (%)")
  
  #colour Blim risk cells according to 5% limit
  #Risk.rep["Blim (%)",Risk.rep["Blim (%)",]>5] <- paste("risky",Risk.rep["Blim (%)",Risk.rep["Blim (%)",]>5])
  
  Ext.rep <- data.frame('2017' = c(ac(100*as.numeric(d$pExt$val['2017']))),
                        '2018' = c(ac(100*as.numeric(d$pExt$val['2018']))),
                        '2019' = c(ac(100*as.numeric(d$pExt$val['2019']))),
                        '2020' = c(ac(100*as.numeric(d$pExt$val['2020']))),
                        '2021' = c(ac(100*as.numeric(d$pExt$val['2021']))),
                        '2022' = c(ac(100*as.numeric(d$pExt$val['2022']))),
                        '2023' = c(ac(100*as.numeric(d$pExt$val['2023']))),
                        '2024' = c(ac(100*as.numeric(d$pExt$val['2024']))),
                        '2025' = c(ac(100*as.numeric(d$pExt$val['2025']))),
                        '2026' = c(ac(100*as.numeric(d$pExt$val['2026']))),
                        'ST' = c(ac(100*as.numeric(d$pExt$val['ST']))),
                        'MT' = c(ac(100*as.numeric(d$pExt$val['MT']))),
                        stringsAsFactors = FALSE,
                        row.names = c("BExt (%)"))
  
  colnames(Ext.rep)<-c(as.character(seq(2017,2026)),'ST','MT')

  
  Ext.rep2 <- data.frame('2027' = c(ac(100*as.numeric(d$pExt$val['2027']))),
                        '2028' = c(ac(100*as.numeric(d$pExt$val['2028']))),
                        '2029' = c(ac(100*as.numeric(d$pExt$val['2029']))),
                        '2030' = c(ac(100*as.numeric(d$pExt$val['2030']))),
                        '2031' = c(ac(100*as.numeric(d$pExt$val['2031']))),
                        '2032' = c(ac(100*as.numeric(d$pExt$val['2032']))),
                        '2033' = c(ac(100*as.numeric(d$pExt$val['2033']))),
                        '2034' = c(ac(100*as.numeric(d$pExt$val['2034']))),
                        '2035' = c(ac(100*as.numeric(d$pExt$val['2035']))),
                        '2036' = c(ac(100*as.numeric(d$pExt$val['2036']))),
                        'LT' = c(ac(100*as.numeric(d$pExt$val['LT']))),
                        stringsAsFactors = FALSE,
                        row.names = c("BExt (%)"))
  
  colnames(Ext.rep2)<-c(as.character(seq(2027,2036)),'LT')
  
  #individual tables
  #ssb.tab <- xtable(SSB.rep)
  #ssb.tab2 <- xtable(SSB.rep.2)
  #Yld.tab <- xtable(Yld.rep)
  #Risk.tab <- xtable(Risk.rep)
  #Ext.tab <- xtable(Ext.rep)
  
  #combined (neater)
  All.rep <- dplyr::bind_rows(SSB.rep,FBar.rep)
  All.rep <- dplyr::bind_rows(All.rep,Yld.rep)
  All.rep <- dplyr::bind_rows(All.rep,IAV.rep)
  All.rep <- dplyr::bind_rows(All.rep,Risk.rep)
  All.rep <- dplyr::bind_rows(All.rep,Ext.rep)
  #All.tab <- xtable(All.rep, caption = paste0("F = ",n))
  #All.tab <- xtable(All.rep)

  All.rep2 <- dplyr::bind_rows(SSB.rep2,FBar.rep2)
  All.rep2 <- dplyr::bind_rows(All.rep2,Yld.rep2)
  All.rep2 <- dplyr::bind_rows(All.rep2,IAV.rep2)
  All.rep2 <- dplyr::bind_rows(All.rep2,Risk.rep2)
  All.rep2 <- dplyr::bind_rows(All.rep2,Ext.rep2)

  
  #rownames(All.tab) <- c("SSB (kt)","SSB CV","FBar","FBar (5%)","FBar (95%)","Yld (kt)","Yld (5%)","Yld (95%)","IAV (kt)","IAV (5%)","IAV (95%)","Blim (%)","Bpa (%)","Ext (%)")
  #rownames(All.rep) <- c("SSB (kt)","SSB CV","FBar","FBar (5%)","FBar (95%)","Yld (kt)","Yld (5%)","Yld (95%)","IAV (kt)","IAV (5%)","IAV (95%)","Blim (%)","Bpa (%)","Ext (%)")
  rownames(All.rep) <- c("SSB (kt)","SSB CV","FBar","FBar (5\\%)","FBar (95\\%)","Yld (kt)","Yld (5\\%)","Yld (95\\%)","IAV (kt)","IAV (5\\%)","IAV (95\\%)","Blim (\\%)","Bpa (\\%)","Ext (\\%)")
  rownames(All.rep2) <- c("SSB (kt)","SSB CV","FBar","FBar (5\\%)","FBar (95\\%)","Yld (kt)","Yld (5\\%)","Yld (95\\%)","IAV (kt)","IAV (5\\%)","IAV (95\\%)","Blim (\\%)","Bpa (\\%)","Ext (\\%)")
  
  #All.rep.2 <- dplyr::bind_rows(SSB.rep.2,FBar.rep.2)
  #All.rep.2 <- dplyr::bind_rows(All.rep.2,Yld.rep.2)
  #All.tab.2 <- xtable(All.rep.2, caption = paste0("F = ",n))
  #rownames(All.tab.2) <- c("SSB (kt)","SSB CV","FBar","FBar (5%)","FBar (95%)","Yld (kt)","Yld (5%)","Yld (95%)")
  
  #documentclass <- paste0("\\","documentclass{article}")
  #package <- paste0("\\","usepackage[landscape]{geometry}")
  #package2 <- paste0("\\","usepackage[table]{xcolor}")

  #begindoc <- paste0("\\","begin{document}")
  #beginfont <- paste0("\\","begin{tiny}")
  #endfont <- paste0("\\","end{tiny}")
  #enddoc <- paste0("\\","end{document}")
  #risky <- paste0("\\","cellcolor{red}")
  #risky <- cat("\\","cellcolor{red}",sep="")
   
  #write(documentclass,file=fTex,append=TRUE)
  #write(package,file=fTex,append=TRUE)
  #write(package2,file=fTex,append=TRUE)
  #write(begindoc,file=fTex,append=TRUE)
  #write(beginfont,file=fTex,append=TRUE)
  #write(print.xtable(All.tab, type="latex", hline.after=c(-1,0,2,5,8,11,nrow(All.rep)), print.results = FALSE),file=fTex,append=TRUE)
  #optab <- xtable(All.tab, type="latex", print.results = FALSE, caption=paste0("F=",n," Short Term"))
  #tt <- All.rep["Blim (%)",]
  
  #colour risk cells
  All.rep["Blim (\\%)",] <- lapply(as.numeric(All.rep["Blim (\\%)",]), function(x)
    paste0("\\cellcolor{", fRiskColour(x), "}", x))
  All.rep2["Blim (\\%)",] <- lapply(as.numeric(All.rep2["Blim (\\%)",]), function(x)
    paste0("\\cellcolor{", fRiskColour(x), "}", x))
  
  #tt <- stringr::str_replace_all(tt,"risky",risky)
  #tt <- sanitize(tt,type="latex")
  #All.rep["Blim (%)",]<- tt
  
  
  #optab <- xtable(All.rep, type="latex", print.results = FALSE, caption=paste0("F=",n," Short Term"))
  #transform to latex format
  optab <- xtable(All.rep, 
                  type="latex",
                  caption=paste0(OM,",HCR",HCR," F=",n," Short/Medium Term"))
  align(optab) <- "rlllll|lllll|l|l"
  optab2 <- xtable(All.rep2, 
                  type="latex",
                  caption=paste0(OM,",HCR",HCR," F=",n," Long Term"))
  align(optab2) <- "rllllllllll|l"

  
  #write(print(optab, type="latex", hline.after=c(-1,0,2,5,8,11,nrow(All.rep))),file=fTex,append=TRUE)
  write(print(optab, type="latex",
              hline.after=c(-1,0,2,5,8,11,nrow(All.rep)),
              sanitize.text.function = identity),
        file=fTex,append=TRUE)

  write(print(optab2, type="latex",
              hline.after=c(-1,0,2,5,8,11,nrow(All.rep2)),
              sanitize.text.function = identity),
        file=fTex,append=TRUE)

  #MSY results
  if (MSY) {
    #FMSY runs, final 10 years
    #2207 - 2216
    SSB.rep3 <- data.frame('2207' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2207',,,,])/1e3,digits=1)),
                                      ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2207',,,,]),digits=0))),
                           '2208' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2208',,,,])/1e3,digits=1)),
                                      ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2208',,,,]),digits=0))),
                           '2209' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2209',,,,])/1e3,digits=1)),
                                      ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2209',,,,]),digits=0))),
                           '2210' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2210',,,,])/1e3,digits=1)),
                                      ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2210',,,,]),digits=0))),
                           '2211' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2211',,,,])/1e3,digits=1)),
                                      ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2211',,,,]),digits=0))),
                           '2212' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2212',,,,])/1e3,digits=1)),
                                      ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2212',,,,]),digits=0))),
                           '2213' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2213',,,,])/1e3,digits=1)),
                                      ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2213',,,,]),digits=0))),
                           '2214' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2214',,,,])/1e3,digits=1)),
                                      ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2214',,,,]),digits=0))),
                           '2215' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2215',,,,])/1e3,digits=1)),
                                      ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2215',,,,]),digits=0))),
                           '2216' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'2216',,,,])/1e3,digits=1)),
                                      ac(round(100.*as.numeric(d$SSB$val[c('CV'),'2216',,,,]),digits=0))),
                           'ET' = c(ac(round(as.numeric(d$SSB$val[c('50%'),'ET',,,,])/1e3,digits=1)),
                                    ac(round(100.*as.numeric(d$SSB$val[c('CV'),'ET',,,,]),digits=0))),
                           stringsAsFactors = FALSE)
    
    colnames(SSB.rep3)<-c(as.character(seq(2207,2216)),'ET')
    rownames(SSB.rep3)<-c("SSB (kt)","SSB CV")

    FBar.rep3 <- data.frame('2207' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2207',,,,]),digits=2)),
                            '2208' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2208',,,,]),digits=2)),
                            '2209' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2209',,,,]),digits=2)),
                            '2210' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2210',,,,]),digits=2)),
                            '2211' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2211',,,,]),digits=2)),
                            '2212' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2212',,,,]),digits=2)),
                            '2213' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2213',,,,]),digits=2)),
                            '2214' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2214',,,,]),digits=2)),
                            '2215' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2215',,,,]),digits=2)),
                            '2216' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'2216',,,,]),digits=2)),
                            'ET' = ac(round(as.numeric(d$Fbar$val[c('50%','5%','95%'),'ET',,,,]),digits=2)),
                            stringsAsFactors = FALSE)
    
    colnames(FBar.rep3)<-c(as.character(seq(2207,2216)),'ET')
    rownames(FBar.rep3)<-c("FBar","FBar (5%)","FBar (95%)")
    
    #Yield 2207 - 2216
    Yld.rep3 <- data.frame('2207' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2207',,,,])/1e3,digits=1)),
                           '2208' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2208',,,,])/1e3,digits=1)),
                           '2209' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2209',,,,])/1e3,digits=1)),
                           '2210' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2210',,,,])/1e3,digits=1)),
                           '2211' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2211',,,,])/1e3,digits=1)),
                           '2212' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2212',,,,])/1e3,digits=1)),
                           '2213' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2213',,,,])/1e3,digits=1)),
                           '2214' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2214',,,,])/1e3,digits=1)),
                           '2215' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2215',,,,])/1e3,digits=1)),
                           '2216' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'2216',,,,])/1e3,digits=1)),
                           'ET' = ac(round(as.numeric(d$Catch$val[c('50%','5%','95%'),'ET',,,,])/1e3,digits=1)),
                           stringsAsFactors = FALSE)
    
    colnames(Yld.rep3)<-c(as.character(seq(2207,2216)),'ET')
    rownames(Yld.rep3)<-c("Yld (kt)","Yld (5%)","Yld (95%)")
    
    IAV.rep3 <- data.frame('2207' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2207',,,,])/1e3,digits=1)),
                           '2208' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2208',,,,])/1e3,digits=1)),
                           '2209' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2209',,,,])/1e3,digits=1)),
                           '2210' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2210',,,,])/1e3,digits=1)),
                           '2211' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2211',,,,])/1e3,digits=1)),
                           '2212' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2212',,,,])/1e3,digits=1)),
                           '2213' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2213',,,,])/1e3,digits=1)),
                           '2214' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2214',,,,])/1e3,digits=1)),
                           '2215' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2215',,,,])/1e3,digits=1)),
                           '2216' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'2216',,,,])/1e3,digits=1)),
                           'ET' = ac(round(as.numeric(d$TACchange$val[c('50%','5%','95%'),'ET',,,,])/1e3,digits=1)),
                           stringsAsFactors = FALSE)
    
    colnames(IAV.rep3)<-c(as.character(seq(2207,2216)),'ET')
    rownames(IAV.rep3)<-c("IAV (kt)","IAV (5%)","IAV (95%)")
    

    Risk.rep3 <- data.frame('2207' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2207',,,,]),digits=0)),
                                       ac(round(100*as.numeric(d$pBpa$val[c('median'),'2207',,,,]),digits=0))),
                            '2208' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2208',,,,]),digits=0)),
                                       ac(round(100*as.numeric(d$pBpa$val[c('median'),'2208',,,,]),digits=0))),
                            '2209' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2209',,,,]),digits=0)),
                                       ac(round(100*as.numeric(d$pBpa$val[c('median'),'2209',,,,]),digits=0))),
                            '2210' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2210',,,,]),digits=0)),
                                       ac(round(100*as.numeric(d$pBpa$val[c('median'),'2210',,,,]),digits=0))),
                            '2211' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2211',,,,]),digits=0)),
                                       ac(round(100*as.numeric(d$pBpa$val[c('median'),'2211',,,,]),digits=0))),
                            '2212' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2212',,,,]),digits=0)),
                                       ac(round(100*as.numeric(d$pBpa$val[c('median'),'2212',,,,]),digits=0))),
                            '2213' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2213',,,,]),digits=0)),
                                       ac(round(100*as.numeric(d$pBpa$val[c('median'),'2213',,,,]),digits=0))),
                            '2214' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2214',,,,]),digits=0)),
                                       ac(round(100*as.numeric(d$pBpa$val[c('median'),'2214',,,,]),digits=0))),
                            '2215' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2215',,,,]),digits=0)),
                                       ac(round(100*as.numeric(d$pBpa$val[c('median'),'2215',,,,]),digits=0))),
                            '2216' = c(ac(round(100*as.numeric(d$pBlim$val[c('median'),'2216',,,,]),digits=0)),
                                       ac(round(100*as.numeric(d$pBpa$val[c('median'),'2216',,,,]),digits=0))),
                            'ET' = c(ac(round(100*as.numeric(d$pBlim$val[c('max'),'ET',,,,]),digits=0)),
                                     ac(round(00*as.numeric(d$pBpa$val[c('max'),'ET',,,,]),digits=0))),
                            stringsAsFactors = FALSE)
    
    colnames(Risk.rep3)<-c(as.character(seq(2207,2216)),'ET')
    rownames(Risk.rep3)<-c("Blim (%)","Bpa (%)")

    Ext.rep3 <- data.frame('2207' = c(ac(100*as.numeric(d$pExt$val['2207']))),
                           '2208' = c(ac(100*as.numeric(d$pExt$val['2208']))),
                           '2209' = c(ac(100*as.numeric(d$pExt$val['2209']))),
                           '2210' = c(ac(100*as.numeric(d$pExt$val['2210']))),
                           '2211' = c(ac(100*as.numeric(d$pExt$val['2211']))),
                           '2212' = c(ac(100*as.numeric(d$pExt$val['2212']))),
                           '2213' = c(ac(100*as.numeric(d$pExt$val['2213']))),
                           '2214' = c(ac(100*as.numeric(d$pExt$val['2214']))),
                           '2215' = c(ac(100*as.numeric(d$pExt$val['2215']))),
                           '2216' = c(ac(100*as.numeric(d$pExt$val['2216']))),
                           'ET' = c(ac(100*as.numeric(d$pExt$val['ET']))),
                           stringsAsFactors = FALSE,
                           row.names = c("BExt (%)"))
    
    colnames(Ext.rep3)<-c(as.character(seq(2207,2216)),'ET')
    
    All.rep3 <- dplyr::bind_rows(SSB.rep3,FBar.rep3)
    All.rep3 <- dplyr::bind_rows(All.rep3,Yld.rep3)
    All.rep3 <- dplyr::bind_rows(All.rep3,IAV.rep3)
    All.rep3 <- dplyr::bind_rows(All.rep3,Risk.rep3)
    All.rep3 <- dplyr::bind_rows(All.rep3,Ext.rep3)

    rownames(All.rep3) <- c("SSB (kt)","SSB CV","FBar","FBar (5\\%)","FBar (95\\%)","Yld (kt)","Yld (5\\%)","Yld (95\\%)","IAV (kt)","IAV (5\\%)","IAV (95\\%)","Blim (\\%)","Bpa (\\%)","Ext (\\%)")
    
    All.rep3["Blim (\\%)",] <- lapply(as.numeric(All.rep3["Blim (\\%)",]), function(x)
      paste0("\\cellcolor{", fRiskColour(x), "}", x))
    
    optab3 <- xtable(All.rep3, 
                     type="latex",
                     caption=paste0(OM,",HCR",HCR," F=",n," Long Term"))
    align(optab3) <- "rllllllllll|l"

    write(print(optab3, type="latex",
                hline.after=c(-1,0,2,5,8,11,nrow(All.rep3)),
                sanitize.text.function = identity),
          file=fTex,append=TRUE)
    
  }
  
  
  
  #write(print.xtable(All.tab.2, type="latex", print.results = FALSE),file=fTex,append=TRUE)
  #write(endfont,file=fTex,append=TRUE)
  #write(enddoc,file=fTex,append=TRUE)
  
  #generate the pdf
  #tools::texi2pdf(file=fTex, clean=TRUE)
  
  #move the putput file into the results directory
  #file.copy(from = wdfpdf, to = fpdf)
  
  #remove the tex source file and original pdf
  #file.remove(fTex)
  #file.remove(wdfpdf)
  
  #write a tex version for inclusion in the report
  #write(print(optab, type="latex", 
  #            hline.after=c(-1,0,2,5,8,11,nrow(All.rep)),
  #            sanitize()),
  #      file=fTex,append=TRUE)
  
  # write(print(optab, type="latex", 
  #             hline.after=c(-1,0,2,5,8,11,nrow(All.rep)),
  #             sanitize.text.function = identity),
  #       file=fTex,append=TRUE)
  # 
}