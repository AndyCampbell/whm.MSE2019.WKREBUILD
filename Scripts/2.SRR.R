#SRR.R

#Stock-recruit investigations

#initially stock with a segmented regression fit to the complete dataset with a breakpoint at Bloss (Blim)
Blim <- min(ssb(WG18))
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= Blim, ab$a * Blim, ab$a * ssb))

SRR.segreg.no82 <- eqsr_fit(WG18, remove.years = c(1982), nsamp=1000, models = c("SegregBlim"))
eqsr_plot(SRR.segreg.no82)

#save SRR fits
save(SRR.segreg.no82, file=file.path(RData.dir,"SRR.RData"))
