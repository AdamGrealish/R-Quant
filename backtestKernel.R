#  source("C:\\R Toolbox\\backtestKernel.R")

backtestKernel <- function(dateList, univList, factors, rebalFreq="1 months", returnFreq=1, implementationLag="1 days", spreadBuckets=5, factorTable="factor_scores"){


# Instantiate results matrices
spreads           <- matrix(NA, length(dateList), length(factors))
dimnames(spreads) <- list(format(dateList, format="%Y%m%d"), factors)
ics               <- spreads
hits              <- spreads
univSize          <- spreads
nObs              <- spreads
missing           <- spreads
factorCor         <- spreads

# Date loop
for (i in 1:(length(dateList)-1)){
   dtDate   <- as.Date(as.character(dateList[i]), format="%Y%m%d")
   dtStr    <- format(dtDate, format="%Y%m%d")
   dt       <- as.integer(dtStr)
   
   print(dt)
   
   # Make secList
   secList     <- univList[[i]]
   
   # Calc Forward Return
   # implementation lag of 1d. calc factor on dt (eod), look at return dt+1 -> dt+1m+1 (implemented at next day's close)
   # Only uses monthly returns right now!
   
   endDt        <- dateList[(i+1)]
   fwdRet       <- ret1mEnding(secList, endDt) 
   #endDt       <- seq.Date(dtDate, len=(returnFreq+1), by="1 months")[(returnFreq+1)]
   #fwdRet      <- ret1mEnding( secList, as.integer(format(endDt, format="%Y%m%d")) )
   
   # By Factor
   for (f in 1:length(factors)){
      # Create factor scores
      scoreObj <- eval( parse(text= paste("getFactorScores(secList, dt, ", "\"", factors[f], "\"", ", table=", "\"", factorTable, "\"", ")", sep="")))
      score    <- scoreObj$values
      
      # Data checks
      idx <- (!is.na(score) & !is.na(fwdRet))
      
      # Performance
      spreads[i, f]  <- quantileSpread(score[idx], fwdRet[idx], spreadBuckets)
      ics[i, f]      <- cor(score[idx], fwdRet[idx], method="spearman")
      hits[i, f]     <- hitRate(score[idx], fwdRet[idx])
      univSize[i, f] <- sum((!is.na(fwdRet)))
      nObs[i, f]     <- sum(idx)
      missing[i, f]  <- sum(is.na(score))
   }
}

# Store in data object
dataObj  <- NULL

dataObj$spreads   <- spreads
dataObj$ics       <- ics
dataObj$hits      <- hits
dataObj$univSize  <- univSize
dataObj$nObs      <- nObs
dataObj$missing   <- missing

dataObj$factors      <- factors
dataObj$dateList     <- dateList
dataObj$univList     <- univList

dataObj
}



