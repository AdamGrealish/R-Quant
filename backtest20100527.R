#  source("C:\\R Toolbox\\backtest.R")

# Include files
source("C:\\R Toolbox\\include.R")

###############################
# Set Paramaters
###############################
#startDate   <- 19920131
startDate   <- 20061231
endDate     <- 20100331
univId      <- "I0003"

rebalFreq   <- "1 months"
returnFreq  <- 1   #number of months
implementLag   <- "1 days"
spreadBuckets  <- 5

factors <- c("bookToMV", "earningsBeforeExtraOrdinaryItemsToMV", "piotroski")
################################

# Load GICS data

# Create date sequence
dateSeq <- seq.Date(as.Date(as.character(startDate), format="%Y%m%d"), as.Date(as.character(endDate), format="%Y%m%d"), rebalFreq)

# Instantiate results matrices
spreads           <- matrix(NA, length(dateSeq), length(factors))
dimnames(spreads) <- list(format(dateSeq, format="%Y%m%d"), factors)
ics               <- spreads
hits              <- spreads
univSize          <- spreads
nObs              <- spreads
missing           <- spreads
factorCor         <- spreads


# Date loop
for (i in 1:length(dateSeq)){
   dtDate   <- dateSeq[i]
   dtStr    <- format(dtDate, format="%Y%m%d")
   dt       <- as.integer(dtStr)
   
   print(dt)
   
   # Make secList
   secList     <- indexMembers(univId, dt)
   bp          <- getFactorScores(secList, dt, "bookToMV", table="factor_scores_sp500")$values
   secList     <- secList[bp<=median(bp, na.rm=T)]
   secList     <- secList[!is.na(secList)]
   
   # Calc Forward Return
   # implementation lag of 1d. calc factor on dt (eod), look at return dt+1 -> dt+1m+1 (implemented at next day's close)
   # Only uses monthly returns right now!
   endDt       <- seq.Date(dtDate, len=(returnFreq+1), by="1 months")[(returnFreq+1)]
   fwdRet      <- ret1mEnding( secList, as.integer(format(endDt, format="%Y%m%d")) )
   
   # By Factor
   for (f in 1:length(factors)){
      # Create factor scores
      scoreObj <- eval( parse(text= paste("getFactorScores(secList, dt, ", "\"", factors[f], "\"", ", table=", "\"", "factor_scores_sp500", "\"", ")", sep="")))
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

# Visualize results
plot(cumsum(spreads[,1]), type="l", col="black", axes=FALSE, ann=FALSE)
incr  <- 12
axis(1, at=seq(1,dim(spreads)[1],by=incr), lab=dimnames(spreads)[[1]][((0:(dim(spreads)[1]-1)) %% incr)==0], las=2)
#axis(2, round(seq(min(cumsum(spreads[,3]), na.rm=T), max(cumsum(spreads[,1]), na.rm=T), by=10),-1), las=1)
axis(2, round(seq(-800, 800, by=10),-1), las=1)

lines(cumsum(spreads[,2]), type="l", col="red")
lines(cumsum(spreads[,3]), type="l", col="blue")
