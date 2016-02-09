#  source("C:\\R Toolbox\\backtestKernel_beta.R")

backtestKernel <- function(dateList, univList, factors, rebalFreq="1 months", returnFreq=1, implementationLag="1 days", spreadBuckets=5, factorTable="factor_scores", partitionUnivList=NULL){

partitionUnivList <- c(list(univList), partitionUnivList)

# Instantiate results matrices
blankMat             <- matrix(NA, length(dateList), length(factors))
dimnames(blankMat)   <- list(format(dateList, format="%Y%m%d"), factors)
for (i in 1:length(partitionUnivList)){
   assign(paste("spreads",  i, sep="."), blankMat)
   assign(paste("ics",      i, sep="."), blankMat)
   assign(paste("hits",     i, sep="."), blankMat)
   assign(paste("univSize", i, sep="."), blankMat)
   assign(paste("nObs",     i, sep="."), blankMat)
   assign(paste("missing",  i, sep="."), blankMat)
}

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
      
      # By Parition
      for (p in 1:length(partitionUnivList)){
         thisPartition  <- partitionUnivList[[p]]
         partList       <- thisPartition[[i]]
         # Data checks
         #idx         <- (!is.na(score) & !is.na(fwdRet))
         validIdx    <- (!is.na(score) & !is.na(fwdRet))
         inPartition <- is.element(secList, partList)
         idx         <- (validIdx & inPartition)
         
         # Performance
         spreads        <- get(paste("spreads", p, sep="."))
         spreads[i, f]  <- quantileSpread(score[idx], fwdRet[idx], spreadBuckets)
         assign(paste("spreads", p, sep="."), spreads)
         
         ics            <- get(paste("ics", p, sep="."))
         ics[i, f]      <- cor(score[idx], fwdRet[idx], method="spearman")
         assign(paste("ics", p, sep="."), ics)
         
         hits           <- get(paste("hits", p, sep="."))
         hits[i, f]     <- hitRate(score[idx], fwdRet[idx])
         assign(paste("hits", p, sep="."), hits)
         
         univSize       <- get(paste("univSize", p, sep="."))
         univSize[i, f] <- sum((!is.na(fwdRet)))
         assign(paste("univSize", p, sep="."), univSize)
         
         nObs           <- get(paste("nObs", p, sep="."))
         nObs[i, f]     <- sum(idx)
         assign(paste("nObs", p, sep="."), nObs)
         
         missing        <- get(paste("missing", p, sep="."))
         missing[i, f]  <- sum(is.na(score) & inPartition)
         assign(paste("missing", p, sep="."), missing)
     }
   }
}

# Store in data object
superObj  <- NULL

for (p in 1:length(partitionUnivList)){
   dataObj           <- NULL
   
   dataObj$spreads   <- get(paste("spreads"  , p, sep="."))
   dataObj$ics       <- get(paste("ics"      , p, sep="."))
   dataObj$hits      <- get(paste("hits"     , p, sep="."))
   dataObj$univSize  <- get(paste("univSize" , p, sep="."))
   dataObj$nObs      <- get(paste("nObs"     , p, sep="."))
   dataObj$missing   <- get(paste("missing"  , p, sep="."))
   
   dataObj$factors   <- factors
   dataObj$dateList  <- dateList
   dataObj$univList  <- partitionUnivList[[p]]
   
   assign(paste("dataObj", p, sep="."), dataObj)
   superObj          <- c(superObj, list(get(paste("dataObj", p, sep="."))))
}

superObj
}



