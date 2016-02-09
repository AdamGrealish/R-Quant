#  source("C:\\R Toolbox\\backtest.R")

# Include files
source("C:\\R Toolbox\\include.R")

# Load price data
priceData <- loadPriceData("F:\\Data\\InterDayData\\Last Price Split Adj.csv")

# Set Paramaters
startDate   <- 20070331
endDate     <- 20091231
secList     <- dimnames(priceData)[[2]]

rebalFreq   <- "1 months"
returnFreq  <- "1 months"
implementLag   <- "1 days"
spreadBuckets  <- 5

factors <- c("zBookToMV", "zNetIncomeToMV", "zMom12m", "zMomTrendTstat", "zModel")


# Load market data
capMatrix <- loadPriceData("F:\\Data\\InterDayData\\CurMktCap.csv")

# Load financial statement data
netIncomeMatrix   <- loadFSData("F:\\Data\\InterDayData\\NetIncome.csv")
bookMatrix        <- loadFSData("F:\\Data\\InterDayData\\Shareholdersequity.csv")

# Load financial statement availability
availMatrix <- loadFSAvailabiltyData("F:\\Data\\InterDayData\\AnncDates.csv")

# Load GICS data

# Instantiate results matrices
spreads           <- matrix(NA, length(dateSeq), length(factors))
dimnames(spreads) <- list(format(dateSeq, format="%Y%m%d"), factors)
ics               <- spreads
hits              <- spreads
univSize          <- spreads
nObs              <- spreads
missing           <- spreads
factorCor         <- spreads

# Create date sequence
dateSeq <- seq.Date(as.Date(as.character(startDate), format="%Y%m%d"), as.Date(as.character(endDate), format="%Y%m%d"), rebalFreq)

# Date loop
for (i in 1:length(dateSeq)){
   dt    <- dateSeq[i]
   dtStr <- format(dt, format="%Y%m%d")
   dtInt <- as.integer(dtStr)
   
   print(dtStr)
   
#   dataAtDt <- rbind(
#               priceData[match(dtStr, dimnames(priceData)[[1]]),, drop=F],
#               capMatrix[match(as.character(mostRecentDate(dtStr,dimnames(capMatrix)[[1]])), dimnames(capMatrix)[[1]]),, drop=F]
#               )
   
   # Calc Forward Return
   # implementation lag of 1d. calc factor on dt (eod), look at return dt+1 -> dt+1m+1 (implemented at next day's close)
   tradeStart  <- seq.Date(dt,         len=2,by=implementLag)[2]
   tradeEnd    <- seq.Date(tradeStart, len=2,by=returnFreq)[2]
   fwdRet      <- ret(secList, as.integer(format(tradeStart,format="%Y%m%d")), as.integer(format(tradeEnd,format="%Y%m%d")), priceData)
   
   # Investment Factors
   # Value
   zNetIncomeToMV    <- zscore(sum4QFinancialOverMarket(secList, dtInt, netIncomeMatrix, capMatrix, availMatrix), na.rm=T)
   zBookToMV         <- zscore(lastFinancialOverMarket( secList, dtInt, bookMatrix,      capMatrix, availMatrix), na.rm=T)
   # Momentum
   zMom12m           <- zscore(mom12(secList, dtInt, priceData), na.rm=T)
   zMomTrendTstat    <- zscore(momTrendTstat(secList, dtInt, priceData), na.rm=T)
   
   # Model
   model             <- .5*zNetIncomeToMV  + .5*zMom12m 
   zModel            <- zscore(model , na.rm=T)
   
   
   # Calculate performance
   # define universe with data checks
   idx <- (!is.na(get(factors[f])) & !is.na(fwdRet))
   
   # By Factor
   for (f in 1:length(factors)){
      spreads[i, f]  <- quantileSpread(get(factors[f])[idx], fwdRet[idx], spreadBuckets)
      ics[i, f]      <- cor(get(factors[f])[idx], fwdRet[idx], method="spearman")
      hits[i, f]     <- hitRate(get(factors[f])[idx], fwdRet[idx])
      univSize[i, f] <- sum((!is.na(fwdRet)))
      nObs[i, f]     <- sum(idx)
      missing[i, f]  <- sum(is.na(get(factors[f])))
      factorCor[i, f] <- cor(zNetIncomeToMV[idx], get(factors[f])[idx])
   }
}

# Visualize results
plot(cumsum(spreads[,1]), type="l", col="black", axes=FALSE, ann=FALSE)
axis(1, at=seq(1,dim(spreads)[1],by=3), lab=dimnames(spreads)[[1]][((0:(dim(spreads)[1]-1)) %% 3)==0], las=2)
#axis(2, round(seq(min(cumsum(spreads[,3]), na.rm=T), max(cumsum(spreads[,1]), na.rm=T), by=10),-1), las=1)
axis(2, round(seq(-120, 60, by=10),-1), las=1)

lines(cumsum(spreads[,2]), type="l", col="red")
lines(cumsum(spreads[,3]), type="l", col="blue")
