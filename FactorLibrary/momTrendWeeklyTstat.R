#  source("C:\\R Toolbox\\FactorLibrary\\momTrendWeeklyTstat.R")

momTrendWeeklyTstat <- function(secList, dt, priceData){
# regress weekly returns against time over 1yr time period. return tstat 

numPeriods  <- 53
periodUnit  <- "1 weeks"
naCutoff    <- numPeriods/2 # dont use if half the prices for a stock are NA

seqUnit     <- paste("-", periodUnit, sep="")

dates 	<- dimnames(priceData)[[1]]
tickers <- dimnames(priceData)[[2]]

dt          <- as.Date(as.character(dt), "%Y%m%d")
dtSeq       <- seq(dt, len=numPeriods, by=seqUnit)
dtSeq       <- as.integer(format(dtSeq, "%Y%m%d"))

secIdx 	<- match(secList, tickers)
dtIdx    <- match(dtSeq, dates)

x <- numPeriods:1
y <- priceData[dtIdx,secIdx]
naIdx <- apply(is.na(y), 2, sum) > naCutoff
y <- y[,!naIdx]

regObj <- lm(y~x, na.action=na.omit)

mat <- NULL
for (i in 1:length(secList)){
   if (naIdx[i] ==F){
      temp  <- coef(summary(regObj))[[i]][2,3]
   } else{
      temp  <- NA
   }
   
   mat   <- cbind(mat, temp)
}

dimnames(mat)[[2]] <- secList
mat
}