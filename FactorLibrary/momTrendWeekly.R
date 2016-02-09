#  source("C:\\R Toolbox\\FactorLibrary\\momTrendWeekly.R")

momTrendWeekly <- function(secList, dt, priceData){
# regress weekly returns against time over 1yr time period. return slope coeff

dates 	<- dimnames(priceData)[[1]]
tickers <- dimnames(priceData)[[2]]

dt          <- as.Date(as.character(dt), "%Y%m%d")
dtSeq       <- seq(dt, len=53, by="-1 weeks")
dtSeq       <- as.integer(format(dtSeq, "%Y%m%d"))

secIdx 	<- match(secList, tickers)
dtIdx    <- match(dtSeq, dates)

x <- 53:1
y <- priceData[dtIdx,secIdx]

regObj <- lm(y~x)

coef(regObj)[2,, drop=F]

}