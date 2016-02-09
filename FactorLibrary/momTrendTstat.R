#  source("C:\\R Toolbox\\FactorLibrary\\momTrendTstat.R")

momTrendTstat <- function(secList, dt, priceData){
# regress monthly returns against time over 1yr time period. return tstat

dates 	<- dimnames(priceData)[[1]]
tickers <- dimnames(priceData)[[2]]

dt          <- as.Date(as.character(dt), "%Y%m%d")
dtSeq       <- seq(dt, len=13, by="-1 months")
dtSeq       <- as.integer(format(dtSeq, "%Y%m%d"))

secIdx 	<- match(secList, tickers)
dtIdx    <- match(dtSeq, dates)

x <- 13:1
y <- priceData[dtIdx,secIdx]

regObj <- lm(y~x)

mat <- NULL
for (i in 1:length(secList)){
   temp  <- coef(summary(regObj))[[i]][2,3]
   mat   <- cbind(mat, temp)
}

dimnames(mat)[[2]] <- secList
mat


}