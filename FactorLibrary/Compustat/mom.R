#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\mom.R")


mom <- function(secList, dt, nMonths=11, lag=1){

# Total return for stocks in secList for the nMonth up to dt. 
# Defaults to 12 month momentum excluding the most recent month.
#
# Example:
# 
# dt          <- 19930101
# secList     <- indexMembers("I0003", dt)
# dataObj     <- mom(secList, dt)
# mom12         <- dataObj$values 
#

# lag starting date
#startDate   <- seq.Date(as.Date(as.character(dt), format="%Y%m%d"), len=2, by=paste("-", lag, " months", sep=""))[2]

queryStr       <- "select distinct datadate from compustat_monthly_pricing"
fullDateList   <- sqlQuery(csDB, queryStr)[,"datadate"]
dateList       <- sort(fullDateList[fullDateList <= dt], decreasing=T) # go all the way back from dt, most recent first
dateList       <- dateList[!is.na(dateList)]  #strip NAs

startDate      <- dateList[(1+lag)]



# Get returns data
dataObj     <- pdeItem(secList, as.integer(format(startDate, format="%Y%m%d")), "trt1m", lookBack=nMonths)
retTS       <- dataObj$rawValuesMat # NO adjustment to returns!

cumRetMat   <- t(apply((1+ (retTS/100)), 1, cumprod) - 1)
m           <- cumRetMat[,dim(retTS)[2],drop=F]  * 100

# Put in data object
momObj   <- NULL

momObj$values <- m
momObj$retMat <- retTS

momObj
}
