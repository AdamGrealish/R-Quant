#		source("C:\\R Toolbox\\ret.R")



ret <- function(secList, startDate, endDate, priceData){

# take priceData matrix of adjusted prices, find securities in secList,
# calculate total return from startDate to endDate
#
# Example:
#
# secList   <- c("AA", "AAI")
# startDate <- 20060105
# endDate   <- 20060109
# priceData <- loadPriceData("C:\\Data\\InterDayData\\Last Price Split Adj.csv")
# r   <- ret(secList, startDate, endDate, priceData)
#


dates 	<- dimnames(priceData)[[1]]
tickers <- dimnames(priceData)[[2]]

secIdx 	<- match(secList, tickers)
startIdx <- match(startDate, dates)
endIdx 	<- match(endDate, dates)


startMat  <- priceData[startIdx, secIdx, drop=F]
endMat		<- priceData[endIdx, secIdx, drop=F]

retMat 			<- (endMat / startMat - 1)*100

dimnames(retMat)[[1]] <- startDate

retMat
}