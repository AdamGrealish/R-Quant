#  source("C:\\R Toolbox\\FactorLibrary\\lastFinancialOverMarket.R")

lastFinancialOverMarket <- function(secList, dt, financialData, marketData, availMatrix){

# Calculates the ratio of the last quater of matrix of finacial data over a  matrix of market data
# availMatrix tells us when finacial statements were available
# Example:
#
# secList   <- c("AA", "AAI")
# dt <-  20080101
# bookToMV   <- lastFinancialOverMarket(secList, dt, bookMatrix, capMatrix, availMatrix)
#

nStocks <- dim(financialData)[2]

# Get report dates   
fsDates <- findMostRecentFSDate(dt, availMatrix, 1)

# Calc 4 quarter sum for financials
lastMat <- matrix(NA,1, nStocks)
dimnames(lastMat)[[2]] <- dimnames(financialData)[[2]]

for (c in 1:nStocks){
   dateIdx       <-  match(fsDates[,c], as.integer(dimnames(financialData)[[1]]))
   lastMat[1,c]  <- financialData[dateIdx,c]
}
numerator   <- lastMat

# Calc current market data 
denominator <- marketData[match(dt, as.integer(dimnames(marketData)[[1]])),]


factor <- numerator / denominator

# Only return results for stocks in secList
tickers <- gsub(" ", "", dimnames(financialData)[[2]])
secIdx 	<- match(secList, tickers)

factor[,secIdx]
#factor
}

### need sum4Q, last, average4Q