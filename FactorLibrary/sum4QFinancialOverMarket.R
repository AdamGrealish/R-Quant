#  source("C:\\R Toolbox\\FactorLibrary\\sum4QFinancialOverMarket.R")

sum4QFinancialOverMarket <- function(secList, dt, financialData, marketData, availMatrix){

# Calculates the ratio of the sum of last 4 quaters of matrix of finacial data over a  matrix of market data
# availMatrix tells us when finacial statements were available
# Example:
#
# secList   <- c("AA", "AAI")
# dt <-  20080101
# netIncomeToMV   <- sum4QFinancialOverMarket(secList, dt, netIncomeMatrix, capMatrix, availMatrix)
#

nStocks <- dim(financialData)[2]

# Get report dates   
fsDates <- findMostRecentFSDate(dt, availMatrix, 4)

# Calc 4 quarter sum for financials
sum4QMat <- matrix(NA,1, nStocks)
dimnames(sum4QMat)[[2]] <- dimnames(financialData)[[2]]

for (c in 1:nStocks){
   dateIdx        <-  match(fsDates[,c], as.integer(dimnames(financialData)[[1]]))
   sum4QMat[1,c]  <- sum(financialData[dateIdx,c])
}
numerator   <- sum4QMat

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