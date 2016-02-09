#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\pdeItem.R")


pdeItem <- function(secList, dt, dataItemStr, lookBack=1){

# returns object with data item from Compustat Pricing, Dividends, and Earnings (PDE) database
#
# secList:     list of unique security IDs (gvkey)
# dt:          date as an integer
# dataItemStr: Compustat mnuemonic as string
# csDB:        ODBC connection to Compustat database
# lookBack:    number of months to look back for values in the data, will replace NA values with older non-NA values.
#
# Example:
# 
# dt          <- 19930101
# secList     <- indexMembers("I0003", dt)
# dataItemStr <- "prccm"
# dataObj     <- pdeItem(secList, dt, dataItemStr)
# price       <- dataObj$values
#

# Look for connection
makeCompustatConnection()

# Create list of monthly dates from PDE database
queryStr       <- "select distinct datadate from compustat_monthly_pricing"
fullDateList   <- sqlQuery(csDB, queryStr)[,"datadate"]
dateList       <- sort(fullDateList[fullDateList <= dt ], decreasing=T) # go all the way back from dt, most recent first
dateList       <- dateList[1:lookBack]        #go back 'lookBack' months
dateList       <- dateList[!is.na(dateList)]  #strip NAs
 
startDate      <- dateList[length(dateList)]
endDate        <- dateList[1]

# Get data
queryStr <- paste("SELECT gvkey, tic, iid, datadate, ajexm,", dataItemStr, 
            "FROM compustat_monthly_pricing where gvkey in (", paste(secList,"",sep="", collapse=","), 
            ") and datadate <= ", endDate)
pData   <- sqlQuery(csDB, queryStr)


# Make datadate column a variable
dataDate       <- pData[,"datadate"]

# Make matrix of dates used by stock
dateMat  <- matrix(1,length(secList), 1) %*% dateList
dimnames(dateMat)       <- list(secList, dateList)

# Instantiate data matrices
adjustedDataMat            <- matrix(NA, length(secList), length(dateList))
adjustmentMat              <- matrix(NA, length(secList), length(dateList))
rawDataMat                 <- matrix(NA, length(secList), length(dateList))
dimnames(adjustedDataMat)  <- list(secList, dateList)
dimnames(adjustmentMat)    <- list(secList, dateList)
dimnames(rawDataMat)       <- list(secList, dateList)

for (i in 1:length(dateList)){
   # Define data vars
   periodId       <- pData[dataDate==dateList[i], "gvkey"]
   periodIssue    <- pData[dataDate==dateList[i], "iid"]
   periodAdj      <- pData[dataDate==dateList[i], "ajexm"]
   periodData     <- pData[dataDate==dateList[i], dataItemStr]
   
   ### Rolling up multiple issues ###
   # identify gvkeys with multiple issues
   multIssueIdx      <- as.numeric(periodIssue) >= 2
   multIssueStocks   <- periodId[multIssueIdx]
   # roll up
   # add mult issue data to primary issue data
   # drop multiple issues from data and adj
   ###
   
   # intersect with secList
   matchIdx             <- match(secList, periodId)
   matchedPeriodData    <- periodData[matchIdx]
   matchedPeriodAdj     <- periodAdj[matchIdx]
   
   # adjust item
   if (is.element(dataItemStr, c("cshoq","cshtrm"))){   # invert certain adjustments
      matchedPeriodAdj <- 1 / matchedPeriodAdj
   }
   
   # place in data matrices
   adjustedDataMat[, i] <- (matchedPeriodData / matchedPeriodAdj)
   adjustmentMat[, i]   <- matchedPeriodAdj
   rawDataMat[, i]      <- matchedPeriodData
}

# Get most recent data
if (lookBack > 1){
   adjustedData   <- moveToLeft(adjustedDataMat)[,1, drop=F]
   adjustment     <- moveToLeft(adjustmentMat)[,1, drop=F]
   rawData        <- moveToLeft(rawDataMat)[,1, drop=F]
} else{
   adjustedData   <- adjustedDataMat
   adjustment     <- adjustmentMat  
   rawData        <- rawDataMat     
}

# Place in data object
dataObj <- NULL

dataObj$values       <- adjustedData
dataObj$adjustment   <- adjustment
dataObj$rawValues    <- rawData

dataObj$valuesMat    <- adjustedDataMat
dataObj$adjustmentMat<- adjustmentMat
dataObj$rawValuesMat <- rawDataMat
dataObj$dateMat      <- dateMat

dataObj
}