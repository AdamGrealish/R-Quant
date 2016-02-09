#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\pdeItem.R")


pdeItem <- function(secList, dt, dataItemStr, lookBack=1, rollUpMultipleIssues=F, uniqueAtIssue=F){

# returns object with data item from Compustat Pricing, Dividends, and Earnings (PDE) database
#
# secList:     list of unique security IDs (gvkey)
# dt:          date as an integer
# dataItemStr: Compustat mnuemonic as string
# csDB:        ODBC connection to Compustat database
# lookBack:    number of months to look back for values in the data, will replace NA values with older non-NA values.
# rollUpMultipleIssues: Aggregate multiple security issues for the same firm. If set to False will use primary issue if possible
# uniqueAtIssue:  return information at issue level, as opposed to firm level. *** NOTE: items are no longer in secList order. ***
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


# Store original secList
secListOrig <- secList
gvkeyList   <- secList

# Make datadate column a variable
dataDate       <- pData[,"datadate"]

# make secList a cat of gvkey and issue
if (uniqueAtIssue){
   gvkeyList <- pData[dataDate==endDate, "gvkey"]
   issueList <- pData[dataDate==endDate, "iid"]
   secList   <- paste(gvkeyList, issueList, sep=".")
}


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
   
   if (uniqueAtIssue){
      periodId <- paste(periodId, periodIssue, sep=".")
   }
   
   # adjust item
   if (is.element(dataItemStr, c("cshoq","cshtrm"))){   # invert certain adjustments
      periodAdj <- 1 / periodAdj
   }
   periodAdjData <- periodData / periodAdj
   
   ### Rolling up multiple issues ###
   if (rollUpMultipleIssues){
      # Roll up multiple issues
      secList              <- secListOrig    #if we are rolling up, then issues no longer matter
      
      rolledPeriodData     <- tapply(periodData,    periodId, sum)
      rolledPeriodAdjData  <- tapply(periodAdjData, periodId, sum)
      rollId               <- as.integer(dimnames(rolledPeriodData)[[1]])
   
      # intersect with secList
      matchIdx             <- match(secList, rollId)
      matchedPeriodData    <- rolledPeriodData[matchIdx]
      matchedPeriodAdjData <- rolledPeriodAdjData[matchIdx]
      
   } else{
      # Don't roll, use primary line if possible  
      # Naive match
      matchIdx             <- match(secList, periodId)
      matchedPeriodData    <- periodData[matchIdx]
      matchedPeriodAdjData <- periodAdjData[matchIdx]
      # Primary issue
      primaryData          <- periodData[periodIssue == "01"]
      primaryAdjData       <- periodAdjData[periodIssue == "01"]
      primaryId            <- periodId[periodIssue == "01"]
      primaryIssue         <- periodIssue[periodIssue == "01"]
      if (uniqueAtIssue){
         primaryId <- paste(primaryId, primaryIssue, sep=".")
      }
      # order primary to secList
      pIdx                 <- match(secList, primaryId)
      primaryData          <- primaryData[pIdx]
      primaryAdjData       <- primaryAdjData[pIdx]
      # put overwrite naive with primary where exists
      matchedPeriodData[!is.na(primaryData)]       <- primaryData[!is.na(primaryData)]
      matchedPeriodAdjData[!is.na(primaryAdjData)] <- primaryAdjData[!is.na(primaryAdjData)]      
   }
   ###
   
   # place in data matrices
   adjustedDataMat[, i] <- matchedPeriodAdjData
   rawDataMat[, i]      <- matchedPeriodData
}

# Get most recent data
if (lookBack > 1){
   adjustedData   <- moveToLeft(adjustedDataMat)[,1, drop=F]
   rawData        <- moveToLeft(rawDataMat)[,1, drop=F]
} else{
   adjustedData   <- adjustedDataMat
   rawData        <- rawDataMat     
}


# Place in data object
dataObj <- NULL

dataObj$values       <- adjustedData
dataObj$rawValues    <- rawData
dataObj$adjustment   <- rawData / adjustedData


dataObj$valuesMat    <- adjustedDataMat
dataObj$rawValuesMat <- rawDataMat
dataObj$adjustmentMat<- rawDataMat / adjustedDataMat

dataObj$item         <- dataItemStr
dataObj$id           <- secList
dataObj$gvkey        <- gvkeyList
dataObj$dateMat      <- dateMat

dataObj
}