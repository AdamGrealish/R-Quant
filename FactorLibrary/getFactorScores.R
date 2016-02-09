#  source("C:\\R Toolbox\\FactorLibrary\\getFactorScores.R")

getFactorScores <- function(secList, dt, fid, lookBack=1, table="factor_scores"){

# returns object with $value with factor score data from Factors database
#
# secList:     list of unique security IDs (gvkey)
# dt:          date as an integer
# fid:         unique factor id or funcName
#
# Example:
# 
# dt          <- 19930101
# secList     <- indexMembers("I0003", dt)
# fid         <- 1 #or "bookToMV"
# dataObj     <- getFactorScores(secList, dt, fid)
# bp          <- dataObj$values
#

# Look for connection
makeFactorsConnection()

# Create list of monthly dates from database
queryStr       <- paste("select distinct dt from ", table)
fullDateList   <- sqlQuery(factorDB, queryStr)[,"dt"]
dateList       <- sort(fullDateList[fullDateList <= dt ], decreasing=T) # go all the way back from dt, most recent first
dateList       <- dateList[1:lookBack]        #go back 'lookBack' months
dateList       <- dateList[!is.na(dateList)]  #strip NAs
 
startDate      <- dateList[length(dateList)]
endDate        <- dateList[1]

if (is.numeric(fid)){
   searchStr   <- paste("fid = ", fid, " ", sep="")
} else{
   searchStr   <- paste("funcName = '", fid, "' ", sep="")
}

# Get data
queryStr     <- paste("SELECT gvkey, dt, fid, funcName, descr, score FROM ", table, " where ", searchStr, 
               " and dt <= ", endDate,
               " and gvkey in (", paste(secList,"",sep="", collapse=","), ")", sep="")
factorData   <- sqlQuery(factorDB, queryStr)


# Make datadate column a variable
dataDate       <- factorData[,"dt"]

# Make matrix of dates used by stock
dateMat  <- matrix(1,length(secList), 1) %*% dateList
dimnames(dateMat)       <- list(secList, dateList)

# Instantiate data matrices
dataMat                 <- matrix(NA, length(secList), length(dateList))
dimnames(dataMat)       <- list(secList, dateList)

for (i in 1:length(dateList)){
   # Define data vars
   periodId       <- factorData[dataDate==dateList[i], "gvkey"]
   periodData     <- factorData[dataDate==dateList[i], "score"]
   
   # Match to secList
   matchIdx             <- match(secList, periodId)
   matchedPeriodData    <- periodData[matchIdx]
   
   # place in data matrix
   dataMat[, i]      <- matchedPeriodData
}

# Get most recent data
if (lookBack > 1){
   data        <- moveToLeft(dataMat)[,1, drop=F]
} else{
   data        <- dataMat     
}

# Place in data object
dataObj <- NULL

dataObj$values       <- data

dataObj$valuesMat    <- dataMat

dataObj$item         <- fid
dataObj$label        <- as.character(factorData[1, "funcName"])
dataObj$descr        <- as.character(factorData[1, "descr"])
dataObj$id           <- secList
dataObj$dateMat      <- dateMat

dataObj

}