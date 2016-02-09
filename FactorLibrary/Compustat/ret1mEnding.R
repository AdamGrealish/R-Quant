#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\ret1mEnding.R")

ret1mEnding <- function(secList, dt){
# 1month total return ending on dt. 
# Return is in percent format
#
# Example:
# 
# dt          <- 19930101
# secList     <- indexMembers("I0003", dt)
# ret         <- ret1mEnding(secList, dt)
#

# Look for connection
makeCompustatConnection()

dataItemStr <- "trt1m"

# Create list of monthly dates from PDE database
queryStr       <- "select distinct datadate from compustat_monthly_pricing"
fullDateList   <- sqlQuery(csDB, queryStr)[,"datadate"]
dateList       <- sort(fullDateList[fullDateList <= dt ], decreasing=T)

lookupDate     <- dateList[1]

# Get data
queryStr <- paste("SELECT gvkey, tic, datadate, ajexm,", dataItemStr, 
            "FROM compustat_monthly_pricing where gvkey in (", paste(secList,"",sep="", collapse=","), 
            ") and datadate =", lookupDate)
pData   <- sqlQuery(csDB, queryStr)

# Define data vars
periodId       <- pData[, "gvkey"]
periodData     <- pData[, dataItemStr]

# intersect with secList
matchIdx          <- match(secList, periodId)
matchedPeriodData <- periodData[matchIdx]

# put in matrix
ret1m             <- as.matrix(matchedPeriodData)
dimnames(ret1m)   <- list(secList, lookupDate)

ret1m
}