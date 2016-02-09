#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\csqItem.R")

csqItem <- function(secList, dt, dataItemStr, nQtrs=6, cf=FALSE, reportLag=TRUE){

# returns an object with $values matrix of dataItem values 
#
# secList:     list of unique security IDs (gvkey)
# dt:          date as an integer
# dataItemStr: Compustat mnuemonic as string
# csDB:        ODBC connection to Compustat quarterly database
# nQtrs:       Number of quarters of data to retrieve
# cf:          flag for cash flow item. Must be handled differently bc of cumulative reporting
# reportLag:   only return values after reportDate (dt >= reportDate), or lag my 1 quarter if reportDate is NA
#
# Example:
#
# dt          <- 19930101
# secList     <- indexMembers("I0003", dt)
# dataItemStr <- "ceqq"
# dataItemStr <- "oancfy"
# nQtrs       <- 6
# bookObj     <- csqItem(secList, dt, dataItemStr)
# bookValues  <- bookObj$valuesMat
#

# Look for connection
makeCompustatConnection()

# Catch some cashflow items, set cf flag to TRUE
cfDataItemList <- c("IBCY", "DPCY", "XIDOCY", "TXDCY", "ESUBCY", "SPPIVY", "FOPOY", "TXBCOY", "FOPOXY", "RECCHY", 
                               "INVCHY", "APALCHY", "TXACHY", "AOLOCHY", "OANCFY", "IVCHY", "SIVY", "IVSTCHY", "CAPXY", 
                               "SPPEY", "AQCY", "IVACOY", "IVNCFY", "SSTKY", "TXBCOFY", "PRSTKCY", "DVY", "DLTISY", 
                               "DLTRY", "DLCCHY", "FIAOY", "FINCFY", "EXREY", "CHECHY", "INTPNY", "TXPDY")
                               
if (is.element(tolower(dataItemStr), tolower(cfDataItemList))){
   cf <- TRUE
}

# if CF, give yourself an extra quarter to look back, since we will be differencing year-to-date data
if (cf){
   nQtrs <- nQtrs + 1
}

nMonths     <- nQtrs*3

# Create list of monthly financial statement dates
queryStr       <- "select distinct datadate from compustat_qtrly_fundamentals"
fullDateList   <- sqlQuery(csDB, queryStr)[,"datadate"]
dateList       <- sort(fullDateList[fullDateList <= dt ], decreasing=T)
dateList       <- dateList[1:nMonths]  #most recent first
dateList       <- dateList[!is.na(dateList)]   #strip NAs

startDate      <- dateList[length(dateList)]
endDate        <- dateList[1]

# Get data from db
dataTable <- "compustat_qtrly_fundamentals as a"
if (cf){
   dataTable <- "compustat_qtrly_fundamentals as a inner join compustat_qtrly_yeartodate as b on a.gvkey=b.gvkey and a.datadate=b.datadate"
}
queryStr <- paste("SELECT a.gvkey, a.tic, a.datadate, a.rdq, a.fyearq, a.fqtr,", dataItemStr, 
            "FROM ", dataTable, " where a.gvkey in (", paste(secList,"",sep="", collapse=","), 
            ") and a.datadate >=", startDate, "and a.datadate <=", endDate )
fsData   <- sqlQuery(csDB, queryStr)

# Remove non unique gvkey-date stocks
dupIdx   <- (!duplicated(paste(fsData[,"gvkey"], fsData[,"datadate"]   )))
fsData       <- fsData[dupIdx,]  

# make datadate column a variable
dataDate       <- fsData[,"datadate"]

# Instantiate data matrices
dateMat              <- matrix(NA, length(secList), nQtrs)
dimnames(dateMat)    <- list(secList, c("Recent", 2:(nQtrs-1), "Oldest"))
fYearMat             <- matrix(NA, length(secList), nQtrs)
dimnames(fYearMat)   <- list(secList, c("Recent", 2:(nQtrs-1), "Oldest"))
fQtrMat              <- matrix(NA, length(secList), nQtrs)
dimnames(fQtrMat)    <- list(secList, c("Recent", 2:(nQtrs-1), "Oldest"))
reportMat            <- matrix(NA, length(secList), nQtrs)
dimnames(reportMat)  <- list(secList, c("Recent", 2:(nQtrs-1), "Oldest"))
dataMat              <- matrix(NA, length(secList), nQtrs)
dimnames(dataMat)    <- list(secList, c("Recent", 2:(nQtrs-1), "Oldest"))

# Iterate thru each month, aggregate every 3 months
for (i in 1:nMonths){
   # for each period pull out data
   # dataItem for period i for all stocks
   periodDate     <- fsData[dataDate==dateList[i],"datadate"]
   periodId       <- fsData[dataDate==dateList[i],"gvkey"]
   periodReport   <- fsData[dataDate==dateList[i],"rdq"]
   periodFYear    <- fsData[dataDate==dateList[i],"fyearq"]
   periodFQtr     <- fsData[dataDate==dateList[i],"fqtr"]
   periodData     <- fsData[dataDate==dateList[i],dataItemStr]
   
   # intersect with secList
   matchIdx             <- match(secList, periodId)
   matchedPeriodDate    <- periodDate[matchIdx]
   matchedPeriodFQtr    <- periodFQtr[matchIdx]
   matchedPeriodFYear   <- periodFYear[matchIdx]
   matchedPeriodReport  <- periodReport[matchIdx]
   matchedPeriodData    <- periodData[matchIdx]
   
   # identify missing data 
   naIdx             <- (!is.na(matchedPeriodData))
   
   if (reportLag){
      # NA out financials that were not reported yet. periodReport == NA pass through 
      matchedPeriodData[matchedPeriodReport >= dt]       <- NA    
      # If report date not available in first quarter, lag 3 months
      if (i <= 3){
         matchedPeriodData[is.na(matchedPeriodReport)]  <- NA
      }
   }
   
   # place item in appropriate spot
   dateMat[naIdx, ceiling(i/3)]     <- matchedPeriodDate[naIdx]
   fQtrMat[naIdx, ceiling(i/3)]     <- matchedPeriodFQtr[naIdx]
   fYearMat[naIdx, ceiling(i/3)]    <- matchedPeriodFYear[naIdx]
   reportMat[naIdx, ceiling(i/3)]   <- matchedPeriodReport[naIdx]
   dataMat[naIdx, ceiling(i/3)]     <- matchedPeriodData[naIdx]
}

# Cash flow handling
if (cf ==T){
   # Turn year-to-date data into quaterly data
   # difference quarters. Qtr(t) - Qtr(t-1)
   dataDeltaMat <- dataMat[,1:(nQtrs-1)] - dataMat[,2:nQtrs]
   fQtrDeltaMat <- fQtrMat[,1:(nQtrs-1)] - fQtrMat[,2:nQtrs]
   # add back Q1s
   q1Idx                <- fQtrMat[,1:(nQtrs-1)]==1
   q1Idx[is.na(q1Idx)]  <- F
   dataMat5             <- dataMat[,1:(nQtrs-1)]
   dataDeltaMat[q1Idx]  <- dataMat5[q1Idx]
   
   # assign and trim
   dataMat     <- dataDeltaMat
   reportMat   <- reportMat[,1:(nQtrs-1)]
   fQtrMat     <- fQtrMat[,1:(nQtrs-1)]
   fYearMat    <- fYearMat[,1:(nQtrs-1)]
   dateMat     <- dateMat[,1:(nQtrs-1)]  
}

dataObj              <- NULL
dataObj$valuesMat    <- dataMat
dataObj$reportMat    <- reportMat
dataObj$fQtrMat      <- fQtrMat
dataObj$fYearMat     <- fYearMat
dataObj$fDateMat     <- dateMat

dataObj$item         <- dataItemStr
dataObj$id           <- secList

dataObj
}