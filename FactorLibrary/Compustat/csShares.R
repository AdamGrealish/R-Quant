#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\csShares.R")


csShares <- function(secList, dt){

# returns object with number of shares outstanding -- adjusted and unadjusted
#
# secList:     list of unique security IDs (gvkey)
# dt:          date as an integer
# dataItemStr: Compustat mnuemonic as string
#
# Example:
# 
# dt          <- 20100101
# secList     <- indexMembers("I0003", dt)
# sharesOut   <- csShares(secList, dt)
#

# Get shares outstanding from Quarterly Fundamental database
qObj <- csqItem(secList, dt, "cshoq", reportLag=F)

# Get shares outstanding from Monthly Pricing database
mObj <- pdeItem(secList, dt, "cshoq", lookBack=4, rollUpMultipleIssues=T)

# Most recent dates available in Quarterly data
mostRecentQtrly                        <- qObj$fDateMat
mostRecentQtrly[is.na(qObj$valuesMat)] <- NA
mostRecentQtrly                        <- moveToLeft(mostRecentQtrly)[,1, drop=F]
mostRecentQtrly[is.na(mostRecentQtrly),] <- 0   #if no date is available, code to very low number (aka use Monthly data)

# Most recent dates available in Monthly data
mostRecentMonthly                         <- mObj$dateMat
mostRecentMonthly[is.na(mObj$valuesMat)]  <- NA
mostRecentMonthly                         <- moveToLeft(mostRecentMonthly)[,1, drop=F]
mostRecentMonthly[is.na(mostRecentMonthly),] <- 0  #if no date is available, code to very low number (aka use Quarterly data)

# Use newest values
# Unadjusted
unadjSharesOut                                        <- moveToLeft(qObj$valuesMat)[,1, drop=F]  # Quarterly shares are unadjusted,
unadjSharesOut[mostRecentMonthly > mostRecentQtrly]   <- moveToLeft(mObj$rawValuesMat)[mostRecentMonthly > mostRecentQtrly  ,1] # so use unadjusted monthly sharesOut
# adjusted
idx         <- match(mostRecentQtrly, dimnames(mObj$adjustmentMat)[[2]])
adjFactor   <- mObj$adjustmentMat[1,idx]
adjSharesOut                                        <- moveToLeft(qObj$valuesMat)[,1, drop=F]  / adjFactor
adjSharesOut[mostRecentMonthly > mostRecentQtrly]   <- moveToLeft(mObj$valuesMat)[mostRecentMonthly > mostRecentQtrly  ,1] 

dataObj <- NULL
dataObj$unadjustedShares <- unadjSharesOut
dataObj$adjustedShares   <- adjSharesOut

dataObj
}