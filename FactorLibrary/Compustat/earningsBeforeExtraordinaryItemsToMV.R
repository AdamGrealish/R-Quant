#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\earningsBeforeExtraordinaryItemsToMV.R")


earningsBeforeExtraordinaryItemsToMV <- function(secList, dt){

# returns an object with $value as the (Earnings Before Extraordinary Items / Market Value) value for the stocks in secList at time dt
#
# Example:
# 
# dt          <- 19930101
# secList     <- indexMembers("I0003", dt)
# earnObj     <- earningsBeforeExtraordinaryItemsToMV(secList, dt)
# ep          <- earnObj$values 
#

############################
# FINANCIAL STATEMENT DATA
############################
# Get data
niObj  <- csqItem(secList, dt, "ibcomq")
# Move non-NAs to the left
niMat  <- moveToLeft(niObj$valuesMat)

# CALC from quarterly FS items
ni     <- as.matrix(apply(niMat[,1:4], 1, sum))  # sum most recent 4 quarters of net income


############################
# MARKET DATA
############################
mv    <- csMktCap(secList, dt)


############################
# FACTOR
############################
factor <- ni / mv

dimnames(factor)[2] <- dimnames(mv)[2]


dataObj        <- NULL
dataObj$values <- factor
dataObj$ni     <- ni
dataObj$mv     <- mv

dataObj
}