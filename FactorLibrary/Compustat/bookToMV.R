#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\bookToMV.R")


bookToMV <- function(secList, dt){

# returns an object with $value as the B/P value for the stocks in secList at time dt
#
# Example:
# 
# dt          <- 19930101
# secList     <- indexMembers("I0003", dt)
# bpObj       <- bookToMV(secList, dt)
# bp          <- bpObj$values 
#

############################
# FINANCIAL STATEMENT DATA
############################
# Get data
bookObj  <- csqItem(secList, dt, "ceqq")
# Move non-NAs to the left
bookMat  <- moveToLeft(bookObj$valuesMat)
   
# CALC from quarterly FS items
bookValue      <- bookMat[,1, drop=F]  # most recent quarterly book value


############################
# MARKET DATA
############################
mv    <- csMktCap(secList, dt)


############################
# FACTOR
############################
factor <- bookValue / mv

dimnames(factor)[2] <- dimnames(mv)[2]


dataObj   <- NULL
dataObj$values <- factor
dataObj$book   <- bookValue
dataObj$mv     <- mv

dataObj
}