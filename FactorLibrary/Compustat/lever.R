#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\lever.R")


lever <- function(secList, dt){

# returns an object with $value as the (Total Long-term Debt / Avg Total Assets) value for the stocks in secList at time dt
#
# Example:
# 
# dt          <- 19930101
# secList     <- indexMembers("I0003", dt)
# dataObj     <- lever(secList, dt)
# lever       <- dataObj$values 
#

############################
# FINANCIAL STATEMENT DATA 1
############################
# Get data
ltdObj  <- csqItem(secList, dt, "dlttq")
# Move non-NAs to the left
ltdMat  <- moveToLeft(ltdObj$valuesMat)

# CALC from quarterly FS items
ltd     <- ltdMat[,1, drop=F] # most recent quarter of Long-term debt


############################
# FINANCIAL STATEMENT DATA 2
############################
# Get data
taObj  <- csqItem(secList, dt, "atq")
# Move non-NAs to the left
taMat  <- moveToLeft(taObj$valuesMat)

# CALC from quarterly FS items
ta     <- as.matrix(apply(taMat[,1:4], 1, mean))  # 4 quarter average of total assets


############################
# FACTOR
############################
factor <- ltd / ta

dimnames(factor)[2] <- max(ltdObj$fDateMat, na.rm=T)


dataObj   <- NULL
dataObj$values <- factor
dataObj$ltd    <- ltd
dataObj$ta     <- ta

dataObj
}