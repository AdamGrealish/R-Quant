#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\turn.R")


turn <- function(secList, dt){

# returns an object with $value as the (Net Sales / Avg Total Assets) value for the stocks in secList at time dt
#
# Example:
# 
# dt          <- 19930101
# secList     <- indexMembers("I0003", dt)
# dataObj     <- turn(secList, dt)
# turn        <- dataObj$values 
#

############################
# FINANCIAL STATEMENT DATA 1
############################
# Get data
salesObj  <- csqItem(secList, dt, "saleq")
# Move non-NAs to the left
salesMat  <- moveToLeft(salesObj$valuesMat)

# CALC from quarterly FS items
sales     <- as.matrix(apply(salesMat[,1:4], 1, sum))  # sum most recent 4 quarters of sales


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
factor <- sales / ta

dimnames(factor)[2] <- max(salesObj$fDateMat, na.rm=T)


dataObj   <- NULL
dataObj$values <- factor
dataObj$sales  <- sales
dataObj$ta     <- ta

dataObj
}