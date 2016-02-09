#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\accruals.R")


accruals <- function(secList, dt){

# returns an object with $value as the Accruals (NI - CFO / Total Assets) value for the stocks in secList at time dt
#
# Example:
# 
# dt          <- 19930101
# secList     <- indexMembers("I0003", dt)
# dataObj     <- accruals(secList, dt)
# accruals    <- dataObj$values 
#

############################
# FINANCIAL STATEMENT DATA 1
############################
# Get data
cfoObj  <- csqItem(secList, dt, "oancfy")
# Move non-NAs to the left
cfoMat  <- moveToLeft(cfoObj$valuesMat)

# CALC from quarterly FS items
cfo     <- as.matrix(apply(cfoMat[,1:4], 1, sum))  # sum most recent 4 quarters of CFO

############################
# FINANCIAL STATEMENT DATA 2
############################
# Get data
niObj  <- csqItem(secList, dt, "ibcomq")
# Move non-NAs to the left
niMat  <- moveToLeft(niObj$valuesMat)

# CALC from quarterly FS items
ni     <- as.matrix(apply(niMat[,1:4], 1, sum))  # sum most recent 4 quarters of net income


############################
# FINANCIAL STATEMENT DATA 3
############################
# Get data
taObj  <- csqItem(secList, dt, "atq")
# Move non-NAs to the left
taMat  <- moveToLeft(taObj$valuesMat)

# CALC from quarterly FS items
ta     <- taMat[,5, drop=F]  # total assets at beginning of year over which earnings are measured


############################
# FACTOR
############################
factor <- (ni-cfo) / ta

dimnames(factor)[2] <- max(cfoObj$fDateMat, na.rm=T)


dataObj   <- NULL
dataObj$values <- factor
dataObj$ni     <- ni
dataObj$cfo    <- cfo
dataObj$ta     <- ta

dataObj
}