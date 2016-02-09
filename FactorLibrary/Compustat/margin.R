#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\margin.R")


margin <- function(secList, dt){

# returns an object with $value as the (Gross Margin / Net Sales) value for the stocks in secList at time dt
#
# Example:
# 
# dt          <- 19930101
# secList     <- indexMembers("I0003", dt)
# dataObj     <- margin(secList, dt)
# margin      <- dataObj$values 
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
cogsObj  <- csqItem(secList, dt, "cogsq")
# Move non-NAs to the left
cogsMat  <- moveToLeft(cogsObj$valuesMat)

# CALC from quarterly FS items
cogs     <- as.matrix(apply(cogsMat[,1:4], 1, sum))  # sum most recent 4 quarters of COGS


############################
# FACTOR
############################
grossProfit <- sales - cogs
factor      <- grossProfit / sales

dimnames(factor)[2] <- max(salesObj$fDateMat, na.rm=T)


dataObj   <- NULL
dataObj$values <- factor
dataObj$gp     <- grossProfit
dataObj$sales  <- sales
dataObj$cogs   <- cogs

dataObj
}