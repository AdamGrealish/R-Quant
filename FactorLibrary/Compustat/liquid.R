#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\liquid.R")


liquid <- function(secList, dt){

# returns an object with $value as the (Total Current Assets / Total Current Liabilities) value for the stocks in secList at time dt
#
# Example:
# 
# dt          <- 19930101
# secList     <- indexMembers("I0003", dt)
# dataObj     <- liquid(secList, dt)
# liquid      <- dataObj$values 
#

############################
# FINANCIAL STATEMENT DATA 1
############################
# Get data
tcaObj  <- csqItem(secList, dt, "actq")
# Move non-NAs to the left
tcaMat  <- moveToLeft(tcaObj$valuesMat)

# CALC from quarterly FS items
tca     <- tcaMat[,1, drop=F] # most recent quarter of Total Current Assets


############################
# FINANCIAL STATEMENT DATA 2
############################
# Get data
tclObj  <- csqItem(secList, dt, "lctq")
# Move non-NAs to the left
tclMat  <- moveToLeft(tclObj$valuesMat)

# CALC from quarterly FS items
tcl     <- tclMat[,1, drop=F] # most recent quarter of Total Current Liabilities


############################
# FACTOR
############################
factor <- tca / tcl

dimnames(factor)[2] <- max(tcaObj$fDateMat, na.rm=T)


dataObj   <- NULL
dataObj$values <- factor
dataObj$tca    <- tca
dataObj$tcl    <- tcl

dataObj
}