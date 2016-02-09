#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\csMktCap.R")

csMktCap <- function(secList, dt){

# returns market cap at date dt for firms in secList 
#
# Example:
# 
# dt          <- 19930101
# secList     <- indexMembers("I0003", dt)
# mv          <- csMktCap(secList, dt)
#


p        <- pdeItem(secList, dt, "prccm")
shares   <- csShares(secList, dt)$unadjustedShares  #unadjusted shares

#mv       <- p$values * shares
mv       <- p$rawValues * shares   # unadjusted price * unadjusted shares

mv
}