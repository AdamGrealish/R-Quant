#  source("C:\\R Toolbox\\FactorLibrary\\mom12lag1.R")

mom12lag1 <- function(secList, dt, priceData){

# Calculates 12m price momentum on date dt
# Example:
#
# secList   <- c("AA", "AAI")
# dt <-  20080104
# priceData <- loadPriceData("C:\\Data\\InterDayData\\Last Price Split Adj.csv")
# mom12lag1m   <- mom12lag1(secList, dt, priceData)
#


dt          <- as.Date(as.character(dt), "%Y%m%d")
endDate     <- seq(dt, len=2, by="-1 months")[2]
startDate   <- seq(dt, len=2, by="-12 months")[2]

endDate     <- as.integer(format(endDate, "%Y%m%d"))
startDate   <- as.integer(format(startDate, "%Y%m%d"))

mom12    <- ret(secList, startDate, endDate, priceData)

mom12
}