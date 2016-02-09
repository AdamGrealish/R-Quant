#  source("C:\\R Toolbox\\FactorLibrary\\mom12.R")

mom12 <- function(secList, dt, priceData){

# Calculates 12m price momentum on date dt
# Example:
#
# secList   <- c("AA", "AAI")
# dt <-  20080101
# priceData <- loadPriceData("C:\\Data\\InterDayData\\Last Price Split Adj.csv")
# mom12m   <- mom12(secList, dt, priceData)
#


dt          <- as.Date(as.character(dt), "%Y%m%d")
endDate     <- seq(dt, len=2, by="0 months")[2]
startDate   <- seq(dt, len=2, by="-12 months")[2]

endDate     <- as.integer(format(endDate, "%Y%m%d"))
startDate   <- as.integer(format(startDate, "%Y%m%d"))

mom12    <- ret(secList, startDate, endDate, priceData)

mom12
}