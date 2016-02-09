#  source("C:\\R Toolbox\\contextPaper.R")

# Include files
source("C:\\R Toolbox\\include.R")

# Load Compustat data
#csQtrFile   <- "F:\\Data\\ContextPaper\\compustat_qtrly_fundamentals.csv"
#csPriceFile <- "F:\\Data\\ContextPaper\\compustat_monthly_pricing.csv"
#
#fs          <- read.table(csQtrFile, header=T, sep=",", quote="\"")
#p           <- read.table(csPriceFile, header=T, sep=",", quote="\"")

# Create ODBC connections
library(RODBC)
pricingDB   <- odbcConnect("csPricing")
quarterlyDB <- odbcConnect("csQuarterly")

# Queriy data
queryStr <- "SELECT * FROM Compustat_qtrly_fundamentals where tic = 'GS'" 
data     <- sqlQuery(quarterlyDB, queryStr)

# remove non unique gvkey-date stocks
dupIdx   <- (!duplicated(paste(fs[,"gvkey"], fs[,"datadate"]   )))
fs       <- fs[dupIdx,]  