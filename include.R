#        source("C:\\R Toolbox\\include.R")

# Add Libraries
library(RODBC)

# Make compustat connection
csDB        <- odbcConnect("compustat")
factorDB    <- odbcConnect("factors")

# Data Loaders
source("C:\\R Toolbox\\loadPriceData.R")
source("C:\\R Toolbox\\loadFSData.R")
source("C:\\R Toolbox\\loadFSAvailabilityData.R")

# Uts
source("C:\\R Toolbox\\mostRecentDate.R")
source("C:\\R Toolbox\\quantileSpread.R")
source("C:\\R Toolbox\\ret.R")
source("C:\\R Toolbox\\findMostRecentFSDate.R")
source("C:\\R Toolbox\\zscore.R")
source("C:\\R Toolbox\\hitRate.R")
source("C:\\R Toolbox\\moveToLeft.R")
source("C:\\R Toolbox\\writeFactorToCSV.R")
source("C:\\R Toolbox\\makeFactorsConnection.R")
source("C:\\R Toolbox\\FactorLibrary\\getFactorScores.R")
source("C:\\R Toolbox\\gvkey2ticker.R")
#source("C:\\R Toolbox\\backtestKernel.R")
source("C:\\R Toolbox\\backtestKernel_beta.R")


# Momentum
source("C:\\R Toolbox\\FactorLibrary\\mom12.R")
source("C:\\R Toolbox\\FactorLibrary\\mom12lag1.R")
source("C:\\R Toolbox\\FactorLibrary\\momTrend.R")
source("C:\\R Toolbox\\FactorLibrary\\momTrendTstat.R")
source("C:\\R Toolbox\\FactorLibrary\\momTrendWeekly.R")
source("C:\\R Toolbox\\FactorLibrary\\momTrendWeeklyTstat.R")
 
 
# Financial Ratios
source("C:\\R Toolbox\\FactorLibrary\\sum4QFinancialOverMarket.R")
source("C:\\R Toolbox\\FactorLibrary\\lastFinancialOverMarket.R")

# Compustat
source("C:\\R Toolbox\\FactorLibrary\\Compustat\\makeCompustatConnection.R")

source("C:\\R Toolbox\\FactorLibrary\\Compustat\\csqItem.R")
source("C:\\R Toolbox\\FactorLibrary\\Compustat\\pdeItem.R")
source("C:\\R Toolbox\\FactorLibrary\\Compustat\\csShares.R")

source("C:\\R Toolbox\\FactorLibrary\\Compustat\\ret1mEnding.R")
source("C:\\R Toolbox\\FactorLibrary\\Compustat\\indexMembers.R")

source("C:\\R Toolbox\\FactorLibrary\\Compustat\\csMktCap.R")
source("C:\\R Toolbox\\FactorLibrary\\Compustat\\bookToMV.R")
source("C:\\R Toolbox\\FactorLibrary\\Compustat\\earningsBeforeExtraordinaryItemsToMV.R")
source("C:\\R Toolbox\\FactorLibrary\\Compustat\\roa.R")
source("C:\\R Toolbox\\FactorLibrary\\Compustat\\margin.R")
source("C:\\R Toolbox\\FactorLibrary\\Compustat\\turn.R")
source("C:\\R Toolbox\\FactorLibrary\\Compustat\\lever.R")
source("C:\\R Toolbox\\FactorLibrary\\Compustat\\liquid.R") 
source("C:\\R Toolbox\\FactorLibrary\\Compustat\\cfo.R")
source("C:\\R Toolbox\\FactorLibrary\\Compustat\\accruals.R")

source("C:\\R Toolbox\\FactorLibrary\\Compustat\\mom.R")

source("C:\\R Toolbox\\FactorLibrary\\Compustat\\simpleModel.R")

# Factors
source("C:\\R Toolbox\\FactorLibrary\\piotroski.R")