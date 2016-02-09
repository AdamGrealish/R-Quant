#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\writeFactorScript.R")


# Write factor data to csv script

#fidList         <- c(1,2,3,4,5,6,7,8,10)
#funcNameList    <- c("bookToMV", "roa", "margin", "turn", "lever", "liquid", "cfo", "accruals", "piotroski")
#descrList       <- c(
#                     "BM: Book Value / Market Value",
#                     "ROA: NI / Total Assets",
#                     "Margin: Gross Margin / Net Sales",
#                     "Turn: Net Sales / Avg Total Assets", 
#                     "Lever: Total Long-term Debt / Avg Total Assets", 
#                     "Liquid: Total Current Assets / Total Current Liabilities",
#                     "CFO: CFO / Total Assets",
#                     "ACCRUALS: (NI-CFO) / Total Assets",
#                     "Piotroski Score"
#                     )

#fidList        <- 10
#funcNameList   <- "piotroski"
#descrList      <- "Piotroski Score"

#fidList         <- c(300)
#funcNameList    <- c("mom")
#descrList       <- c(
#                     "12 Month Momentum excluding most recent month"
#                     )

fidList         <- c(500)
funcNameList    <- c("simpleModel")
descrList       <- c(
                     "Simple Model: .5 BP + .5 Mom"
                     )

#fidList         <- c(200, 300, 10)
#funcNameList    <- c("earningsBeforeExtraordinaryItemsToMV", "mom", "piotroski")
#descrList       <- c(
#                     "Earnings Before Extraordinary Items To MV",
#                     "11m Momentum lagged 1m",
#                     "Piotroski Score"
#                     )

# dateList
queryStr       <- "select distinct datadate from compustat_qtrly_fundamentals"
fullDateList   <- sqlQuery(csDB, queryStr)[,"datadate"]
dateList       <- sort(fullDateList, decreasing=T)
dateList       <- dateList[1:(length(dateList)-12)]

#idxTicker       <- "I0003" #SP500
idxTicker       <- "I0020" #SP1500
filePath        <- "F:\\Data\\Factors\\SP1500\\"

# write
writeFactorToCSV(fidList, funcNameList, descrList, dateList, idxTicker, filePath)
