#  source("C:\\R Toolbox\\gvkey2ticker.R")



gvkey2ticker   <- function(secList){

# Get data
queryStr <- paste("SELECT distinct gvkey, tic FROM compustat_qtrly_fundamentals where gvkey in (", paste(secList,"",sep="", collapse=","), 
            ")" )
data   <- sqlQuery(csDB, queryStr)

as.character(data[,"tic"])

}