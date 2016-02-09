#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\indexMembers.R")

indexMembers <- function(idxTicker="I0003", dt){

# Returns list of gvkeys for members of index idxTicker. Defaults to SP500.
#
# idxTicker:   ticker for index as string. use tic field. see F:\Data\ContextPaper\indexKey.csv
# dt:          date as integer
#
# Example:
# idxTicker    <- "I0003" #SP500
# idxTicker    <- "I0020" #SP1500
# dt           <- 19930101
# secList      <- indexMembers(idxTicker, dt)
#

# Look for connection
makeCompustatConnection()

# Query index constituents database
queryStr <- paste(   "select gvkey from index_constituents where tic='", idxTicker, "' and from_ <= ",
                     dt, " and (thru = 0 or thru >= ", dt, ")", sep="")
data     <- sqlQuery(csDB, queryStr)

data[,"gvkey"]
}

