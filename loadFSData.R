#     source("C:\\R Toolbox\\loadFSData.R")

loadFSData <- function(fullPath, nStocks = 2950){
# load a rectangular matrix with data down the rows and tickers across the columns
#
# Example:
# netIncomeMatrix <- loadFSData("F:\\Data\\InterDayData\\NetIncome.csv")
#

# read in entire file as char vector
infoVector  <- scan(fullPath, what=character(), skip=0, sep=",")     

nCols       <- nStocks*2+1
nRows       <- length(infoVector) / nCols  # determine number of rows
infoData    <- matrix(infoVector, nrow=nRows, ncol=nCols, byrow=T)      # make a char matrix 
infoData    <- infoData[,-1] # remove 1st column

#label matrix
a <- c("", infoData[1,1:(dim(infoData)[2]-1)])
b <- infoData[1,]
c <- paste(a,b)
dimnames(infoData)[2] <- list(c)  

# remove first row
infoData <- infoData[-1,]   

# convert NAs
infoData[infoData=="#N/A N/A"] <- NA


# gather all unique dates
dateIdx  <- (1:dim(infoData)[2] %% 2 ) ==1  #first column
valueIdx <- ((1:dim(infoData)[2]+1) %% 2 ) ==1 #second column
uniqueDates <- unique(as.vector(infoData[,dateIdx]))
uniqueDates <- uniqueDates[uniqueDates != ""]
uniqueDates <- uniqueDates[!is.na(uniqueDates)]
descendingDatesIdx <- order(as.Date(uniqueDates, format="%m/%d/%Y"))
uniqueDates <- uniqueDates[descendingDatesIdx]

# make matrix to hold all values at all dates
fsMatrix    <- matrix(NA, length(uniqueDates), sum(valueIdx))
dimnames(fsMatrix) <- list(format(as.Date(uniqueDates, format="%m/%d/%Y"), format="%Y%m%d"), c[valueIdx])

# populate matrix
for (c in 1:nStocks*2){
   for (r in 1:length(uniqueDates)){
      rowMatchIdx       <- match(uniqueDates[r], infoData[,(c-1)])
      fsMatrix[r,(c/2)] <- as.numeric(infoData[rowMatchIdx ,c])
   }
}

fsMatrix
}