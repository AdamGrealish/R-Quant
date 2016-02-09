#  source("C:\\R Toolbox\\loadFSAvailabilityData.R")

loadFSAvailabiltyData <- function(fullPath, nStocks = 2950){
# for a given day what is the last available financial statement
# today -> most recent available financials
#
# Example:
#
# availMatrix <- loadFSAvailabiltyData("F:\\Data\\InterDayData\\AnncDates.csv")
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
dateIdx        <- ((1:dim(infoData)[2]+1) %% 2 ) ==1  #second column
statementIdx   <- (1:dim(infoData)[2] %% 2 ) ==1  #first column

uniqueDates <- sort(unique(as.vector(infoData[,dateIdx])))
uniqueDates <- uniqueDates[uniqueDates != ""]
uniqueDates <- uniqueDates[!is.na(uniqueDates)]

# make matrix to hold all values at all dates
availMatrix    <- matrix(NA, length(uniqueDates), sum(statementIdx))
dimnames(availMatrix) <- list(uniqueDates, c[statementIdx])

# populate matrix
for (c in 1:nStocks*2){
   for (r in 1:length(uniqueDates)){
      rowMatchIdx       <- match(uniqueDates[r], infoData[,(c)])
      availMatrix[r,(c/2)] <- as.integer(format(as.Date(infoData[rowMatchIdx ,(c-1)],format="%m/%d/%Y"),format="%Y%m%d"))
   }
}

# Add NAs for all other dates
fullDates <- format(seq(as.Date(uniqueDates[1], format="%Y%m%d"), len=difftime(Sys.Date(), as.Date(uniqueDates[1], format="%Y%m%d"), units="days" )[[1]], by="1 days"), format="%Y%m%d")
extraDays <- setdiff(fullDates, uniqueDates)
extraMat  <- matrix(NA, length(extraDays), nStocks)
dimnames(extraMat)[[1]]   <- extraDays

# Append and order
fullAvailMatrix      <- rbind(availMatrix, extraMat)
concatDates          <- c(uniqueDates, extraDays)
descendingDatesIdx   <- order(concatDates)
fullAvailMatrix      <- fullAvailMatrix[descendingDatesIdx,]


fullAvailMatrix
}