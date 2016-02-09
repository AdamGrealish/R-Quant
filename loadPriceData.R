#	 source("C:\\R Toolbox\\loadPriceData.R")

############################################
# Example:
# priceData <- loadPriceData("F:\\Data\\InterDayData\\Last Price Split Adj.csv")
############################################

loadPriceData <- function(fullPath, nRows = 1493){
# load a rectangular matrix with data down the rows and tickers across the columns

# read in entire file as char vector
infoVector  <- scan(fullPath, what=character(), skip=0, sep=",")     

nCols       <- length(infoVector) / nRows   # determine number of columns
infoData    <- matrix(infoVector, nrow=nRows, ncol=nCols, byrow=T)      # make a char matrix 

#label matrix
dimnames(infoData) <- c(list(infoData[,1]), list(infoData[1,]))  

# remove first row, first column
infoData <- infoData[-1,-1]   

# convert NAs
infoData[infoData=="#N/A N/A"] <- NA

#save dimnames
dnames <- dimnames(infoData)
dnames[[1]] <-  format(as.Date(dnames[[1]], format="%m/%d/%Y"), format="%Y%m%d")



# make doubles
infoData <- apply(infoData, 2, as.numeric)

#re-apply dimnames   
dimnames(infoData) <- dnames

infoData
}