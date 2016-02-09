#  source("C:\\R Toolbox\\findMostRecentFSDate.R")

findMostRecentFSDate <- function(dt, dateMatrix, numQuarters=1, direction="backward"){
# looks back (default) from time dt to find closest numQuarters dates (default=1) in dateMatrix for each column of dateMatrix
#
# if dt is in dateList, function returns dt
# look forward with direction="forward"
#
# Example:
# dt           <- 20061231
# fsDatesAtDt  <- findMostRecentFSDate(dt, availMatrix, 4)
#

bound <- match(dt, dimnames(dateMatrix)[[1]])

potentialDates <- dateMatrix[1:bound,]
#fsDatesAtDt    <- apply(potentialDates, 2, max, na.rm =T)

a <- apply(potentialDates, 2, unique)

fsDatesAtDt <- matrix(NA, numQuarters, dim(dateMatrix)[[2]])
dimnames(fsDatesAtDt)[2] <- dimnames(dateMatrix)[2]

for (c in 1:dim(dateMatrix)[2]){
   len <-   length(a[[c]])
   if (numQuarters > len){
      fsDatesAtDt[,c] <- as.vector(matrix(NA, numQuarters))
   } else{
      fsDatesAtDt[,c] <- a[[c]][(len-numQuarters+1):len]
   }
}

fsDatesAtDt
}