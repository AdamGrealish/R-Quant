#  source("C:\\R Toolbox\\moveToLeft.R")

moveToLeft <- function(x){

# Row by row, move non-NA values of matrix x to left, still NAs on right

nRows <- dim(x)[1]
nCols <- dim(x)[2]

missingIdx  <- t(apply(x, 1, is.na))
oIdx <- t(apply(missingIdx, 1, order))

for (i in 1:(nRows)){   
   x[i,] <- x[i,oIdx[i,]]
}

x
}

