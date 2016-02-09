#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\simpleModel.R")


simpleModel <- function(secList, dt, factorTable="factor_scores"){

val <- getFactorScores(secList, dt, "bookToMV", table=factorTable)$values
m   <- getFactorScores(secList, dt, "mom",      table=factorTable)$values

model <- .5*zscore(val, na.rm=T) + .5*zscore(m, na.rm=T)

dataObj <- NULL
dataObj$values <- model

dataObj$valPartZ   <- zscore(val, na.rm=T)
dataObj$momPartZ   <- zscore(m, na.rm=T)

dataObj$valPartRaw   <- val
dataObj$momPartRaw   <- m

dataObj
}