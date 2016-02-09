#  source("C:\\R Toolbox\\zscore.R")

zscore <- function(x, na.rm=FALSE){
# Z-score a vector

x <- as.vector(x)

(x - mean(x, na.rm=na.rm)) / sd(x, na.rm=na.rm)

}