#  source("C:\\R Toolbox\\hitRate.R")


hitRate <- function(score, ret){
# Calculates hit rate.

x <- score * ret

sum(x>0) / length(x)

}