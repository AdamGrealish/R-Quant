#  source("C:\\R Toolbox\\quantileSpread.R")

quantileSpread <- function(score, ret, tile) {
# quantileSpread(score, ret, tile) calculates the quantile return spread 
# for an equal-weighted portfolio sorted on 'score' 
#
# score: vector of scores on which to sort stocks.
# ret:   vector of returns corresponding to the elements in 'score'. The 
#        first element of the score vector and the ret vector should 
#        correspond to the same stock.
# tile:  the number of tiles used for spread calculation, ie: 10 is decile spreads.

# Create index to order from highest to lowest score
idx         <- rev(order(score))
orderedRet  <- ret[idx]

n     <- length(score)
ntile <- floor(n / tile)

top   <- mean(orderedRet[1:ntile])
btm   <- mean(orderedRet[(n-ntile+1):n])

sprd  <- top - btm

sprd

}