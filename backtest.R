#  source("C:\\R Toolbox\\backtest.R")


###############################
# Set Paramaters
###############################
# Factor List
#factors <- c("piotroski")
#factors  <- c("bookToMV", "earningsBeforeExtraordinaryItemsToMV", "mom")
#factors <- c("bookToMV", "mom", "simpleModel")
factors <- c("piotroski", "bookToMV", "earningsBeforeExtraordinaryItemsToMV", "mom", "simpleModel")

# Create date sequence
startDate   <- 19920131
#startDate   <- 20061231
endDate     <- 20100331
queryStr       <- "select distinct datadate from compustat_monthly_pricing"
fullDateList   <- sqlQuery(csDB, queryStr)[,"datadate"]
dateList       <- sort(fullDateList[(fullDateList <= endDate) & (fullDateList >= startDate) ], decreasing=F) # go all the way back from dt, most recent last
dateList       <- dateList[!is.na(dateList)]  #strip NAs

# add an extra month to dateList
dateList2      <- c(dateList, format(seq.Date(as.Date(as.character(dateList[length(dateList)]), format="%Y%m%d"),len=2,by="1 months")[2], format="%Y%m%d"))

# Make univList
univId      <- "I0003" #SP500
#univId      <- "I0020" #SP1500
univList    <- NULL
for (i in 1:length(dateList)){
   secList     <- indexMembers(univId, dateList[i])
   #ep          <- getFactorScores(secList, dateList[i], "earningsBeforeExtraordinaryItemsToMV")$values
   #rdq         <- moveToLeft(csqItem(secList, dateList[i], "rdq")$reportMat)[,1]
   #secList     <- secList[!((rdq >= dateList[i]) & (rdq <= dateList2[i+1]))] # earnings Not reported this month
   #secList     <- secList[((rdq >= dateList[i]) & (rdq <= dateList2[i+1]))] # earnings report this month
   #secList     <- secList[bp<=quantile(bp, (1/3), na.rm=T)] # btm 1/3 of bp
   #secList     <- secList[ep<=0]
   secList     <- secList[!is.na(secList)]
   
   univList[[i]]  <- secList
}   

# Set backtest params
rebalFreq      <- "1 months"
returnFreq     <- 1   #number of months
implementLag   <- "1 days"
spreadBuckets  <- 5
###############################


# RUN BACKTEST
a <- backtestKernel(dateList, univList, factors, factorTable="factor_scores_sp500")

save(list=c(
"earningsReport.sp1500.all"
), file="F:\\Data\\ContextPaper\\Backtest\\resultObjs14")


# Visualize results
plot(cumsum(spreads[,1]), type="l", col="black", axes=FALSE, ann=FALSE)
incr  <- 12
axis(1, at=seq(1,dim(spreads)[1],by=incr), lab=dimnames(spreads)[[1]][((0:(dim(spreads)[1]-1)) %% incr)==0], las=2)
#axis(2, round(seq(min(cumsum(spreads[,3]), na.rm=T), max(cumsum(spreads[,1]), na.rm=T), by=10),-1), las=1)
axis(2, round(seq(-800, 800, by=10),-1), las=1)

lines(cumsum(spreads[,2]), type="l", col="red")
lines(cumsum(spreads[,3]), type="l", col="blue")