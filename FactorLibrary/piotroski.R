#  source("C:\\R Toolbox\\FactorLibrary\\piotroski.R")


piotroski <- function(secList, dt, naCutoff=4){

# Calculate Piotroski Score. Return factor ($values) and all pieces, raw and binary
#
# secList   <- list of gvkeys
# dt        <- date as integer
# naCutoff  <- if more than 'naCutoff' items of the 9 Piotroski items are missing -> NA
#
# Example:
#
# dt              <- 20100101
# secList         <- indexMembers("I0003", dt)
# dataObj         <- piotroski(secList, dt)
# piotroskiScore  <- dataObj$values 
#

# Get dates
dt1 <- dt
dt0 <- format(seq.Date(as.Date(as.character(dt), format="%Y%m%d"),len=2, by="-1 years")[2], format="%Y%m%d")

# Get data
roa1     <- getFactorScores(secList, dt1, "roa")$values
roa0     <- getFactorScores(secList, dt0, "roa")$values

margin1  <- getFactorScores(secList, dt1, "margin")$values
margin0  <- getFactorScores(secList, dt0, "margin")$values

cfo1     <- getFactorScores(secList, dt1, "cfo")$values
accruals1 <- getFactorScores(secList, dt1, "accruals")$values

liquid1  <- getFactorScores(secList, dt1, "liquid")$values
liquid0  <- getFactorScores(secList, dt0, "liquid")$values

lever1   <- getFactorScores(secList, dt1, "lever")$values
lever0   <- getFactorScores(secList, dt0, "lever")$values

turn1    <- getFactorScores(secList, dt1, "turn")$values
turn0    <- getFactorScores(secList, dt0, "turn")$values

shares1  <- csShares(secList, dt1)$adjustedShares
shares0  <- csShares(secList, dt0)$adjustedShares

# Raw values
ROA      <- roa1
dROA     <- roa1 - roa0
dMARGIN  <- margin1 - margin0
CFO      <- cfo1
dLIQUID  <- liquid1 - liquid0
dLEVER   <- lever1 - lever0
dTURN    <- turn1 - turn0
ACCRUAL  <- accruals1
EQOFFER  <- shares1 - shares0

# Binary
F.ROA       <- (ROA     >0)*1
F.dROA      <- (dROA    >0)*1
F.dMARGIN   <- (dMARGIN >0)*1
F.CFO       <- (CFO     >0)*1
F.dLIQUID   <- (dLIQUID >0)*1
F.dLEVER    <- (dLEVER  <0)*1    #leverage fell
F.dTURN     <- (dTURN   >0)*1
F.ACCRUAL   <- (ACCRUAL <=0)*1   #CFO>ROA
F.EQOFFER   <- (EQOFFER <=0)*1   #shares did not increase

# Make Piotroski score
scoreMat <-  cbind(
                   F.ROA       ,
                   F.dROA      ,
                   F.dMARGIN   ,
                   F.CFO       ,
                   F.dLIQUID   ,
                   F.dLEVER    ,
                   F.dTURN     ,
                   F.ACCRUAL   ,
                   F.EQOFFER
                  )
               
SCORE          <- apply(scoreMat, 1, sum, na.rm=T) 
naIdx          <- apply((apply(scoreMat, 1, is.na)), 2, sum) > naCutoff   # NA out stocks with more than 4 items missing
SCORE[naIdx]   <- NA
SCORE          <- as.matrix(SCORE)


# Store in object
dataObj <- NULL

dataObj$values    <- SCORE

dataObj$ROA       <- ROA      
dataObj$dROA      <- dROA     
dataObj$dMARGIN   <- dMARGIN  
dataObj$CFO       <- CFO      
dataObj$dLIQUID   <- dLIQUID  
dataObj$dLEVER    <- dLEVER   
dataObj$dTURN     <- dTURN    
dataObj$ACCRUAL   <- ACCRUAL  
dataObj$EQOFFER   <- EQOFFER  
                  
dataObj$F.ROA     <- F.ROA      
dataObj$F.dROA    <- F.dROA     
dataObj$F.dMARGIN <- F.dMARGIN  
dataObj$F.CFO     <- F.CFO      
dataObj$F.dLIQUID <- F.dLIQUID  
dataObj$F.dLEVER  <- F.dLEVER   
dataObj$F.dTURN   <- F.dTURN    
dataObj$F.ACCRUAL <- F.ACCRUAL  
dataObj$F.EQOFFER <- F.EQOFFER  

dataObj
}