#  source("C:\\R Toolbox\\mostRecentDate.R")


mostRecentDate <- function(dt, dateVector){
# finds most recent date that is not in the future.

dt          <- as.integer(dt)
dateVector  <- as.integer(dateVector)

idx         <- dateVector <= dt

sort(dateVector[idx], decreasing=T)[1]

}