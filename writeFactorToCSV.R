#  source("C:\\R Toolbox\\writeFactorToCSV.R")


writeFactorToCSV <- function(fidList, funcNameList, descrList, dateList, idxTicker, filePath){

# Write factor scores to csv files
#
# Example:
# fidList         <- c(2,3)
# funcNameList    <- c("roa", "margin")
# descrList       <- c("ROA: NI / Total Assets", "Margin: Gross Margin / Net Sales")
# dateList        <- c(20100331, 20100228, 20100131)
# idxTicker       <- "I0003"
# filePath        <- "F:\\Data\\Factors\\"
#
# writeFactorToCSV(fidList, funcNameList, descrList, dateList, idxTicker, filePath)
# 

for (i in 1:length(funcNameList)){
   fid            <- fidList[i]
   funcName       <- funcNameList[i]
   descr          <- descrList[i]
   
   fileName       <- paste(filePath, funcName, ".csv", sep="")
   
   # Check if file exists, if so delete. Create new file with header
   if (file.exists(fileName)){
      file.remove(fileName)
   } 
   cat("fid,funcname,descr,dt,gvkey,score,blank\n", file=fileName)
   
   print(funcName)
   
   for (dt in dateList){
      print(dt)
      # Create Universe
      secList           <- indexMembers(idxTicker, dt)
      
      # Create factor
      factor         <- eval(parse(text=paste(funcName,"(secList, dt)$values", sep="")))  #bookToMV(secList, dt)$values
      
      # Make output 
      outputStr <- as.matrix(paste(fid, funcName, descr, dt, secList, factor, "\n", sep=","), length(secList), 1)
      
      # Write to csv
      cat(outputStr, file=fileName, append=T)
   
   }
   cat(funcName, "data written to", fileName, "\n")
}

}