#  source("C:\\R Toolbox\\makeFactorsConnection.R")

makeFactorsConnection <- function(){

# Creates global connection to Factors database ODBC driver and calls it 'factorDB' for use with all functions
#
# Example:
# makeFactorsConnection()
#

if (exists("factorDB")){
   #print("connection exists")
   if (class(factorDB)=="RODBC"){
      #print("factorDB is right class")
   } else{
      print("ERROR: factorDB WRONG class!")
   }
} else{
   print("No factorDB connection existed. New connection created")
   factorDB        <<- odbcConnect("factors")  #make global connection
}

}