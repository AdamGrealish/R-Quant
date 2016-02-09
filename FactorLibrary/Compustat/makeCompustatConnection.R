#  source("C:\\R Toolbox\\FactorLibrary\\Compustat\\makeCompustatConnection.R")

makeCompustatConnection <- function(){

# Creates global connection to Compustat database ODBC driver and calls it 'csDB' for use with all functions
#
# Example:
# makeCompustatConnection()
#

if (exists("csDB")){
   #print("connection exists")
   if (class(csDB)=="RODBC"){
      #print("csDB is right class")
   } else{
      print("ERROR: csDB WRONG class!")
   }
} else{
   print("No csDB connection existed. New connection created")
   csDB        <<- odbcConnect("compustat")  #make global connection
}

}