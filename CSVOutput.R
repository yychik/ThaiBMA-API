##=========================================================
#Scrtpt to Perform QA Check on Thai FI                    |
#Last Update: 20th Oct 2016                               |
#Author: Bryan Chik, Owen Ou                              |
#                                                         |
#==========================================================

source('C:/Thai Bond API/HelperFunctions.R', echo=TRUE)
source('C:/Thai Bond API/getThaiBondTC.R', echo=TRUE)

  #bondUniv <- getThaiFIUniv()
  bondUniv <- getThaiLatestIssue("CP")
  #bondList <- c("LB176A", "LB206A", "LB226A","LBA37DA","SBST215A", "SBST265A","EA207A","EA217A","ITD16D02A")
  bondList <- bondUniv$Symbol
  bondCount <-length(bondList)
  batchSize <- 50
  
  for (i in 0:((bondCount-1)%/%batchSize)){
    
    allData <- getMultipleThaiBondInfo(bondList[(batchSize*i+1):min(batchSize*(i+1),bondCount)])
    
    valid <- allData$validdata
    invalid <- allData$invalidsym
    
    write.table(valid,file="C:/Users/bchik/Desktop/ThaiBMA Outputs/CP.csv", append = TRUE, quote = TRUE,
                sep = "|", na = "", row.names = FALSE, col.names = (i==0))
    
    # write.table(invalid,file="C:/Users/oou/Desktop/Analytics/Thai Bonds/20150630 Invalid.csv", append = TRUE, quote = TRUE,
    #             sep = ",", na = "", row.names = FALSE, col.names = (i==0))
    
    message("Batch ", i+1,"/", (bondCount-1)%/%batchSize+1," done")
  }
  
