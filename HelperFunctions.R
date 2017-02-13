##=========================================================
#API functions to send HTTP requests for ThaiBMA Bond Data|
#Last Update: 6th Oct 2016                                |
#Author: Bryan Chik, Owen Ou                              |
#                                                         |
#Function List:                                           |
# Helper functions                                        |
#   getToken                                              |
#   validateSymbol                                        |
#   list2df                                               |
#   formatBondInfo                                        |
#==========================================================

library('httr')
library('XML')
library('data.table')

#=====================================================HELPER FUNCTIONS==================================================


#Function to generate token to be used in the GET request header to ThaiBMA server
getToken <- function()
{
  #ThaiBMA data query URL
  tokenURL <- "http://www.thaibma.or.th/EN/BondInfo/BondFeature/Issue.aspx"
  
  
  #Parse HTML Code source
  tokenHTML <- htmlParse(tokenURL)
  
  #Process to get token and timestamp
  token <- xpathSApply(tokenHTML, "//input[@name='ctl00$ctl00$token']", xmlAttrs)[5]
  timestamp <- xpathSApply(tokenHTML, "//input[@name='ctl00$ctl00$time']", xmlAttrs)[5]
  
  #Clear out parsed HTML for better memory
  rm(tokenHTML)
  
  #Initiate empty output list
  output <- list()
  
  #Return token and timestamp
  output$token <- token
  output$timestamp <- timestamp
  return(output)
  
}  

#=================================================================
#Function to Validate Symbol within ThaiBMA site
#Input: Local Thai FI Symbol (string), token 
#Output: Object with validate status (1 or 0) and the internal symbol

validateSymbol <- function(localTicker)
{
  #ThaiBMA URL for symbol Validation
  validateURL <- paste("http://www.thaibma.or.th/issue/ValidateSymbol?Symbol=", localTicker, sep="")
  
  #Print message...
  message("Validating:", localTicker)
  
  #Get validation statis
  internalSymbol.status <- content(GET(validateURL))[[1]]
  internalSymbol.symbol <- content(GET(validateURL))[[2]]
  
  return(list(Status = internalSymbol.status, internalSymbol = internalSymbol.symbol))
}

#=======================================
#Helper function to transform a list to dataframe. List elements have to be of same structure in order to work
#Input: List
#Output: collapsed data table with 1 observation per symbol

list2df <- function(list)
{
  #Check list
  if(length(list) >= 2)
  {
    list_length <- unlist(lapply(list,length)) #Get length within each element in the list
    if(length(unique(list_length)) != 1)
    {
      message("List Structure Not Identical!")
    }
    
    else
    {
      output <- as.data.table(do.call(rbind, lapply(list, data.frame, stringsAsFactors=FALSE)))
      return(output[, lapply(.SD, paste, collapse=";"), by=Symbol]) #return the output as a collapsed frame
    }
  }
  else if (length(list) == 1) #when there are no repeated entries, return the original list
  {
    return(as.data.table(do.call(rbind, lapply(list, data.frame, stringsAsFactors=FALSE))))
  }
  else if (length(list) == 0) #when the list is empty, return a dummy list
  {
    return(as.data.table(list(Symbol = NA)))
  }
  
}

#========================================================
#Convert numbers from millions to actual units (split the strings > multiply by a million > collapse the strings)
convertMillions <- function (x){
  ifelse(x=="","",lapply(lapply(strsplit(x,";"),function (y) as.numeric(y)*1e+6),function (z) paste(z,collapse=";")))
}

#========================================================
# Function to adjust data format for FactSet modelling
formatBondInfo <- function(bondInfo){
  
  #Remove symbol from name
  bondInfo[,SymbolTitle:= gsub(paste(Symbol," : | \\(",Symbol,"\\)",sep = ""),"",SymbolTitle),by = Symbol]
  
  #Remove "0" from IsinEn and IsinTh column
  bondInfo$IsinEn[bondInfo$IsinEn==0] <- ""
  bondInfo$IsinEn[bondInfo$IsinTh==0] <- ""
  
  #Day count basis
  bondInfo[, ':=' (
    AccrualBasisNameEn = gsub("Actual","ACT",AccrualBasisNameEn)
  
    #Date format from YYYY-MM-DDT00:00:00 to YYYYMMDD
    #,RegistrationDate = gsub("-|T[0:]{8}","",RegistrationDate)
    ,RegistrationDate = formatDates(RegistrationDate, "%Y%m%d")
    
    #Get coupon rate from reference text
    ,CouponRate = ifelse (grepl(";",ReferenceText),"",gsub("[A-Za-z]+: |%","",ReferenceText))
    
    ,SteppedRate = ifelse (grepl(";",ReferenceText),gsub("[A-Za-z]+: |%","",ReferenceText),"")
    
    #Stepped coupon rates start dates
    ,SteppedDates = ifelse (grepl(";",bondInfo$ReferenceText),FromDate,"")
    
    #Convert issue size from millions to actual unit
    ,IssueSize = as.numeric(IssueSize) * 1000000
  )]
  
  #Format dates
  dateCols = c("MaturityDate","IssuedDate","FromDate","ToDate","SteppedDates")
  #for (j in dateCols) set(bondInfo, j = j, value = gsub("-|T[0:]{8}","",bondInfo[[j]]))
  for (j in dateCols) set(bondInfo, j = j, value = formatDates(bondInfo[[j]], "%Y%m%d"))
  
  #Retrieve the first payment date from the list of all pay dates
  #bondInfo[,FirstPayDate := gsub("-|;.*|T[0:]{8}","",PlanPaymentDate)]
  bondInfo[,FirstPayDate := formatDates(PlanPaymentDate, "%Y%m%d")]
  
  bondInfo[as.character(PrincipalPayment) == "", PrincipalType := "At Maturity"]
  bondInfo[grepl("Amortizing",PrincipalPayment), PrincipalType := "Sinkable"]
  bondInfo[grepl("Perpetual",PrincipalPayment), PrincipalType := "Perpetual"]
  
  #For bonds that have no payment frequency info from the web, substitute with payment intervals
  #Calculate payment intervals by dividing issue term by number of payments
  bondInfo[CouponFrequencyNameEn=="",CouponFrequencyNameEn:=
        as.character(round(12*as.numeric(IssueTerm)/length(regmatches(isXA,gregexpr("[01]",isXA))),digits=0))]
  
  frequencyFormat <- function(freq){
    if(freq == "Quarterly" || freq == "4") "Quarterly"
    else if (freq == "Semi-annually" || freq == "6") "Semi-annual"
    else if (freq == "12") "Annual"
    else if (freq == "At Maturity" || freq == "Inf") "None"
    else ""
  }
  
  bondInfo[,CouponFrequencyNameEn:= frequencyFormat(CouponFrequencyNameEn), by = Symbol]
  
  #Fix Issue Remark with extra namespaces
  bondInfo$IssueRemark <- gsub("[\r\n]","",bondInfo$IssueRemark)
  
  #Schedule of sink dates (which are named CouponDates in the JSON)
  #bondInfo$SinkDates = ifelse(bondInfo$CouponDate == "","",gsub("-|T[0:]{8}","",bondInfo$CouponDate))
  bondInfo$SinkDates = ifelse(bondInfo$CouponDate == "","",formatDates(bondInfo$CouponDate, "%Y%m%d"))
  
  #Convert sink amounts from millions to actual units (split the strings > multiply by a million > collapse the strings)
  bondInfo$PrincipalAmount <- unlist(lapply(bondInfo$PrincipalAmount,convertMillions))
  
  return(bondInfo[,unique(names(bondInfo)),with = FALSE]) #Only returns unique columns of the original table
}

#========================================================
# Function to initiate empty data frame
initBondInfoContainer <- function()
{
  colnames <-c("IssueID","IssueLegacyId","Symbol","SymbolTitle","RegistrationDate","IssueNameTh","IssueNameEn",
               "IsinTh","IsinEn","ClaimNameEn","SecureType","PrincipalPayment","CurrencyCode","InitialPar", "CurrentPar",
               "IssueSize","OutstandingSize","IssuedDate","MaturityDate","IssueTerm","CouponFrequencyNameEn",
               "AccrualBasisNameEn","EmbbeddedOption","DistributionNameEn","CollateralRemark","IssueRemark","RiskLevelId","RiskLevel",
               "issuer_id","issuer_code","IssueLegacyID","FactorNo","OffsetRate","ReferenceText","MaxRate", "MinRate","FromDate",
               "ToDate","NoXDaysBefore","NoXLastdaysBefore","isXA","XiDate", "PlanPaymentDate", "PeriodFrom", "PeriodTo", "CouponRate", "registrar",
               "Period", "ProspectusId", "SteppedRate", "SteppedDates", "FirstPayDate", "PrincipalType", "CouponDate", "PrincipalAmount")
  
  bondInfoContainer <- data.frame(matrix(numeric(),1,length(colnames)), stringsAsFactors=FALSE)
  names(bondInfoContainer) <- colnames
  
  return(bondInfoContainer)
}
#========================================================
#Function to format TBMA server dates to any user-defined formats, format is from R base package
formatDates <- function(TBMADate, format)
{
  temp <- as.Date(TBMADate, format="%Y-%m-%d")
  return(format(temp,format))
}
#====================================================END HELPER FUNCTION===============================================