#=========================================================
#API functions to send HTTP requests for ThaiBMA Bond Data|
#Last Update: 6th Oct 2016                                |
#Author: Bryan Chik, Owen Ou                              |
#                                                         |
# Query functions                                         |
#   getSingleThaiBondTC                                   |
#   getMultipleThaiBondTC                                 |
#   getSingleThaiBondCouponRef                            |
#   getSingleThaiBondAmoortization                        |
#   getMultipleThaiBondAllData                            |
#   getHistoricalRating                                   |
#   getThaiLatestIssue                                    |
#   getBondPricing                                        |
#   getThaiFIUniv                                         |             
#==========================================================

library('httr')
library('XML')
library('data.table')


#Function to get single bond data
#Input: local Thai FI data (string)
#Output the respective bond terms (table)

getSingleThaiBondTC <- function(localTicker, token)
{
  #Token URL
  tokenURL <- "http://www.thaibma.or.th/EN/BondInfo/BondFeature/Issue.aspx"

  #Get URL for bond terms
  bondfeatureURL = paste("http://www.thaibma.or.th/issue/feature?Symbol=",localTicker,sep="")
    
  #Send HTTP GET request with specific headers
  bondfeature <- GET(bondfeatureURL, add_headers(Token = token$token, timestamp = token$timestamp, Referer= tokenURL))
    
  #return data
  return(as.data.table(do.call(cbind,content(bondfeature))))

}

#=======================================
# Function to get bond coupon payment reference 
getSingleThaiBondCouponRef <- function(localTicker, token)
{
    #Token URL
    tokenURL <- "http://www.thaibma.or.th/EN/BondInfo/BondFeature/Issue.aspx"
    
    #Get URL for bond coupon payment reference
    couponRefURL = paste("http://www.thaibma.or.th/issue/couponpaymentreference?Symbol=",localTicker,sep="")
    
    #Send HTTP GET request with specific headers
    couponRef <- GET(couponRefURL, add_headers(Token = token$token, timestamp = token$timestamp, Referer= tokenURL))
    
    #Return the data
    return(list2df(content(couponRef)))
}
#=======================================
#Function to XI Data
#Input: local Thai FI data (string)
#Output the respective bond terms (table)

getSingleThaiBondXIXA <- function(localTicker, token)
{
    #Token URL
    tokenURL <- "http://www.thaibma.or.th/EN/BondInfo/BondFeature/Issue.aspx"
    
    #Get URL for bond terms
    bondXIURL = paste("http://www.thaibma.or.th/issue/xi?Symbol=",localTicker,"&isXA=false",sep="")
    bondXAURL = paste("http://www.thaibma.or.th/issue/xi?Symbol=",localTicker,"&isXA=true",sep="")
    
    #Send HTTP GET request with specific headers
    bondXI <- GET(bondXIURL, add_headers(Token = token$token, timestamp = token$timestamp, Referer= tokenURL))
    bondXA <- GET(bondXAURL, add_headers(Token = token$token, timestamp = token$timestamp, Referer= tokenURL))
    
    #format the XiDate, PlanPaymentDate, PeriodFrom, PeriodTo date
    bondXI <- lapply(content(bondXI), function(x) { for(i in 5:8){x[[i]] <- formatDates(x[[i]], "%Y%m%d")}; x})
    bondXA <- lapply(content(bondXA), function(x) { for(i in 5:8){x[[i]] <- formatDates(x[[i]], "%Y%m%d")}; x})
    
    #return data
    return(list2df(c(bondXI,bondXA)))

}


#=======================================
# Function to get bond amortization information
getSingleThaiBondAmortization <- function(localTicker, token)
{
    #Token URL
    tokenURL <- "http://www.thaibma.or.th/EN/BondInfo/BondFeature/Issue.aspx"
    
    #Get URL for bond coupon payment reference
    amortizationURL = paste("http://www.thaibma.or.th/issue/amortizingschedule?symbol=",localTicker,sep="")
    
    #Send HTTP GET request with specific headers
    amortization <- GET(amortizationURL, add_headers(Token = token$token, timestamp = token$timestamp, Referer= tokenURL))
    
    return(list2df(content(amortization)))
}

#=======================================

#Function to get multiple bond data
#Input: local Thai FI data (array of string)
#Output the respective bond terms (table)

#=======================================
#Function to get multiple bond data, including T&C, coupon reference, amortization and other items as needed

getMultipleThaiBondInfo <- function(localTickers)
{
  #Validate symbol in the list
  validate <- t(matrix(unlist(lapply(localTickers, validateSymbol)),2))
  
  #Seperate valid symbols and invalid ones
  validSymbol <- localTickers[validate[,1]==1]
  invalidSymbol <- localTickers[validate[,1]==0]
  
  #Generate token and record start time
  token <- getToken()
  startTime <- Sys.time()
  
  #Initiate empty list to store the data
  termdata <- initBondInfoContainer()

  
  #Once we have the valid symbols, loop through those to get the terms and bind them
  for(i in 1:length(validSymbol))
  {
    message("Fetching:", validSymbol[i]," Progress:", round(i/length(validSymbol)*100,digits = 2),"%")
    termdata <- rbind(termdata, cbind(getSingleThaiBondTC(validSymbol[i], token),getSingleThaiBondCouponRef(validSymbol[i], token),
                          getSingleThaiBondXIXA(validSymbol[i], token),getSingleThaiBondAmortization(validSymbol[i], token)),fill = TRUE)
    
    #Time delay after each data fetch
    #Sys.sleep(runif(1,0.2,1))
    
    #Reset token and timing after 20 mins
    if (Sys.time()-startTime > 1200)
    {
      token <- getToken()
      startTime <- Sys.time()
    }
  }
  
  #Remove the first line of blank space..
  termdata <- termdata[-1,]
  
  
  #Bind all the term data from the valid symbols
  #alltermdata <- rbindlist(termdata,fill=TRUE)
  
  #Initiate empty list to store the output
  output <- list()
  
  #Return a list containing terms data and the invalid symbols
  output$validdata <- formatBondInfo(termdata)
  output$invalidsym <- invalidSymbol
  
  return(output)
}

#==========================================================================
#Function to get latest bond rating, if any
#Input: local ticker, Token
#Output: ratings
getRating <- function(localTicker, token)
{
    #Token URL
    tokenURL <- "http://www.thaibma.or.th/EN/BondInfo/BondFeature/Issue.aspx"
    
    #Get URL for bond ratings
    bondratingURL_local = paste("http://www.thaibma.or.th/issue/rating?Symbol=",validate$internalSymbol,"&IsLocalAgency=true",sep="")
    bondratingURL_foreign = paste("http://www.thaibma.or.th/issue/rating?Symbol=",validate$internalSymbol,"&IsLocalAgency=false",sep="")
    
    #Send HTTP GET request with specific headers
    bondratinglocal <- GET(bondratingURL_local, add_headers(Token = token$token, timestamp = token$timestamp, Referer= tokenURL))
    bondratingforeign <- GET(bondratingURL_foreign, add_headers(Token = token$token, timestamp = token$timestamp, Referer= tokenURL))
    
    #Check dimensions as some of the bonds have no ratings, both local and foreign
    #atingSize$local <- length(content(bondratinglocal))
    #ratingSize$foreign<- length(content(bondratingforeign))
    
    #initiate empty list
    bondrating <- list()
    bondrating[[1]] <- rbindlist(content(bondratinglocal))
    bondrating[[2]] <- rbindlist(content(bondratingforeign))
    
    #return rating
    return(rbindlist(bondrating))
}

#==========================================================================
#Function to get historical bond rating, if any
#Input: local ticker, Token
#Output: ratings
getHistoricalRating <- function(localTicker, token)
{
  #First, validate whether the symbol is valid
  validate <- validateSymbol(localTicker)
  
  #If the validation status is 0, stop function
  if(validate$Status == 0) {stop("Symbol Not Found")}
  
  #else continue to query data
  else
  {
    
    #Token URL
    tokenURL <- "http://www.thaibma.or.th/EN/BondInfo/BondFeature/Issue.aspx"
    
    #Get URL for bond ratings
    bondratingURL = paste("http://www.thaibma.or.th/issue/ratinghistorical?symbol=",localTicker,sep="")
    
    #Send HTTP GET request with specific headers
    bondrating <- GET(bondratingURL, add_headers(Token = token$token, timestamp = token$timestamp, Referer= tokenURL))
    
    #Check dimensions as some of the bonds have no ratings
    ratingSize <- length(content(bondrating))
    
    #return rating if it is rated
    if(ratingSize == 0)
    {
      message("Not Rated")
      return(NULL)
    }
    else
    {return(rbindlist(content(bondrating)))}
  }
}

#==========================================================================
#Function to get latest bond clean price, if any
#Input: local ticker
#Output: price info
getBondPricing <- function(localTicker)
{
  #Create bond price URL using the ticker input
  bondPriceURL <- paste("http://www.thaibma.or.th/bondprice/GetPrice/null/", localTicker, sep="")
  
  #Send HTTPS request to get bond price data
  priceInfo <- list2df(content(GET(bondPriceURL)))
  
  #Don't need the issuer column, so going to delete it
  priceInfo$IssueID <- NULL
  
  #Format "Date" column in the returned JSON, which is the date of query
  priceInfo$Date <- format(as.Date(priceInfo$Date), "%Y%m%d")
  priceInfo$LastQuotedDate <- format(as.Date(priceInfo$LastQuotedDate, format="%d-%b-%Y"), "%Y%m%d")
  priceInfo$LastTradedDate <- format(as.Date(priceInfo$LastTradedDate, format="%d-%b-%Y"), "%Y%m%d")
  
  #return object
  return(priceInfo)
}

#==========================================================
#Function to return latest issue for different bond types
#bondType can either be: TB, GB, SA, SOE, CORP, CP, FB, USD and CNY
getThaiLatestIssue <- function(bondType)
{
  #Check whether bondType is correctly input
  if(sum(bondType == c("TB", "GB", "SA", "SOE", "CORP", "CP", "FB", "USD", "CNY")) == 0) {message("Wrong Bond Type as input")}
  
  #Once it has gone through..
  else{
    
    #Create query URL
    url <- paste("http://www.thaibma.or.th/registeredbond/GetBondOutstandingList/", bondType, sep="")
    
    #Returned all Issue data and add a column of date
    allIssue.data <- list2df(content(GET(url)))
    
    #Combine as of date, based on computer run-time
    allIssue.data <- data.frame(rep(format(Sys.Date(),format="%Y%m%d"), nrow(allIssue.data)), allIssue.data$Symbol, allIssue.data$Outstanding, allIssue.data$CurrencyCode, allIssue.data$IssuerTypeName, stringsAsFactors=FALSE)
    colnames(allIssue.data) <- c("AsOfDate","Symbol","Outstanding","CurrencyCode", "IssuerType")

    return(allIssue.data)
  }
}

#==========================================================
#Function to return latest issue for different bond types
#bondType can either be: TB, GB, SA, SOE, CORP, CP, FB, USD and CNY

getThaiFIUniv <- function()
{
  return(list2df(lapply(c("TB", "GB", "SA", "SOE", "CORP", "CP", "FB", "USD", "CNY"), getThaiLatestIssue)))
}

#==========================================================
getIssuers <- function(){
  
  issuerURL <- "http://www.thaibma.or.th/issuer/listissuer/home"
  
  temp <- content(GET(issuerURL)) #download issuer data
  issuerList <- as.data.frame(t(do.call(cbind,temp)))
  return(issuerList$IssuerAbbrName)
  
}

#==========================================================
getHistIssues <- function(Issuer){
  
  longBondURL <- paste("http://www.thaibma.or.th/issuer/regissue?abbrName=",Issuer,"&term=long", sep = "")
  shortBondURL <- paste("http://www.thaibma.or.th/issuer/regissue?abbrName=",Issuer,"&term=short", sep = "")
  
  longTemp <- content(GET(longBondURL))
  longList <- list()
  if (length(longTemp) > 0){
    longList <- as.data.frame(t(do.call(cbind,longTemp)))
    longList$TermLength <- "Long"
    longList$IssuerAbbrName = Issuer
  }
  
  shortTemp <- content(GET(shortBondURL))
  shortList <- list()
  if (length(shortTemp) > 0){
    shortList <- as.data.frame(t(do.call(cbind,shortTemp)))
    shortList$TermLength <- "Short"
    shortList$IssuerAbbrName = Issuer
  }
  
  bondList <- rbind(longList,shortList)
  
  return(bondList)
  
}

#==========================================================
outputAllBonds <- function(){
  
  issuerList <- getIssuers()
  
  batchSize <- 50
  batchNumber <- (length(issuerList)-1)%/%100
  
  for (i in 0:4){
    output2 <- list()
    
    for (j in (100*i+1):min(100*(i+1),length(issuerList))){
      
      print(issuerList[[j]])
      bondList <- getBonds(URLencode(issuerList[[j]]))
      
      output2 <- rbindlist(list(output2,bondList), fill = TRUE)
      
    }
    
    output2 <- data.frame(lapply(output2,as.character))
    write.table(output2, "C:/Users/oou/Desktop/Analytics/Thai Bonds/All Debentures.csv", append = TRUE,
                quote = TRUE, sep = ",", col.names = (i==0))
  }
  
}
