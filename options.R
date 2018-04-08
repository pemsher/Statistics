# Pemba Sherpa
# Unusual Options activity tracker
# This contains methods scans for option series with volume > open interest
# When it finds option series with volume > OI, it then scans the option time and sale
# to find large block orders
# ==============================================
# usage:  symbols : array of underlying or "dow" / "nasdaq"
#         expiration: YYYY-MM-DD format options expiration
#         seriesVolume: default 1000, volume for the option series
#         saleVolume: default 500, block volume for each sale transaction of the option series
# Note:
# The timesales api gives aggregate of volume transaction based on minute as the smallest unit
# This can give some false positive for large orders
# Once you have this information, I recommend using your brokerage software which can give you more granular
# time and sale data to identify block orders

library(stringr)
library(httr)
library(jsonlite)
library(stats)

getOptionChain <- function(symbols,expiration,seriesVolume=1000,saleVolume=500){

  OPTIONS_CHAINS_API_END_POINT <- "https://sandbox.tradier.com/v1/markets/options/chains?"

  TS_API_END_POINT <- "https://sandbox.tradier.com/v1/markets/timesales?"

  DOW_30 <- c("AAPL","CSCO","BA","GE","MSFT","JPM","IBM","INTC","WMT","JNJ","NKE","KO","CVX","UNH","UTX","MCD","PG","DIS","TRV","XOM","VZ","MMM","MRK","V","DWDP","HD","PFE","GS","CAT","AXP")

  NASDAQ_30 <- c("FRSH","OPTT","WLB","JIVE","EXAC","NVCR","SLAB","KLXI","NRCIA","AKTX","FNLC","SCON","EXAS","IBKC","FARM","ATEC","FARO","TSLA","CVCY","CVCO","FNJN","CMRX","NRCIB","FEYE","POPE","LTEA","SPHS","FRTA","AGLE","NVCN")

  NASDAQ_100 <- c("AAL","AAPL","ADBE","ADI","ADP","ADSK","AKAM","ALGN","ALXN","AMAT","AMGN","AMZN","ATVI","AVGO","BIDU","BIIB","BMRN","CA","CELG","CERN","CHKP","CHTR","CTRP","CTAS","CSCO","CTXS","CMCSA","COST","CSX","CTSH","DISCA","DISCK","UNCH","DISH","DLTR","EA","EBAY","ESRX","EXPE","FAST","FB","FISV","FOX","FOXA","GILD","GOOG","GOOGL","HAS","HSIC","HOLX","ILMN","INCY","INTC","INTU","ISRG","JBHT","JD","KLAC","KHC","LBTYK","LILA","LBTYA","QRTEA","MELI","MAR","MAT","MDLZ","MNST","MSFT","MU","MXIM","MYL","NCLH","NFLX","NTES","NVDA","PAYX","BKNG","PYPL","QCOM","REGN","ROST","SHPG","SIRI","SWKS","SBUX","SYMC","TSCO","TXN","TMUS","ULTA","VIAB","VOD","VRTX","WBA","WDC","XRAY","IDXX","LILAK","LRCX","MCHP","ORLY","PCAR","STX","TSLA","VRSK","WYNN","XLNX")
  
  if (symbols[1] == "dow")
  {
    symbols <- DOW_30
  }

  if (symbols[1] == "nasdaq")
  {
    symbols <- NASDAQ_30
  }
  
  if (symbols[1] == "nasdaq100")
  {
    symbols <- NASDAQ_100
  }
  #print(LOOK_BACK)
  for(symIdx in 1:length(symbols))
  {
    optionChainURL <- paste(OPTIONS_CHAINS_API_END_POINT,"symbol=",str_trim(symbols[symIdx],side="both"),"&expiration=",str_trim(expiration,side="both"),sep="")
    optionsChainData <- fromJSON(content(GET(url=optionChainURL,add_headers(Authorization="Bearer i6jVdhOxuVDHAxn88AOajVuvZ3YE",'Content-Type'="application/json")),"text"),flatten=FALSE)
    #print(optionChainURL)
    #print(optionsChainData)
    #print(length(optionsChainData$options$option))
    option_len <- length(optionsChainData$options$option[,"symbol"])
    #print(paste("len ", length(optionsChainData$options$option[,"symbol"])))
    for (i in 1:option_len)
    {
      if (option_len < 1)
      {
        print("**************************************************************")
        print(paste(symbols[symIdx],"<",expiration,">","[NO OPTIONS SERIES]"))
        print("**************************************************************")
        break
      }
     # print(i)
    #  print(paste(optionsChainData$options$option[i,"symbol"]," ",optionsChainData$options$option[i,"volume"]," ",optionsChainData$options$option[i,"open_interest"]))
     # print(paste(optionsChainData$options$option[i,"volume"]," ",optionsChainData$options$option[i,"open_interest"]))
      if(optionsChainData$options$option[i,"volume"] > seriesVolume & (optionsChainData$options$option[i,"volume"] > optionsChainData$options$option[i,"open_interest"]))
      {
        print("=================================================================================")
        print(paste(optionsChainData$options$option[i,"root_symbol"],optionsChainData$options$option[i,"strike"],"[",optionsChainData$options$option[i,"option_type"],"]",optionsChainData$options$option[i,"expiration_date"],optionsChainData$options$option[i,"symbol"]," ",optionsChainData$options$option[i,"volume"]," ",optionsChainData$options$option[i,"open_interest"]))
        print("--------------------------------------------------------------------------------")
        print(paste("Searching...",optionsChainData$options$option[i,"symbol"]," Time of Sales..."))
        print("================================================================================")
        #Prepare option quote url to get time of sales
        optionQuoteURL <- paste(TS_API_END_POINT,"symbol=",str_trim(optionsChainData$options$option[i,"symbol"],side="both"),sep="")
        #print(optionQuoteURL)
        optionsQuoteData <- fromJSON(content(GET(url=optionQuoteURL,add_headers(Authorization="Bearer i6jVdhOxuVDHAxn88AOajVuvZ3YE",'Content-Type'="application/json")),"text"),flatten=FALSE)
        #print(optionsQuoteData)
        #filter to show block order (order >= 500)
        sales_len <- length(optionsQuoteData$series$data[,"volume"])
        for(idx in 1:sales_len)
        {
          if (optionsQuoteData$series$data[idx,"volume"] > saleVolume)
          {
            #print(paste("[Found Large Order] at ",idx ))
            print(optionsQuoteData$series$data[idx,])
          }
        }

      }
    }
  }
}