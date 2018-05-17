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
library(corrplot)
library(stringr)
library(httr)
library(jsonlite)
library(stats)

getOptionChain <- function(symbols,expiration,seriesVolume=1000,saleVolume=500){

  FILE_NAME = paste(str_trim("C:/pemsher/Statistics/scan_",side="both"),str_trim(Sys.Date(),side="both"),"_",str_trim(format(Sys.time(), "%H-%M-%OS3"),side="both"),sep="")
   
  sink(FILE_NAME,append = FALSE,split = TRUE)
  
  OPTIONS_CHAINS_API_END_POINT <- "https://api.tdameritrade.com/v1/marketdata/chains?apikey=pemsher007%40AMER.OAUTHAP&"

  TS_API_END_POINT <- "https://sandbox.tradier.com/v1/markets/timesales?"
  
  SPY_100 <- c("AAPL","ABBV","ABT","ACN","AGN","AIG","ALL","AMGN","AMZN","AXP","BA","BAC","BIIB","BK","BKNG","BLK","BMY","BRK.B","C","CAT","CELG","CHTR","CL","CMCSA","COF","COP","COST","CSCO","CVS","CVX","DHR","DIS","DUK","DWDP","EMR","EXC","F","FB","FDX","FOX","FOXA","GD","GE","GILD","GM","OOG","GOOGL","GS","HAL","HD","HON","IBM","INTC","JNJ","JPM","KHC","KMI","KO","LLY","LMT","LOW","MA","MCD","MDLZ","MDT","MET","MMM","MO","MN","MRK","MS","MSFT","NEE","NKE","ORCL","OXY","PEP","PFE","PG","PM","PYPL","QCOM","RTN","SBU","SLB","SO","SPG","T","TGT","TWX","TXN","UNH","UNP","UPS","USB","UTX","V","VZ","WBA","WFC","WMT","XOM")

  DOW_30 <- c("AAPL","CSCO","BA","GE","MSFT","JPM","IBM","INTC","WMT","JNJ","NKE","KO","CVX","UNH","UTX","MCD","PG","DIS","TRV","XOM","VZ","MMM","MRK","V","DWDP","HD","PFE","GS","CAT","AXP")

  NASDAQ_30 <- c("FRSH","OPTT","WLB","JIVE","EXAC","NVCR","SLAB","KLXI","NRCIA","AKTX","FNLC","SCON","EXAS","IBKC","FARM","ATEC","FARO","TSLA","CVCY","CVCO","FNJN","CMRX","NRCIB","FEYE","POPE","LTEA","SPHS","FRTA","AGLE","NVCN")

  NASDAQ_100 <- c("AAL","AAPL","ADBE","ADI","ADP","ADSK","AKAM","ALGN","ALXN","AMAT","AMGN","AMZN","ATVI","AVGO","BIDU","BIIB","BMRN","CA","CELG","CERN","CHKP","CHTR","CTRP","CTAS","CSCO","CTXS","CMCSA","COST","CSX","CTSH","DISCA","DISCK","UNCH","DISH","DLTR","EA","EBAY","ESRX","EXPE","FAST","FB","FISV","FOX","FOXA","GILD","GOOG","GOOGL","HAS","HSIC","HOLX","ILMN","INCY","INTC","INTU","ISRG","JBHT","JD","KLAC","KHC","LBTYK","LILA","LBTYA","QRTEA","MELI","MAR","MAT","MDLZ","MNST","MSFT","MU","MXIM","MYL","NCLH","NFLX","NTES","NVDA","PAYX","BKNG","PYPL","QCOM","REGN","ROST","SHPG","SIRI","SWKS","SBUX","SYMC","TSCO","TXN","TMUS","ULTA","VIAB","VOD","VRTX","WBA","WDC","XRAY","IDXX","LILAK","LRCX","MCHP","ORLY","PCAR","STX","TSLA","VRSK","WYNN","XLNX")

  if (symbols[1] == "spy100")
  {
    symbols <- SPY_100
  }
  
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

  if (symbols[1] == "equity")
  {
    equity <- c(SPY_100,NASDAQ_100,NASDAQ_30,DOW_30)
    symbols <- unique(equity)
  }
  
  #print(LOOK_BACK)
  for(symIdx in 1:length(symbols))
  {
  #  print(paste("[SEARCHING]",symbols[symIdx], symIdx, " of ",length(symbols)))
    
    # pause after every 50 calls to stay under api quota
    if (symIdx %% 50 == 0)
    {
      print("[****************PAUSING*******************************]")
      Sys.sleep(60)
    }
    
    optionChainURL <- paste(OPTIONS_CHAINS_API_END_POINT,"symbol=",str_trim(symbols[symIdx],side="both"),"&expMonth=",str_trim(expiration,side="both"),sep="")
    optionsChainData <- fromJSON(content(GET(url=optionChainURL,add_headers('Content-Type'="application/json")),"text"),flatten=FALSE)
    #print(optionChainURL)
    #print(optionsChainData)
    #print(length(optionsChainData$options$option))
    option_len <- length(optionsChainData$callExpDateMap$option[,"symbol"])
    #print(paste("len ", length(optionsChainData$options$option[,"symbol"])))
  }
  sink()
  #close(FILE_NAME)
}