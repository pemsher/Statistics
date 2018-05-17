# Pemba Sherpa
# Stock data analysis
# This contains methods to fetch stock data and do correlation analysis
# create an array of ticker symbols and call getStockCor function to generate
# a correlation matrix

library(corrplot)
library(stringr)
library(httr)
library(jsonlite)
library(stats)

getStockClose <- function(ticker,period){

  DAYS <- 0
  TODAY <- Sys.Date()
  LOOK_BACK <- Sys.Date() - DAYS
  STOCK_END_POINT <- "https://sandbox.tradier.com/v1/markets/history?symbol=";
  #print(LOOK_BACK)
  stockUrl <- paste(STOCK_END_POINT,str_trim(ticker[1],side="both"),"&start=",str_trim(LOOK_BACK,side="both"),"&end=",str_trim(TODAY,side="both"),sep="")
  stockData <- fromJSON(content(GET(url=stockUrl,add_headers(Authorization="Bearer i6jVdhOxuVDHAxn88AOajVuvZ3YE",'Content-Type'="application/json")),"text"),flatten=TRUE)
  print(stockUrl)
  print(stockData)
  #stockClosePrice <- vector();
  stockClosePrice <- closePrice(stockData,period,TRUE) 
  colnames(stockClosePrice)[2] <- ticker[1]

  if (length(ticker) > 1)
  {
    for(i in 2:length(ticker))
    {
      stockUrl <- paste(STOCK_END_POINT,str_trim(ticker[i],side="both"),"&start=",str_trim(LOOK_BACK,side="both"),"&end=",str_trim(TODAY,side="both"),sep="")
      stockData <- fromJSON(content(GET(url=stockUrl,add_headers(Authorization="Bearer i6jVdhOxuVDHAxn88AOajVuvZ3YE",'Content-Type'="application/json")),"text"),flatten=TRUE)
      print(stockUrl)
      print(stockData)
      stockClosePrice <- cbind(stockClosePrice,closePrice(stockData,period,FALSE))
      colnames(stockClosePrice)[i+1] <- ticker[i]
    }
  }
  print(stockClosePrice)
}

#
closePrice <- function (S,period=90,date=TRUE){
  
  #Need to add 1 as we discard one entry when we calculate change
  #This way we will still have period worth of observations

  print (S$history$day)
  S$history$day$close
  
  #print("print length")
  #print(S$history$day)
  #temp <- S$history$day[(NROW(S$history$day)-period):(NROW(S$history$day)),]
  #if (date==TRUE){
  #  temp <- subset(temp,select=c(date,close))
  #}
  #else{
  #  temp <- subset(temp,select=c(close))
  #}
  #print ("temp prints")
  #print (temp)
  #temp
}

closePricePerChange <- function(S){
  
  rows = nrow(S)
  columns = ncol(S)
  #print("row and columns")
  #print(rows)
  perChange <- S
  for (c in 2:columns){
    for(i in 2:rows){
      perChange[i,c] <- (S[i,c] - S[i-1,c])/S[i-1,c]
    }
  }
  #print(perChange[2:rows,])
  perChange[2:rows,]
}

