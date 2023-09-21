getPortfolio <- function(tws, INSTRUMENTS){
  
  
  account <- reqAccountUpdates(tws)
  
  portfolio <- account[[2]] %>% 
    lapply(., function(x){
      if(paste0(x$contract$symbol, ".DE") %in% INSTRUMENTS$ticker){
        res <- data.frame(
          "ticker"=paste0(x$contract$symbol, ".DE"),
          "amount"=x$portfolioValue$position,
          "price"=x$portfolioValue$marketPrice,
          "cost"=x$portfolioValue$averageCost
        )
      } else{
        res <- data.frame(
          "ticker"=as.character(NA),
          "amount"=NA,
          "price"=NA,
          "cost"=NA
        )
      }
      return(res)
    }) %>%
    rbindlist() %>%
    na.omit()
  
  return(portfolio)
}



executeTrades <- function(newPort, tws, TRANSMIT = F){
  
  newPort <- newPort %>% 
    select(ticker, trade_delta, price) %>% 
    mutate(ticker = ticker %>% gsub(pattern = ".DE", replacement = ""))
  
  ## Orders
  for(k in 1:nrow(newPort)){
    
    contractInfo <- reqMatchingSymbols(newPort$ticker[k], twsconn = tws)
    sec <- twsSTK(symbol = newPort$ticker[k], exch = contractInfo$contract.primaryExchange, primary = "ETF",
                  conId = contractInfo$contract.conId, currency = contractInfo$contract.currency,
                  local = newPort$ticker[k], right = "0", strike = "0", include_expired = "")
    orderId <- as.numeric(reqIds(tws))
    
    if(newPort$trade_delta[k] > 0){
      
      ### Construct Order
      buyOrder <- twsOrder(orderId = orderId, 
                           action = "BUY",
                           totalQuantity = newPort$trade_delta[k],
                           orderType = "LMT",
                           lmtPrice = newPort$price[k],
                           transmit = TRANSMIT)
      
      ### Place Order
      print_timed("Instrument: ", newPort$ticker[k], " - ", "Side: ", "BUY", " - ", "OrderID: ", orderId)
      placeOrder(tws, sec, buyOrder)
      
    } else if(newPort$trade_delta[k] < 0){
      
      ### Construct Order
      sellOrder <- twsOrder(orderId = orderId, 
                            action = "SELL",
                            totalQuantity = abs(newPort$trade_delta[k]),
                            orderType = "LMT",
                            lmtPrice = newPort$price[k],
                            transmit = TRANSMIT)
      
      ### Place Order
      print_timed("Instrument: ", newPort$ticker[k], " - ", "Side: ", "SELL", " - ", "OrderID: ", orderId)
      placeOrder(twsconn = tws, Contract = sec, Order = sellOrder)
    }
  }
}



IB_pullQuote <- function(tws, ticker){
  
}