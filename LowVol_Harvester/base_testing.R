source("R/dependencies.R")

################################################################################
# PREPARE DATA # 
################################################################################
meta <- data.frame("ticker"=c("IWN", "IWO", "^TNX", "^IRX"),
                   "name"=c("value", "growth", "ten_year", "three_month")
)

# Load data
LAG <- 60 # quarter of a year
data <- tq_get(meta$ticker, from="1900-01-01")

# Split Rates
rates_data <- data %>% 
  filter(symbol %in% c("^TNX", "^IRX")) %>% 
  select(date, symbol, adjusted) %>% 
  pivot_wider(., names_from = symbol, values_from = adjusted)

colnames(rates_data) <- c("date", "long_rate", "short_rate")

# load dividends
div_df <- getDividends("IWN", from = "1900-01-01") %>% 
  as.data.frame() %>% 
  mutate(date = row.names(.)) %>% 
  select(date, IWN=IWN.div) %>% 
  mutate(IWO = NA) %>% 
  rbind(.,
        getDividends("IWO", from = "1900-01-01") %>% 
          as.data.frame() %>% 
          mutate(date = row.names(.)) %>%
          mutate(IWN = NA) %>% 
          select(date,IWN, IWO=IWO.div)
        ) %>% 
  mutate(date = as.Date(date)) %>% 
  pivot_longer(cols = c("IWO", "IWN"), names_to = "symbol", values_to = "div") %>% 
  arrange(symbol) %>% 
  group_by(symbol) %>% 
  na.omit() %>% 
  mutate(div_year = SMA(div,4)*4) %>% 
  ungroup(symbol)

# combine
DATA <- data %>% 
  filter(symbol %in% c("IWN", "IWO")) %>% 
  select(date, symbol, price=adjusted) %>% 
  left_join(.,rates_data, by="date") %>% 
  left_join(., div_df) %>% 
  mutate(div_date = ifelse(!is.na(div),1,0))

# calculate important data
DATA <- DATA %>% 
  mutate(DIV_YIELD = (div_year / price)*100) %>% 
  fill(DIV_YIELD, div, div_year, short_rate, long_rate) %>% 
  mutate(DIV_YIELD_REAL = DIV_YIELD - long_rate) %>% 
  group_by(symbol) %>% 
  mutate(
    return = RETURN(price),
    log_return = log(1+return)
  ) %>% 
  filter(!is.na(return)) %>% 
  mutate(
    hist_vol = runSD(log_return, LAG)*sqrt(252),
    future_vol = hist_vol %>% shift(-LAG),
    future_return = RETURN(price, LAG) %>% shift(-LAG),
    future_log_return = log(1+future_return)
  ) %>% 
  mutate(
    future_DIV = DIV_YIELD %>% shift(-LAG),
  ) %>% 
  ungroup(symbol) %>%
  filter(!is.na(DIV_YIELD)) %>% 
  filter(!is.na(hist_vol)) %>% 
  filter(!is.na(future_DIV)) %>% 
  filter(!is.na(future_vol)) %>% 
  filter(!is.na(future_return))
  


################################################################################
# FORMULATE THESIS #
################################################################################

################################################################################
# PROCESS OPTION DATA #
################################################################################

# Value options
# Call -------------------------------------------------------------------------
## hist value call
DATA$hist_val_call <- DATA %>% 
  apply(.,1,FUN = function(x){
    #print(x)
    res <- RQuantLib::AmericanOption(type = "call",
                              underlying = as.numeric(x["price"]),
                              strike = as.numeric(x["price"]),
                              dividendYield = as.numeric(x["DIV_YIELD"])/100,
                              riskFreeRate = as.numeric(x["short_rate"])/100,
                              maturity = as.numeric(LAG/252),
                              volatility = as.numeric(x["hist_vol"])
                              )
    return(res[1])
  }) %>% unlist()

## optimal value call
DATA$optimal_val_call <- DATA %>% 
  apply(.,1,FUN = function(x){
    #print(x)
    res <- RQuantLib::AmericanOption(type = "call",
                                     underlying = as.numeric(x["price"]),
                                     strike = as.numeric(x["price"]),
                                     dividendYield = as.numeric(x["future_DIV"])/100,
                                     riskFreeRate = as.numeric(x["short_rate"])/100,
                                     maturity = as.numeric(LAG/252),
                                     volatility = as.numeric(x["future_vol"])
    )
    return(res[1])
  }) %>% unlist()

## optimal drift call
DATA$drift_val_call <- DATA %>% 
  apply(.,1,FUN = function(x){
    res <- RQuantLib::AmericanOption(type = "call",
                                     underlying = as.numeric(x["price"]),
                                     strike = as.numeric(x["price"]),
                                     dividendYield = as.numeric(x["future_DIV"])/100,
                                     riskFreeRate = as.numeric(x["future_log_return"]),
                                     maturity = as.numeric(LAG/252),
                                     volatility = as.numeric(x["future_vol"])
    )
    return(res[1])
  }) %>% unlist()

# Put --------------------------------------------------------------------------
## hist value call
DATA$hist_val_put <- DATA %>% 
  apply(.,1,FUN = function(x){
    print(x)
    res <- RQuantLib::AmericanOption(type = "put",
                                     underlying = as.numeric(x["price"]),
                                     strike = as.numeric(x["price"]),
                                     dividendYield = as.numeric(x["DIV_YIELD"])/100,
                                     riskFreeRate = as.numeric(x["short_rate"])/100,
                                     maturity = as.numeric(LAG/252),
                                     volatility = as.numeric(x["hist_vol"])
    )
    return(res[1])
  }) %>% unlist()

## optimal value put
DATA$optimal_val_put <- DATA %>% 
  apply(.,1,FUN = function(x){
    res <- RQuantLib::AmericanOption(type = "put",
                                     underlying = as.numeric(x["price"]),
                                     strike = as.numeric(x["price"]),
                                     dividendYield = as.numeric(x["future_DIV"])/100,
                                     riskFreeRate = as.numeric(x["short_rate"])/100,
                                     maturity = as.numeric(LAG/252),
                                     volatility = as.numeric(x["future_vol"])
    )
    return(res[1])
  }) %>% unlist()

## optimal drift put
DATA$drift_val_put <- DATA %>% 
  apply(.,1,FUN = function(x){
    res <- RQuantLib::AmericanOption(type = "put",
                                     underlying = as.numeric(x["price"]),
                                     strike = as.numeric(x["price"]),
                                     dividendYield = as.numeric(x["future_DIV"])/100,
                                     riskFreeRate = as.numeric(x["future_log_return"]),
                                     maturity = as.numeric(LAG/252),
                                     volatility = as.numeric(x["future_vol"])
    )
    return(res[1])
  }) %>% unlist()

















DATA %>% 
  ggplot(.) +
  geom_line(aes(x=date, y=optimal_val_call, color =symbol))

