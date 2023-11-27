source("R/dependencies.R")
quandl_api_key("YjcYtntQykd9hXCpk6CZ")

################################################################################
# INPUT #
################################################################################
ticker <- "^GDAXI"
data <-  tq_get(ticker, from = "1900-01-01") %>% 
  mutate(return_1B = RETURN(adjusted))

################################################################################
# CONSTRUCT SIGNALS #
################################################################################
# The signals represent either exuberance or some kind of trend measure
data <- data %>%
  na.omit() %>% 
  
  # Trend Type Measures
  mutate(
    
    return_5B = RETURN(adjusted, 5),
    return_20B = RETURN(adjusted, 20),
    return_60B = RETURN(adjusted, 60),
    return_120B = RETURN(adjusted, 120),
    return_260B = RETURN(adjusted, 260),
    
    
    SMA_5B = (adjusted - SMA(adjusted, 5))/adjusted,
    SMA_20B = (adjusted - SMA(adjusted, 20))/adjusted,
    SMA_60B = (adjusted - SMA(adjusted, 60))/adjusted,
    SMA_120B = (adjusted - SMA(adjusted, 120))/adjusted,
    SMA_260B = (adjusted - SMA(adjusted, 260))/adjusted,
    
    
    SMA_5B_derivate = RETURN(SMA(adjusted, 5)),
    SMA_20B_derivate = RETURN(SMA(adjusted, 20)),
    SMA_60B_derivate = RETURN(SMA(adjusted, 60)),
    SMA_120B_derivate = RETURN(SMA(adjusted, 120)),
    SMA_260B_derivate = RETURN(SMA(adjusted, 260)),
    
    SMA_5B_20B_cross = (SMA(adjusted, 5) - SMA(adjusted, 20))/SMA(adjusted, 20),
    SMA_20B_60B_cross = (SMA(adjusted, 20) - SMA(adjusted, 60))/SMA(adjusted, 60),
    SMA_60B_120B_cross = (SMA(adjusted, 60) - SMA(adjusted, 120))/SMA(adjusted, 120),
    SMA_120B_260B_cross = (SMA(adjusted, 120) - SMA(adjusted, 260))/SMA(adjusted, 260)
    
  ) %>% 
  
  # Measure for getting remunerated for Risk
  mutate(
    
    SD_5B = TTR::runSD(return_1B, 5),
    SD_20B = TTR::runSD(return_1B, 20),
    SD_60B = TTR::runSD(return_1B, 60),
    SD_120B = TTR::runSD(return_1B, 120),
    SD_260B = TTR::runSD(return_1B, 260),
    
    range =(high - low) / close,
    ATR_5B = SMA(range, 5),
    ATR_20B = SMA(range, 20),
    ATR_60B = SMA(range, 60),
    ATR_120B = SMA(range, 120),
    ATR_260B = SMA(range, 260)
  ) %>% 
  
  # Measures that try to capture trends running out
  mutate(
    
  )

################################################################################
# FILTER FOR CAUSAL GROUPING #
################################################################################
# get signals
signals <- data %>% colnames()
signals <- signals[!(signals %in% c("symbol", "date", ""))]

#construct label
data <- data %>% mutate(label = RETURN(adjusted, 20) %>% shift(-20))

tests <- lapply(signals, function(s){
    res <- TestCausality(data=data, label="label", signal=s, no_groups = 2)
  }) %>% rbindlist()


# which are good 
good_signals <- 4 == tests %>% rowSums()
good_signals <- ifelse(is.na(good_signals),FALSE, good_signals)
good_signals <- signals[good_signals]

################################################################################
# CONSTRUCT SIGNALS #
################################################################################

# Construct causal signals 
SIGNALS <- data[,c("date", good_signals)]

SIGNALS
th <- CausalFeature(data = data %>% head(3000), label = "label", signal = good_signals[1], th = TRUE)

test <- tail(data, 4000)
test$SIGNAL <- ifelse(test[,good_signals[1]]>th,1,0)

mean(test$SIGNAL * test$label, na.rm=T)
mean(test$label, na.rm=T)

# purge redundant signals



################################################################################
# FIT MODEL #
################################################################################


################################################################################
# EVALUATE MODEL #
################################################################################






