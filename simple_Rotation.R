# Get Sources ##################################################################
source("R/dependencies.R")

# Get Data #####################################################################
instruments <- data.frame(
  "ticker" = c("EXV6.DE", "EXH1.DE", "EXV1.DE", "EXV4.DE", "EXV5.DE",
               "EXH4.DE", "EXV3.DE", "EXV8.DE", "EXH8.DE", "EXH3.DE",
               "EXV2.DE", "EXH7.DE", "EXSA.DE"),
  "names" = c("Basic_Resources", "Oil_Gas", "Banks", "Health_Care", "Automobile",
              "Industrials", "Technology", "Construction", "Utilities", "Food",
              "Telecommunication", "Personal_Goods", "STOXX_600")
)
data <- tidyquant::tq_get(instruments$ticker, from = "2012-01-01") %>%
  left_join(., instruments, by = c("symbol"="ticker")) %>%
  na.omit() %>%
  group_by(names) %>%
  mutate(return=RETURN(adjusted)) %>%
  ungroup(names) %>%
  na.omit()

data <- data %>%
  filter(names != "STOXX_600") %>%
  left_join(.,data %>%
              filter(names == "STOXX_600") %>%
              select(date, benchmark=return),
            by="date") %>%
  
  
  # Ideas ########################################################################

# Trend
## Residual
## Absolute

# Momentum
## Residual
## Absolute
## Sharpe

# Low Risk Cross Section
## Beta
## Volatility


# Weighting
## Beta Neutral
## Long only

## Dollar Neutral
## Beta based on time series Momentum of STOXX_600

# Todo
## Generate Signals
## Build Backtester


# Trend Signals ################################################################
TrendSignals <- data %>%
  group_by(names) %>%
  mutate(
    SMA200_LO = ifelse(adjusted >= SMA(adjusted, 200),1,0) %>% lag(),
    SMA100_LO = ifelse(adjusted >= SMA(adjusted, 100),1,0) %>% lag(),
    SMA50_LO = ifelse(adjusted >= SMA(adjusted, 50),1,0) %>% lag(),
    SMA20_LO = ifelse(adjusted >= SMA(adjusted, 20),1,0) %>% lag(),
    SMA5_LO = ifelse(adjusted >= SMA(adjusted, 5),1,0) %>% lag(),
    
    SMA200_LS = ifelse(adjusted >= SMA(adjusted, 200),1,-1) %>% lag(),
    SMA100_LS = ifelse(adjusted >= SMA(adjusted, 100),1,-1) %>% lag(),
    SMA50_LS = ifelse(adjusted >= SMA(adjusted, 50),1,-1) %>% lag(),
    SMA20_LS = ifelse(adjusted >= SMA(adjusted, 20),1,-1) %>% lag(),
    SMA5_LS = ifelse(adjusted >= SMA(adjusted, 5),1,-1) %>% lag(),
    
    CROSS_100_200_LO = ifelse(SMA(adjusted, 100) >= SMA(adjusted, 200),1,0) %>% lag(),
    CROSS_50_100_LO = ifelse(SMA(adjusted, 50) >= SMA(adjusted, 100),1,0) %>% lag(),
    CROSS_20_50_LO = ifelse(SMA(adjusted, 20) >= SMA(adjusted, 50),1,0) %>% lag(),
    CROSS_5_20_LO = ifelse(SMA(adjusted, 5) >= SMA(adjusted, 20),1,0) %>% lag(),
    
    CROSS_100_200_LS = ifelse(SMA(adjusted, 100) >= SMA(adjusted, 200),1,-1) %>% lag(),
    CROSS_50_100_LS = ifelse(SMA(adjusted, 50) >= SMA(adjusted, 100),1,-1) %>% lag(),
    CROSS_20_50_LS = ifelse(SMA(adjusted, 20) >= SMA(adjusted, 50),1,-1) %>% lag(),
    CROSS_5_20_LS = ifelse(SMA(adjusted, 5) >= SMA(adjusted, 20),1,-1) %>% lag(),
  )






















