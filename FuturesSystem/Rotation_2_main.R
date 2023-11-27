################################################################################
# Pull Dependencies and Connect #
################################################################################
# get sources
source("R/dependencies.R")

# connect to DB
DB_FILE = "C:/0cap_2/MacroMan/SystematicMacro/data/EU_SR_DATA.db"
conn <- RSQLite::dbConnect(RSQLite::SQLite(), DB_FILE)

# IB
tws <- twsConnect() 

################################################################################
# Pull Portfolio Data #
################################################################################
# Instruments
INSTRUMENTS <-  data.frame(
  "ticker" = c("EXV6.DE", "EXH1.DE", "EXV1.DE", "EXV4.DE", "EXV5.DE",
               "EXH4.DE", "EXV3.DE", "EXV8.DE", "EXH8.DE", "EXH3.DE",
               "EXV2.DE", "EXH7.DE"),
  "names" = c("Basic_Resources", "Oil_Gas",
              "Banks", "Health_Care", "Automobile",
              "Industrials", "Technology", "Construction", "Utilities", "Food",
              "Telecommunication", "Personal_Goods")
)

data <- INSTRUMENTS %>% 
  pull(ticker) %>% 
  tidyquant::tq_get(., from = "2012-01-01", to =Sys.Date()) %>% 
  left_join(., INSTRUMENTS, by = c("symbol"="ticker")) %>%
  na.omit()

# Get Returns
df_return <- data %>% 
  group_by(names) %>% 
  mutate(return = RETURN(adjusted)) %>% 
  ungroup(names) %>%          
  select(date, names, return) %>% 
  arrange(date, names)

# Pull Benchmark 
Benchmark <- tq_get("^STOXX", from = "2000-01-01") %>% 
  select(date, adjusted) %>%
  filter(date %in% data$date) %>% 
  mutate(
    Mkt = RETURN(adjusted, 1)
  ) %>%
  select(-adjusted)

# Analyse Portfolio
current_portfolio <- INSTRUMENTS %>% 
  left_join(., getPortfolio(tws, INSTRUMENTS)) %>% 
  mutate(
    date = Sys.Date() %>% as.character()
  )
current_portfolio[is.na(current_portfolio)] <- 0

# Pull Current Prices
# Vorläufig mal last price of last day

# INSTRUMENTS$price <- getQuote(INSTRUMENTS$ticker)$Last

INSTRUMENTS <- INSTRUMENTS %>% left_join(.,data %>%
                                           filter(date==max(date)) %>%
                                           select(names, adjusted)
)
colnames(INSTRUMENTS)[3] <- "price"
INSTRUMENTS <- INSTRUMENTS %>% 
  left_join(., current_portfolio %>% select(-names, - price), by = "ticker")

RSQLite::dbWriteTable(conn, "PORTFOLIO_STAMPS", current_portfolio,append = TRUE, overwrite = FALSE)

################################################################################
# Generate New Portfolio #
################################################################################

# Generate Signals and Rankings
excessreturn <- c(5, 10, 20, 60, 120) %>% 
  lapply(., function(x){
    
    # generate Signal
    temp_df <- decompose_returns(df_return, Benchmark, regression_lag = x)
    temp_df <- temp_df %>% 
      pull(names) %>% 
      unique() %>% 
      lapply(., function(n){
        
        tmp <- temp_df %>% 
          filter(names==n) %>% 
          mutate(
            "excessReturn"= c(rep(NA,x-1), rollapply(., width=x,by.column = FALSE, FUN=function(e){
              # browser()
              alpha <- as.numeric(rep(tail(e[,"alpha"],1),x))
              epsilon = as.numeric(e[,"epsilon"])
              Mkt = as.numeric(e[,"Mkt"])
              ß_Mkt = as.numeric(e[,"ß_Mkt"])
              eret <- as.numeric(e[,"return"])
              
              # res <- prod(1 + alpha + epsilon + ß_Mkt * Mkt) - prod(1 + ß_Mkt * Mkt)
              res <- prod(1 + eret) - prod(1 + ß_Mkt * Mkt)
              return(res)
            }))
          ) %>% 
          mutate(
            Momentu_sig = excessReturn %>% lag(),
            Momentum_ret = excessReturn %>% shift(-(x-1))
          ) %>% 
          select(names, date, Momentu_sig, Momentum_ret)
        return(tmp)
      }) %>% 
      rbindlist()
    
    ranking <- temp_df %>% 
      select(names, date, Momentu_sig) %>% 
      pivot_wider(names_from = names, values_from = Momentu_sig) %>% 
      .[,-1] %>% 
      apply(.,1,rank) %>% 
      t() %>% 
      data.frame(
        "date"=temp_df$date %>% unique(),
        .
      ) %>% 
      pivot_longer(., cols = colnames(.)[colnames(.)!="date"],
                   names_to = "names", values_to = "Momentum_sig_rank")
    
    result <-  temp_df %>% 
      select(names, date, Momentum_ret) %>% 
      pivot_wider(names_from = names, values_from = Momentum_ret) %>% 
      .[,-1] %>% 
      apply(.,1,rank) %>% 
      t() %>% 
      data.frame(
        "date"=temp_df$date %>% unique(),
        .
      ) %>% 
      pivot_longer(., cols = colnames(.)[colnames(.)!="date"],
                   names_to = "names", values_to = "Momentum_ret_rank")
    
    res <- temp_df %>% 
      left_join(., ranking) %>% 
      left_join(., result) %>% 
      mutate("LAG"=x)
    
    return(res)
  }) %>% 
  rbindlist()

## Volatlity
absVol <- c(5, 10, 20, 60, 120) %>% 
  lapply(., function(x){
    
    # generate Signal
    temp_df <- df_return  %>% 
      na.omit() %>% 
      group_by(names) %>% 
      mutate(
        Vol_sig = c(rep(NA,x-1),rollapply(return, width=x,  function(col){sd(return,na.rm=T)})) %>% lag(),
        Vol_ret = RETURN(cum.ret(return),x) %>% shift(-(x-1))
      ) %>% 
      select(names, date, Vol_sig, Vol_ret) %>% 
      ungroup(names)
    
    ranking <- temp_df %>% 
      select(names, date, Vol_sig) %>% 
      pivot_wider(names_from = names, values_from = Vol_sig) %>% 
      .[,-1] %>% 
      apply(.,1,rank) %>% 
      t() %>% 
      data.frame(
        "date"=temp_df$date %>% unique(),
        .
      ) %>% 
      pivot_longer(., cols = colnames(.)[colnames(.)!="date"],
                   names_to = "names", values_to = "Vol_sig_rank")
    
    result <-  temp_df %>% 
      select(names, date, Vol_ret) %>% 
      pivot_wider(names_from = names, values_from = Vol_ret) %>% 
      .[,-1] %>% 
      apply(.,1,rank) %>% 
      t() %>% 
      data.frame(
        "date"=temp_df$date %>% unique(),
        .
      ) %>% 
      pivot_longer(., cols = colnames(.)[colnames(.)!="date"],
                   names_to = "names", values_to = "Vol_ret_rank")
    
    res <- temp_df %>% 
      left_join(., ranking) %>% 
      left_join(., result) %>% 
      mutate("LAG"=x)
    
    return(res)
  }) %>% 
  rbindlist()

SIGNALS <- excessreturn %>% 
  left_join(., absVol %>% select(names, date, LAG, Vol_sig, Vol_sig_rank) 
            , by =c("date", "names", "LAG")) %>%
  select(names, date, Momentu_sig , Momentum_sig_rank, Vol_sig, Vol_sig_rank ,LAG) %>% 
  na.omit()

# Inputs for Signals -----------------------------------------------------------
## General Set Up
holding_period <- "weekly"
signal_lag_momentum <- 5
signal_lag_risk <- 20
max_position <- 0.4
min_position <- 0.05


## Get Momentum Signal
SIGNAL_Momentum <- SIGNALS %>% 
  mutate(SIGNAL = ifelse(Momentum_sig_rank > 6.5,1,-1)) %>% 
  filter(LAG == signal_lag_momentum) %>% 
  select(date, names, SIGNAL)

SIGNAL_Risk <- SIGNALS %>% 
  mutate(SIGNAL = ifelse(Vol_sig_rank < 6.5,1,-1)) %>% 
  filter(LAG == signal_lag_risk) %>% 
  select(date, names, SIGNAL)

## Define Objectives
OBJECTIVES_Momentum <- SIGNALS %>%
  filter(LAG==signal_lag_risk) %>% 
  na.omit() %>% 
  mutate(OBJECTIVE = 1 / Vol_sig) %>% 
  select(date, names, OBJECTIVE)

OBJECTIVES_Risk <- SIGNALS %>%
  filter(LAG==signal_lag_momentum) %>% 
  na.omit() %>% 
  mutate(OBJECTIVE = Momentu_sig) %>% 
  select(date, names, OBJECTIVE)

## Decompose
DEC <- decompose_returns(df_return, Benchmark, regression_lag = 60) 



# Optimize Portfolio
last_date <- df_return$date %>% max()

objectives_momentum <- OBJECTIVES_Momentum %>% filter(date==max(date)) %>% arrange(names) %>%  pull(OBJECTIVE) %>% as.numeric()
objectives_risk <- OBJECTIVES_Risk %>% filter(date==max(date)) %>% arrange(names) %>%  pull(OBJECTIVE) %>% as.numeric()

exposure_momentum <-  SIGNAL_Momentum %>% filter(date==max(date)) %>% arrange(names) %>%  pull(SIGNAL) %>% as.numeric()
exposure_risk <-  SIGNAL_Risk %>% filter(date==max(date)) %>% arrange(names) %>%  pull(SIGNAL) %>% as.numeric()
beta <- DEC %>% filter(date==max(date)) %>% arrange(names) %>% pull(ß_Mkt) %>% as.numeric()

weights_momentum <- PortfolioOptimizer(objectives=objectives_momentum,
                              exposure=exposure_momentum,
                              beta=beta,
                              BETA=0.5,
                              MAX_POSITION=0.4,
                              MIN_POSITION=0.05,
                              DOLLAR=0.5,
                              LONG_LEG_DOLLAR=1.5,
                              SHORT_LEG_DOLLAR=1,
                              VERBOSE=TRUE)

weights_risk <- PortfolioOptimizer(objectives=objectives_risk,
                                       exposure=exposure_risk,
                                       beta=beta,
                                       BETA=0.5,
                                       MAX_POSITION=0.4,
                                       MIN_POSITION=0.05,
                                       DOLLAR=0.5,
                                       LONG_LEG_DOLLAR=1.5,
                                       SHORT_LEG_DOLLAR=1,
                                       VERBOSE=TRUE)

weights_final <- (weights_momentum + weights_risk)/2

# Discretize Portfolio
INSTRUMENTS <- INSTRUMENTS %>% 
  arrange(names) %>% 
  mutate(optPort = weights_final)


p_val_long <- sum(INSTRUMENTS$price * ifelse(INSTRUMENTS$amount>0,INSTRUMENTS$amount,0))
p_val_short <- sum(INSTRUMENTS$price * ifelse(INSTRUMENTS$amount<0,-INSTRUMENTS$amount,0))

p_val <- sum(INSTRUMENTS$price * INSTRUMENTS$amount)*2
INSTRUMENTS$curr_port <- (INSTRUMENTS$price*INSTRUMENTS$amount)/ p_val
INSTRUMENTS$port_delta <- INSTRUMENTS$optPort- INSTRUMENTS$curr_port
INSTRUMENTS$port_delta_round <- ifelse(abs(INSTRUMENTS$port_delta) < 0.05, 0, INSTRUMENTS$port_delta) 
INSTRUMENTS$port_delta_round <- INSTRUMENTS$port_delta_round - (INSTRUMENTS$port_delta_round * sum(INSTRUMENTS$port_delta_round) / sum(abs(INSTRUMENTS$port_delta_round)))
INSTRUMENTS$optPort_new <- INSTRUMENTS$curr_port + INSTRUMENTS$port_delta_round 

INSTRUMENTS$EUR <- p_val * INSTRUMENTS$optPort_new
INSTRUMENTS$SHARES <- round(INSTRUMENTS$EUR / INSTRUMENTS$price)
INSTRUMENTS$trade_delta <- INSTRUMENTS$SHARES - INSTRUMENTS$amount

################################################################################
# Trade #
################################################################################
new_portfolio <- INSTRUMENTS %>%
  select(ticker, trade_delta, price)%>%
  mutate(price = round(price, 1))
executeTrades(newPort = new_portfolio, tws = tws, TRANSMIT = F)


################################################################################
# Save Data #
################################################################################

# Update Agg Port Table
Port_db <- INSTRUMENTS %>% mutate(date = as.character(Sys.Date()))
RSQLite::dbWriteTable(conn, "PORTFOLIO", Port_db,append = TRUE, overwrite = FALSE)
# PLATZHALTER

################################################################################
# Wrap Up #
################################################################################

# To Do
## Rebalancing!!! Das kann nicht jedes mal so fuckign teuer sein.

## Reporting
## Improvement process




# Data Structure Notes
#' Rebalance Table
#' Portfolio Table for Subportfolio 1-3
#' Aggregate Portfolio Table
#' 
#' Also needs a daily report script to check for rebalancing need, portfolio performance and subportfolio performance
