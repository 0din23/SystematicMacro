source("R/dependencies.R")

# Get data
data <- readxl::read_xlsx("C:/0cap_2/MacroMan/Data1.xlsx", sheet = "BacktestData") %>% 
  mutate(date = as.Date(date))

# Get Returns
df_return <- data %>% 
  pivot_longer(cols=colnames(.)[colnames(.)!="date"], names_to = "names",
               values_to = "adjusted") %>% 
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

################################################################################
# SIGNALS #
################################################################################

# Decompose the returns --------------------------------------------------------
# Decompose
DEC <- decompose_returns(df_return, Benchmark, regression_lag = 130) 

# Check Alpha vs. Epsilon
DEC %>% 
  group_by(names) %>% 
  summarize(
    alpha_perc = mean(abs(alpha), na.rm=T) / mean(abs(return), na.rm=T),
    epsilon_perc = mean(abs(epsilon), na.rm=T) / mean(abs(return), na.rm=T),
  )

# Check R^2
reg <- lm(DEC$return ~ DEC$y_hat)
reg %>% summary()


# Calculate Signals and Rankings -----------------------------------------------
## Excess Return
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

################################################################################
# EVALUATE SIGNALS #
################################################################################
SIGNALS <- excessreturn %>% 
  left_join(., absVol %>% select(names, date, LAG, Vol_sig, Vol_sig_rank) 
              , by =c("date", "names", "LAG")) %>% 
  na.omit()

# Analyse Signals and Rankings -------------------------------------------------
SIGNALS %>% 
  group_by(LAG) %>% 
  summarize(
    "Momentum_Corr" = cor(Momentum_sig_rank, Momentum_ret_rank),
    "Vol_Corr" = cor(Vol_sig_rank, Momentum_ret_rank),
    "Group_Overlapp_Momentum" = mean((Momentum_sig_rank>6.5 & Momentum_ret_rank>6.5) |
                                      (Momentum_sig_rank<6.5 & Momentum_ret_rank<6.5)),
    "Group_Overlapp_Volatility" = mean((Vol_sig_rank<6.5 & Momentum_ret_rank>6.5) |
                                       (Vol_sig_rank>6.5 & Momentum_ret_rank<6.5)),
  )






################################################################################
# BACKTEST #
################################################################################
## General Set Up
## General Set Up
holding_period <- "quarterly"
signal_lag_momentum <- 60
signal_lag_risk <- 60
max_position <- 0.4
min_position <- 0.05

## Define Signals
SIGNAL_Momentum <- SIGNALS %>% 
  mutate(SIGNAL = ifelse(Momentum_sig_rank > 6.5,1,0)) %>% 
  filter(LAG == signal_lag_momentum) %>% 
  select(date, names, SIGNAL)

SIGNAL_Risk <- SIGNALS %>% 
  mutate(SIGNAL = ifelse(Vol_sig_rank < 6.5,1,0)) %>% 
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

# Long Only --------------------------------------------------------------------
LO_Momentum <- BacktestEngine_1(RETURNS = df_return, OBJECTIVES = OBJECTIVES_Momentum,
                                 SIGNAL = SIGNAL_Momentum,
                                 BETAS = DEC %>% select(date, names, ß_Mkt),
                                 REBALANCE_FREQ = holding_period,
                                 BETA = 1,
                                 MAX_POSITION = max_position,
                                 MIN_POSITION = min_position,
                                 DOLLAR = 1,
                                 LONG_LEG_DOLLAR = 1,
                                 SHORT_LEG_DOLLAR = 0)

LO_Risk  <- BacktestEngine_1(RETURNS = df_return, OBJECTIVES = OBJECTIVES_Risk,
                             SIGNAL = SIGNAL_Risk,
                             BETAS = DEC %>% select(date, names, ß_Mkt),
                             REBALANCE_FREQ = holding_period,
                             BETA = 1,
                             MAX_POSITION = max_position,
                             MIN_POSITION = min_position,
                             DOLLAR = 1,
                             LONG_LEG_DOLLAR = 1,
                             SHORT_LEG_DOLLAR = 0)

LO_res <- LO_Momentum$portfolio %>% 
  select(date, Momentum_port = portfolio_return) %>% 
  left_join(., LO_Risk$portfolio %>% select(date, Risk_port = portfolio_return)) %>% 
  left_join(., Benchmark) %>%
  arrange(date) %>% 
  replace(is.na(.), 0) %>% 
  mutate(
    combinedStrategy = 0.5 * (Momentum_port + Risk_port),
    Portfolio_Momentum =cum.ret(Momentum_port),
    Portfolio_Risk = cum.ret(Risk_port),
    Benchmark = cum.ret(Mkt),
    Portfolio_Combination = cum.ret(combinedStrategy)) 

# Long Short Beta Neutral ------------------------------------------------------


LS_Momentum <- BacktestEngine_1(RETURNS = df_return, OBJECTIVES = OBJECTIVES_Momentum,
                                SIGNAL = SIGNAL_Momentum,
                                BETAS = DEC %>% select(date, names, ß_Mkt),
                                REBALANCE_FREQ = holding_period,
                                BETA = 0,
                                MAX_POSITION = max_position,
                                MIN_POSITION = min_position,
                                DOLLAR = 0,
                                LONG_LEG_DOLLAR = 1,
                                SHORT_LEG_DOLLAR = 1)

LS_Risk  <- BacktestEngine_1(RETURNS = df_return, OBJECTIVES = OBJECTIVES_Risk,
                             SIGNAL = SIGNAL_Risk,
                             BETAS = DEC %>% select(date, names, ß_Mkt),
                             REBALANCE_FREQ = holding_period,
                             BETA = 0,
                             MAX_POSITION = max_position,
                             MIN_POSITION = min_position,
                             DOLLAR = 0,
                             LONG_LEG_DOLLAR = 1,
                             SHORT_LEG_DOLLAR = 1)

LS_res <- LS_Momentum$portfolio %>% 
  select(date, Momentum_port = portfolio_return) %>% 
  left_join(., LS_Risk$portfolio %>% select(date, Risk_port = portfolio_return)) %>% 
  left_join(., Benchmark) %>%
  arrange(date) %>% 
  replace(is.na(.), 0) %>% 
  mutate(
    combinedStrategy = 0.5 * (Momentum_port + Risk_port),
    Portfolio_Momentum =cum.ret(Momentum_port),
    Portfolio_Risk = cum.ret(Risk_port),
    Benchmark = cum.ret(Mkt),
    Portfolio_Combination = cum.ret(combinedStrategy)) 

# Long Short Net Long ----------------------------------------------------------

LSNL_Momentum <- BacktestEngine_1(RETURNS = df_return, OBJECTIVES = OBJECTIVES_Momentum,
                                SIGNAL = SIGNAL_Momentum,
                                BETAS = DEC %>% select(date, names, ß_Mkt),
                                REBALANCE_FREQ = holding_period,
                                BETA = 0.5,
                                MAX_POSITION = max_position,
                                MIN_POSITION = min_position,
                                DOLLAR = 0.5,
                                LONG_LEG_DOLLAR = 1.5,
                                SHORT_LEG_DOLLAR = 1)

LSNL_Risk  <- BacktestEngine_1(RETURNS = df_return, OBJECTIVES = OBJECTIVES_Risk,
                             SIGNAL = SIGNAL_Risk,
                             BETAS = DEC %>% select(date, names, ß_Mkt),
                             REBALANCE_FREQ = holding_period,
                             BETA = 0.5,
                             MAX_POSITION = max_position,
                             MIN_POSITION = min_position,
                             DOLLAR = 0.5,
                             LONG_LEG_DOLLAR = 1.5,
                             SHORT_LEG_DOLLAR = 1)

LSNL_res <- LSNL_Momentum$portfolio %>% 
  select(date, Momentum_port = portfolio_return) %>% 
  left_join(., LS_Risk$portfolio %>% select(date, Risk_port = portfolio_return)) %>% 
  left_join(., Benchmark) %>%
  arrange(date) %>% 
  replace(is.na(.), 0) %>% 
  mutate(
    combinedStrategy = 0.6 * Momentum_port + 0.4*Risk_port,
    Portfolio_Momentum =cum.ret(Momentum_port),
    Portfolio_Risk = cum.ret(Risk_port),
    Benchmark = cum.ret(Mkt),
    Portfolio_Combination = cum.ret(combinedStrategy)) 


################################################################################
# EVALUATE BACKTEST #
################################################################################



LSNL_res %>% 
  select(Momentum_port,Risk_port,combinedStrategy, Mkt) %>% 
  xts(., order.by=as.Date(LSNL_res$date)) %>% 
  maxDrawdown(.)

LSNL_res %>% 
  select(Momentum_port,Risk_port,combinedStrategy, Mkt) %>% 
  xts(., order.by=as.Date(LSNL_res$date)) %>% 
  Return.annualized()

LS_res %>% 
  select(Momentum_port,Risk_port,combinedStrategy, Mkt) %>% 
  xts(., order.by=as.Date(LSNL_res$date)) %>% 
  SharpeRatio.annualized()


plot <- LS_res %>% 
  ggplot(.) +
  geom_line(aes(x=date, y=Portfolio_Momentum, color = "Portfolio_Momentum")) +
  geom_line(aes(x=date, y=Portfolio_Risk, color = "Portfolio_Risk")) +
  geom_line(aes(x=date, y=Portfolio_Combination, color = "Portfolio_Combination")) +
  geom_line(aes(x=date, y=Benchmark, color = "Benchmark")) +
  # geom_line(aes(x=date, y=0.5*(Portfolio + Benchmark), color = "MIX")) +
  theme_tq()

plot %>% ggplotly()
