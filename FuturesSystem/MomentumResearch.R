# Base Ides
#' The following things can vary in ones description of Momentum
#' 1. What is measured (alpha, excess Return, abs. return)
#' 2. The measurement time frame
#' 3. The measurement of return the momentum predicts
#' 4. The time frame over which momentum predicts returns
#' 
#' To make matters less convoluted and not to overfit to much the followign simplified 
#' rules are used.
#' - Time frames as well as metrics are the same for both variables
#' - excess Return and abs return are used, as those are the only ones harvestable
#' - The signal is lagged one day for tradeability

source("R/dependencies.R")

################################################################################
# DATA #
################################################################################

# Pull Constituent Data (Use ETFs for now)
instruments <-  data.frame(
  "ticker" = c("EXV6.DE", "EXH1.DE", "EXV1.DE", "EXV4.DE", "EXV5.DE",
               "EXH4.DE", "EXV3.DE", "EXV8.DE", "EXH8.DE", "EXH3.DE",
               "EXV2.DE", "EXH7.DE"),
  "names" = c("Basic_Resources", "Oil_Gas", "Banks", "Health_Care", "Automobile",
              "Industrials", "Technology", "Construction", "Utilities", "Food",
              "Telecommunication", "Personal_Goods")
)
data <- instruments %>% 
  pull(ticker) %>% 
  tidyquant::tq_get(., from = "2012-01-01") %>% 
  left_join(., instruments, by = c("symbol"="ticker")) %>%
  na.omit()
df_return <- data %>% 
  group_by(names) %>% 
  mutate(return = RETURN(adjusted)) %>% 
  ungroup(names) %>%          
  select(names, date, return)

# Pull Benchmark 
Benchmark <- tq_get("^STOXX", from = "2000-01-01") %>% 
  select(date, adjusted) %>%
  filter(date %in% data$date) %>% 
  mutate(
    Mkt = RETURN(adjusted, 1)
  ) %>%
  select(-adjusted)

# Decompose
DEC <- decompose_returns(df_return, Benchmark, regression_lag = 20) 

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


################################################################################
# GENERATE SIGNALS #
################################################################################

# absolute Return Momentum (for dollar neutral LS and LO)
absReturn <- c(1, 5, 10, 20, 60, 120) %>% 
  lapply(., function(x){
    
    # generate Signal
    temp_df <- data  %>% 
      group_by(names) %>% 
      mutate(
        Momentu_sig = RETURN(adjusted, x) %>% lag(),
        Momentum_ret = RETURN(adjusted,x) %>% shift(-(x-1))
        ) %>% 
      select(names, date, Momentu_sig, Momentum_ret) %>% 
      ungroup(names)
    
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


# excess Return Momentum (for beta constrained LO and beta neutral LS)
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
              
              res <- prod(1 + alpha + epsilon + ß_Mkt * Mkt) - prod(1 + ß_Mkt * Mkt)
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

# Volatility Signals -----------------------------------------------------------
absVol <- c(5, 10, 20, 60, 120) %>% 
  lapply(., function(x){
    
    # generate Signal
    temp_df <- data  %>% 
      group_by(names) %>% 
      mutate(
        Vol_sig = c(rep(NA,x-1),rollapply(adjusted, width=x,  function(col){sd(RETURN(col,1),na.rm=T)})) %>% lag(),
        Vol_ret = RETURN(adjusted,x) %>% shift(-(x-1))
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
# TEST MOMENTUM SIGNALS #
################################################################################

# Test Rank correlations -------------------------------------------------------
## Total
### Rank Correlation
absReturn %>% 
  select(c(LAG, Momentum_sig_rank, Momentum_ret_rank)) %>% 
  group_by(LAG) %>% 
  summarize(
    "rank_corr" = cor(Momentum_sig_rank, Momentum_ret_rank)
  ) 

excessreturn %>% 
  na.omit() %>% 
  select(c(LAG, Momentum_sig_rank, Momentum_ret_rank)) %>% 
  group_by(LAG) %>% 
  summarize(
    "rank_corr" = cor(Momentum_sig_rank, Momentum_ret_rank)
  ) 

absVol %>% 
  na.omit() %>% 
  select(c(LAG, Vol_sig_rank, Vol_ret_rank)) %>% 
  group_by(LAG) %>% 
  summarize(
    "rank_corr" = cor(Vol_sig_rank, Vol_ret_rank)
  ) 

### LS Correlation
absReturn %>% 
  as.data.frame() %>%
  na.omit() %>% 
  mutate(
    Momentum_sig_portfolio = ifelse(Momentum_sig_rank>6,1,0),
    Momentum_ret_portfolio = ifelse(Momentum_ret_rank>6,1,0),
    Momentum_succes = ifelse(Momentum_sig_portfolio ==Momentum_ret_portfolio, 1,0)
  ) %>% 
  group_by(LAG) %>% 
  summarize(
    "ls_corr" = mean(Momentum_succes)-0.5
  ) 

excessreturn %>% 
  as.data.frame() %>%
  na.omit() %>% 
  mutate(
    Momentum_sig_portfolio = ifelse(Momentum_sig_rank>6,1,0),
    Momentum_ret_portfolio = ifelse(Momentum_ret_rank>6,1,0),
    Momentum_succes = ifelse(Momentum_sig_portfolio ==Momentum_ret_portfolio, 1,0)
  ) %>% 
  group_by(LAG) %>% 
  summarize(
    "ls_corr" = mean(Momentum_succes)-0.5
  ) 

absVol %>% 
  as.data.frame() %>%
  na.omit() %>% 
  mutate(
    Vol_sig_portfolio = ifelse(Vol_sig_rank>6,1,0),
    Vol_ret_portfolio = ifelse(Vol_ret_rank>6,1,0),
    Vol_succes = ifelse(Vol_sig_portfolio == Vol_ret_portfolio, 1,0)
  ) %>% 
  group_by(LAG) %>% 
  summarize(
    "ls_corr" = mean(Vol_succes)-0.5
  ) 

## Sector based
### Rank Correlation
absReturn %>% 
  as.data.frame() %>%
  na.omit() %>% 
  group_by(names, LAG) %>% 
  summarize(
    "rank_corr" = cor(Momentum_sig_rank, Momentum_ret_rank)
  ) %>% 
  pivot_wider(names_from = names, values_from=rank_corr)

excessreturn %>% 
  as.data.frame() %>%
  na.omit() %>% 
  group_by(names, LAG) %>% 
  summarize(
    "rank_corr" = cor(Momentum_sig_rank, Momentum_ret_rank)
  ) %>% 
  pivot_wider(names_from = names, values_from=rank_corr)


### LS Correlation
absReturn %>% 
  as.data.frame() %>%
  na.omit() %>% 
  mutate(
    Momentum_sig_portfolio = ifelse(Momentum_sig_rank>6,1,0),
    Momentum_ret_portfolio = ifelse(Momentum_ret_rank>6,1,0),
    Momentum_succes = ifelse(Momentum_sig_portfolio ==Momentum_ret_portfolio, 1,0)
  ) %>% 
  group_by(names, LAG) %>% 
  summarize(
    "ls_corr" = mean(Momentum_succes)-0.5
  ) %>% 
  pivot_wider(names_from = names, values_from=ls_corr)

excessreturn %>% 
  as.data.frame() %>%
  na.omit() %>% 
  mutate(
    Momentum_sig_portfolio = ifelse(Momentum_sig_rank>6,1,0),
    Momentum_ret_portfolio = ifelse(Momentum_ret_rank>6,1,0),
    Momentum_succes = ifelse(Momentum_sig_portfolio ==Momentum_ret_portfolio, 1,0)
  ) %>% 
  group_by(names, LAG) %>% 
  summarize(
    "ls_corr" = mean(Momentum_succes)-0.5
  ) %>% 
  pivot_wider(names_from = names, values_from=ls_corr)

# Test Return Characteristics --------------------------------------------------
## Total
absReturn %>% 
  na.omit() %>% 
  mutate(
    Momentum_sig_portfolio_lo = ifelse(Momentum_sig_rank>6,1,0) * Momentum_ret,
    Momentum_sig_portfolio_ls = ifelse(Momentum_sig_rank>6,1,-1)* Momentum_ret
  ) %>% 
  group_by(LAG) %>% 
  summarize(
    "mean_benchmark"=mean(Momentum_ret),
    "sd_benchmark"=sd(Momentum_ret),
      
    "mean_lo"= mean(Momentum_sig_portfolio_lo),
    "sd_lo" = sd(Momentum_sig_portfolio_lo),
    
    "mean_ls"= mean(Momentum_sig_portfolio_ls),
    "sd_ls" = sd(Momentum_sig_portfolio_ls)
  ) %>% 
  mutate(
    "mean_benchmark"=(1+mean_benchmark)^(252/LAG)-1,
    "sd_benchmark" = sd_benchmark*sqrt(252/LAG),
    
    "mean_lo"=(1+mean_lo)^(252/LAG)-1,
    "sd_lo" = sd_lo*sqrt(252/LAG),
    
    "mean_ls"=(1+mean_ls)^(252/LAG)-1,
    "sd_ls" = sd_ls*sqrt(252/LAG)
  )

excessreturn %>% 
  na.omit() %>% 
  mutate(
    Momentum_sig_portfolio_lo = ifelse(Momentum_sig_rank>6,1,0) * Momentum_ret,
    Momentum_sig_portfolio_ls = ifelse(Momentum_sig_rank>6,1,-1)* Momentum_ret
  ) %>% 
  group_by(LAG) %>% 
  summarize(
    "mean_benchmark"=mean(Momentum_ret),
    "sd_benchmark"=sd(Momentum_ret),
    
    "mean_lo"= mean(Momentum_sig_portfolio_lo),
    "sd_lo" = sd(Momentum_sig_portfolio_lo),
    
    "mean_ls"= mean(Momentum_sig_portfolio_ls),
    "sd_ls" = sd(Momentum_sig_portfolio_ls)
  ) %>% 
  mutate(
    "mean_benchmark"=(1+mean_benchmark)^(252/LAG)-1,
    "sd_benchmark" = sd_benchmark*sqrt(252/LAG),
    
    "mean_lo"=(1+mean_lo)^(252/LAG)-1,
    "sd_lo" = sd_lo*sqrt(252/LAG),
    
    "mean_ls"=(1+mean_ls)^(252/LAG)-1,
    "sd_ls" = sd_ls*sqrt(252/LAG)
  )


## Sector based
## Total
absReturn %>% 
  na.omit() %>% 
  mutate(
    Momentum_sig_portfolio_lo = ifelse(Momentum_sig_rank>6,1,0) * Momentum_ret,
    Momentum_sig_portfolio_ls = ifelse(Momentum_sig_rank>6,1,-1)* Momentum_ret
  ) %>% 
  group_by(LAG, names) %>% 
  summarize(
    "mean_benchmark"=mean(Momentum_ret),
    "sd_benchmark"=sd(Momentum_ret),
    
    "mean_lo"= mean(Momentum_sig_portfolio_lo),
    "sd_lo" = sd(Momentum_sig_portfolio_lo),
    
    "mean_ls"= mean(Momentum_sig_portfolio_ls),
    "sd_ls" = sd(Momentum_sig_portfolio_ls)
  ) %>% 
  mutate(
    "mean_benchmark"=(1+mean_benchmark)^(252/LAG)-1,
    "sd_benchmark" = sd_benchmark*sqrt(252/LAG),
    
    "mean_lo"=(1+mean_lo)^(252/LAG)-1,
    "sd_lo" = sd_lo*sqrt(252/LAG),
    
    "mean_ls"=(1+mean_ls)^(252/LAG)-1,
    "sd_ls" = sd_ls*sqrt(252/LAG)
  ) %>% 
  mutate(
    excessSharpe_lo = (mean_lo / sd_lo) - (mean_benchmark / sd_benchmark),
    excessSharpe_ls = (mean_ls / sd_ls) - (mean_benchmark / sd_benchmark),
    ) %>% 
  ggplot(.) +
  geom_col(aes(x=names, y=excessSharpe_ls, fill=names)) +
  facet_wrap(~LAG)


excessreturn %>% 
  na.omit() %>% 
  mutate(
    Momentum_sig_portfolio_lo = ifelse(Momentum_sig_rank>6,1,0) * Momentum_ret,
    Momentum_sig_portfolio_ls = ifelse(Momentum_sig_rank>6,1,-1)* Momentum_ret
  ) %>% 
  group_by(LAG, names) %>% 
  summarize(
    "mean_benchmark"=mean(Momentum_ret),
    "sd_benchmark"=sd(Momentum_ret),
    
    "mean_lo"= mean(Momentum_sig_portfolio_lo),
    "sd_lo" = sd(Momentum_sig_portfolio_lo),
    
    "mean_ls"= mean(Momentum_sig_portfolio_ls),
    "sd_ls" = sd(Momentum_sig_portfolio_ls)
  ) %>% 
  mutate(
    "mean_benchmark"=(1+mean_benchmark)^(252/LAG)-1,
    "sd_benchmark" = sd_benchmark*sqrt(252/LAG),
    
    "mean_lo"=(1+mean_lo)^(252/LAG)-1,
    "sd_lo" = sd_lo*sqrt(252/LAG),
    
    "mean_ls"=(1+mean_ls)^(252/LAG)-1,
    "sd_ls" = sd_ls*sqrt(252/LAG)
  ) %>% 
  mutate(
    excessSharpe_lo = (mean_lo / sd_lo) - (mean_benchmark / sd_benchmark),
    excessSharpe_ls = (mean_ls / sd_ls) - (mean_benchmark / sd_benchmark),
  ) %>% 
  ggplot(.) +
  geom_col(aes(x=names, y=excessSharpe_ls, fill=names)) +
  facet_wrap(~LAG)

################################################################################
# BACKTEST MOMENTUM SIGNALS #
################################################################################

# Long ony Backtest ------------------------------------------------------------
BETA=1
MAX_POSITION=0.4
MIN_POSITION = 0.05
DOLLAR=1
LONG_LEG_DOLLAR=1
SHORT_LEG_DOLLAR=0

# Backtest for the absolute return ---------------------------------------------
SIGNAL <- excessreturn %>%
  filter(LAG==5) %>% 
  na.omit() %>% 
  mutate(SIGNAL = ifelse(Momentum_sig_rank>6,1,0)) %>% 
  select(date, names, SIGNAL)

OBJECTIVES <- absReturn %>%
  filter(LAG==5) %>% 
  na.omit() %>% 
  mutate(OBJECTIVE = Momentum_sig_rank) %>% 
  select(date, names, OBJECTIVE)


res <- BacktestEngine_1(RETURNS = df_return, OBJECTIVES = OBJECTIVES, SIGNAL = SIGNAL, BETAS = DEC %>% select(date, names, ß_Mkt),
                 REBALANCE_FREQ = "weekly",
                 BETA = 1,
                 MAX_POSITION = 0.4,
                 MIN_POSITION = 0.05,
                 DOLLAR = 1,
                 LONG_LEG_DOLLAR = 1,
                 SHORT_LEG_DOLLAR = 0)



plot_df <- tester$portfolio %>% 
  left_join(., Benchmark) %>% 
  arrange(date) %>% 
  replace(is.na(.), 0) %>% 
  mutate(Portfolio =cum.ret(portfolio_return),
         Benchmark = cum.ret(Mkt)) 
plot_df %>% 
  ggplot(.) +
  geom_line(aes(x=date, y=Portfolio, color = "Portfolio")) +
  geom_line(aes(x=date, y=Benchmark, color = "Benchmark")) +
  theme_tq()

lm(plot_df$portfolio_return ~ plot_df$Mkt) %>% summary()



# Long Short -------------------------------------------------------------------
SIGNAL_Momentum <- excessreturn %>%
  filter(LAG==20) %>% 
  na.omit() %>% 
  mutate(SIGNAL = ifelse(Momentum_sig_rank>6,1,-1)) %>% 
  select(date, names, SIGNAL)



# SIGNAL <- excessreturn %>%
#   filter(LAG==20) %>% 
#   na.omit() %>% 
#   left_join(., absVol %>% filter(LAG==20) %>% select(date, names, Vol_sig_rank)) %>% 
#   mutate(
#     AVG_RANK = (Momentum_sig_rank - (Vol_sig_rank -13)) / 2,
#     SIGNAL = ifelse(Vol_sig_rank>6,1,-1) 
#   ) %>% 
#   select(date, names, SIGNAL)
  

OBJECTIVES_Momentum <- excessreturn %>%
  filter(LAG==20) %>% 
  na.omit() %>% 
  left_join(., DEC %>% select(date, names, ß_Mkt)) %>% 
  mutate(OBJECTIVE = 1 / ß_Mkt) %>% 
  select(date, names, OBJECTIVE)

OBJECTIVES_Momentum <- absVol %>%
  filter(LAG==20) %>% 
  na.omit() %>% 
  mutate(OBJECTIVE = 1/Vol_sig) %>% 
  select(date, names, OBJECTIVE)


res_Momentum <- BacktestEngine_1(RETURNS = df_return, OBJECTIVES = OBJECTIVES_Momentum,
                                 SIGNAL = SIGNAL_Momentum,
                        BETAS = DEC %>% select(date, names, ß_Mkt),
                        REBALANCE_FREQ = "monthly",
                        BETA = 1,
                        MAX_POSITION = 0.4,
                        MIN_POSITION = 0.05,
                        DOLLAR = 0,
                        LONG_LEG_DOLLAR = 1,
                        SHORT_LEG_DOLLAR = 1)


SIGNAL_Risk <- absVol %>%
  filter(LAG==20) %>% 
  na.omit() %>% 
  mutate(SIGNAL = ifelse(Vol_sig_rank>6,-1,1)) %>% 
  select(date, names, SIGNAL)

OBJECTIVES_Risk <- excessreturn %>%
  filter(LAG==20) %>% 
  na.omit() %>% 
  mutate(OBJECTIVE = Momentu_sig) %>% 
  select(date, names, OBJECTIVE)

res_Risk <- BacktestEngine_1(RETURNS = df_return,
                             OBJECTIVES = OBJECTIVES_Risk,
                             SIGNAL = SIGNAL_Risk,
                             BETAS = DEC %>% select(date, names, ß_Mkt),
                             REBALANCE_FREQ = "monthly",
                             BETA = 1,
                             MAX_POSITION = 0.4,
                             MIN_POSITION = 0.05,
                             DOLLAR = 0,
                             LONG_LEG_DOLLAR = 1,
                             SHORT_LEG_DOLLAR = 1)



plot_df <- res_Momentum$portfolio %>% 
  select(date, Momentum_port = portfolio_return) %>% 
  left_join(., res_Risk$portfolio %>% select(date, Risk_port = portfolio_return)) %>% 
  left_join(., Benchmark) %>% 
  arrange(date) %>% 
  replace(is.na(.), 0) %>% 
  mutate(
    combinedStrategy = 0.5 * (Momentum_port + Risk_port),
    Portfolio_Momentum =cum.ret(Momentum_port),
    Portfolio_Risk = cum.ret(Risk_port),
    Benchmark = cum.ret(Mkt),
    Portfolio_Combination = cum.ret(combinedStrategy)) 

plot_df %>% 
  ggplot(.) +
  geom_line(aes(x=date, y=Portfolio_Momentum, color = "Portfolio_Momentum")) +
  geom_line(aes(x=date, y=Portfolio_Risk, color = "Portfolio_Risk")) +
  geom_line(aes(x=date, y=Portfolio_Combination, color = "Portfolio_Combination")) +
  geom_line(aes(x=date, y=Benchmark, color = "Benchmark")) +
  # geom_line(aes(x=date, y=0.5*(Portfolio + Benchmark), color = "MIX")) +
  theme_tq()

lm(plot_df$Momentum_port ~ plot_df$Mkt) %>% summary()
lm(plot_df$Risk_port ~ plot_df$Mkt) %>% summary()
lm(plot_df$combinedStrategy ~ plot_df$Mkt) %>% summary()

sd(plot_df$Momentum_port, na.rm = T) * sqrt(252)
sd(plot_df$Risk_port, na.rm = T) * sqrt(252)
sd(plot_df$combinedStrategy, na.rm = T) * sqrt(252)
sd(plot_df$Mkt, na.rm = T) * sqrt(252)

tail(plot_df$Portfolio_Momentum,1)^(252/nrow(plot_df))-1
tail(plot_df$Portfolio_Risk,1)^(252/nrow(plot_df))-1
tail(plot_df$Portfolio_Combination,1)^(252/nrow(plot_df))-1
tail(plot_df$Benchmark,1)^(252/nrow(plot_df))-1
   
(tail(plot_df$Portfolio_Momentum,1)^(252/nrow(plot_df))-1)/(sd(plot_df$Momentum_port, na.rm = T) *sqrt(252))
(tail(plot_df$Portfolio_Risk,1)^(252/nrow(plot_df))-1)/(sd(plot_df$Risk_port, na.rm = T) *sqrt(252))
(tail(plot_df$Portfolio_Combination,1)^(252/nrow(plot_df))-1)/(sd(plot_df$combinedStrategy, na.rm = T) *sqrt(252))
(tail(plot_df$Benchmark,1)^(252/nrow(plot_df))-1)/(sd(plot_df$Mkt, na.rm = T) *sqrt(252))

plot_df %>% select(Momentum_port,Risk_port,combinedStrategy, Mkt) %>% cor()


plot_df %>% 
  select(Momentum_port,Risk_port,combinedStrategy, Mkt) %>% 
  xts(., order.by=as.Date(plot_df$date)) %>% 
  maxDrawdown(.)

plot_df %>% 
  select(Momentum_port,Risk_port,combinedStrategy, Mkt) %>% 
  xts(., order.by=as.Date(plot_df$date)) %>% 
  Return.annualized()

plot_df %>% 
  select(Momentum_port,Risk_port,combinedStrategy, Mkt) %>% 
  xts(., order.by=as.Date(plot_df$date)) %>% 
  SharpeRatio.annualized()
