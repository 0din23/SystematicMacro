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
# GENERATE MOMENTUM SIGNALS #
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
  select(c(LAG, Momentum_sig_rank, Momentum_ret_rank)) %>% 
  group_by(LAG) %>% 
  summarize(
    "rank_corr" = cor(Momentum_sig_rank, Momentum_ret_rank)
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
## Equal Weighting
absReturn








# Develope Portfolio Construction Function




  