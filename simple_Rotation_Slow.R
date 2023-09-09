source("R/dependencies.R")
################################################################################
# DATA #
################################################################################
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

################################################################################
# DECOMPOSE RETURNS #
################################################################################
## Input and Setup
df_return <- data %>% 
  group_by(names) %>% 
  mutate(return = RETURN(adjusted)) %>% 
  ungroup(names) %>%          
  select(names, date, return)

X <- tq_get("^STOXX", from = "2000-01-01") %>% 
  select(date, adjusted) %>%
  filter(date %in% df_return$date) %>% 
  mutate(
    Mkt = RETURN(adjusted, 1)
  ) %>%
  select(-adjusted)

DEC <- decompose_returns(df_return, X, regression_lag = 30) 
# Check Alpha vs. Epsilon
DEC %>% 
  group_by(symbol) %>% 
  summarize(
    alpha_perc = mean(abs(alpha), na.rm=T) / mean(abs(return), na.rm=T),
    epsilon_perc = mean(abs(epsilon), na.rm=T) / mean(abs(return), na.rm=T),
  )


# Check R^2
reg <- lm(DEC$return ~ DEC$y_hat)
reg %>% summary()

################################################################################
# CALCULATE SIGNALS #
################################################################################
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

# Absolute Trend ###############################################################
AbsoluteTrendSignals <- data %>%
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
  ) %>% 
  select(-c(symbol, open, high, low, close, adjusted, volume)) %>% 
  ungroup(names)

AbsoluteTrendSignals <- data %>%
  group_by(names) %>%
  mutate(
    SMA200_LO = ifelse(adjusted >= SMA(adjusted, 200),1,0) %>% lag(),
    SMA100_LO = ifelse(adjusted >= SMA(adjusted, 100),1,0) %>% lag(),
    SMA50_LO = ifelse(adjusted >= SMA(adjusted, 50),1,0) %>% lag(),
    SMA20_LO = ifelse(adjusted >= SMA(adjusted, 20),1,0) %>% lag(),
    SMA5_LO = ifelse(adjusted >= SMA(adjusted, 5),1,0) %>% lag(),

    
    CROSS_100_200_LO = ifelse(SMA(adjusted, 100) >= SMA(adjusted, 200),1,0) %>% lag(),
    CROSS_50_100_LO = ifelse(SMA(adjusted, 50) >= SMA(adjusted, 100),1,0) %>% lag(),
    CROSS_20_50_LO = ifelse(SMA(adjusted, 20) >= SMA(adjusted, 50),1,0) %>% lag(),
    CROSS_5_20_LO = ifelse(SMA(adjusted, 5) >= SMA(adjusted, 20),1,0) %>% lag(),

  ) %>% 
  select(-c(symbol, open, high, low, close, adjusted, volume)) %>% 
  ungroup(names)
# Absolute Trend ###############################################################
AbsoluteTrendSignals <- data %>%
  group_by(names) %>%
  mutate(
    
    SMA200_LS = ifelse(adjusted >= SMA(adjusted, 200),1,-1) %>% lag(),
    SMA100_LS = ifelse(adjusted >= SMA(adjusted, 100),1,-1) %>% lag(),
    SMA50_LS = ifelse(adjusted >= SMA(adjusted, 50),1,-1) %>% lag(),
    SMA20_LS = ifelse(adjusted >= SMA(adjusted, 20),1,-1) %>% lag(),
    SMA5_LS = ifelse(adjusted >= SMA(adjusted, 5),1,-1) %>% lag(),
    
    
    CROSS_100_200_LS = ifelse(SMA(adjusted, 100) >= SMA(adjusted, 200),1,-1) %>% lag(),
    CROSS_50_100_LS = ifelse(SMA(adjusted, 50) >= SMA(adjusted, 100),1,-1) %>% lag(),
    CROSS_20_50_LS = ifelse(SMA(adjusted, 20) >= SMA(adjusted, 50),1,-1) %>% lag(),
    CROSS_5_20_LS = ifelse(SMA(adjusted, 5) >= SMA(adjusted, 20),1,-1) %>% lag(),
  ) %>% 
  select(-c(symbol, open, high, low, close, adjusted, volume)) %>% 
  ungroup(names)

# Residual Trend ###############################################################
ResidualTrendSignals <- DEC



# Absolute Momentum ############################################################


  
################################################################################
# TESTING #
################################################################################
AbsoluteTrendSignals_test <- TestSignal(DATA=DEC , SIGNALS=AbsoluteTrendSignals)

AbsoluteTrendSignals_test %>% 
  mutate(excessSharpe = sharpe_strat - sharpe_lo,
         excessReturn = cum_mean_strat - cum_mean_lo,
         excessAlpha = mean_alpha_strat - mean_alpha_lo,
         excessOutperformance = (mean_alpha_strat + mean_epsilon_strat) - (mean_alpha_lo + mean_epsilon_lo)) %>% 
  ggplot() +
  geom_col(aes(x=names, y=mean_alpha_strat, fill=names)) +
  facet_wrap(~strategy)



################################################################################
# BACKTEST ENGINE #
################################################################################
# To Do 
## Constant rebalancing
## different frequencies
## Staggered with different frequencies


# Equal Weighted ###############################################################
SIGNAL <- SIGNALS %>% select(date, names, SMA200_LS) %>% na.omit()


firstTest <- BacktestEngine(SIGNALS=AbsoluteTrendSignals , RETURNS, LO = FALSE)

p <- firstTest %>% 
  left_join(., X) %>% 
  filter(date >= "2013-01-01") %>% 
  pivot_longer(cols=colnames(.)[-1]) %>% 
  na.omit() %>% 
  group_by(name) %>% 
  arrange(date) %>% 
  mutate(value=cum.ret(value)) %>% 
  ungroup(name) %>% 
  ggplot(.) +
  geom_line(aes(x=date, y=value, color=name)) + 
  theme_tq()

ggplotly(p)


BacktestEngine <- function(SIGNALS, RETURNS, LO){
  signals <- SIGNALS %>% select(-c(date, names)) %>% colnames()
  
  res <- signals %>% 
    lapply(., function(s){
      SIGNAL <- SIGNALS %>% select(date, names, all_of(s)) %>% na.omit()
      tmp_test <- BacktestFunction_equal(SIGNAL, RETURNS, LO = LO, mode = "constant")
      tmp_test$signal <- s
      return(tmp_test)
    }) %>% 
    rbindlist() %>% 
    as.data.frame() %>% 
    pivot_wider(values_from = return, names_from=signal)
  return(res)
}


BacktestFunction_equal <- function(SIGNAL, RETURNS, LO = TRUE,
                                   mode = "constant"){
  if(mode=="constant"){
    signal <- colnames(SIGNAL)[3]
    
    if(LO){
      res <- SIGNAL %>% 
        na.omit() %>% 
        pivot_wider(names_from = names, values_from = !!sym(signal)) %>% 
        .[,-1] %>% 
        rowSums() %>% 
        data.frame("date"=unique(SIGNAL$date),
                   "rowsum"=.) %>% 
        left_join(SIGNAL,., by="date") %>% 
        left_join(., RETURNS) %>% 
        mutate(contribution =ifelse(rowsum > 0, !!sym(signal)*return/rowsum,0)) %>% 
        select(date, names, contribution) %>% 
        pivot_wider(names_from = names, values_from = contribution) %>% 
        na.locf() %>% 
        .[,-1] %>% 
        rowSums() %>% 
        data.frame("date"=unique(SIGNAL$date),
                   "return"=.)  
    } else {
      res <- SIGNAL %>% 
        na.omit() %>% 
        pivot_wider(names_from = names, values_from = !!sym(signal))
      res$posum <- res[,-1] %>% apply(.,1,function(x){sum(x[x>0])})
      res$nesum <- res[,-1] %>% apply(.,1,function(x){sum(abs(x[x<0]))})
      
      res <- res %>%
        select(date, posum, nesum) %>% 
        left_join(SIGNAL,., by="date") %>% 
        left_join(., RETURNS) %>% 
        mutate(contribution_pos =ifelse(posum > 0, !!sym(signal)*return/posum,0),
               contribution_neg =ifelse(nesum > 0, !!sym(signal)*return/nesum,0),
               contribution = ifelse( !!sym(signal) == 1,  contribution_pos,  contribution_neg)) %>% 
        select(date, names, contribution) %>% 
        pivot_wider(names_from = names, values_from = contribution) %>% 
        na.locf() %>% 
        .[,-1] %>% 
        rowSums() %>% 
        data.frame("date"=unique(SIGNAL$date),
                   "return"=.)  
    }
  }
  return(res)
}




# Functions vorerst hier -------------------------------------------------------
TestSignal <- function(DATA, SIGNALS){
  
  signals <- SIGNALS %>% select(-c(date, names)) %>% colnames()
  
  signals %>% 
    lapply(., function(s){
      
      # Combine
      temp_df <- SIGNALS %>%
        select(date, names, all_of(s)) %>% 
        left_join(DATA,.) %>% 
        na.omit()

      # Summarize
      summary_1 <- temp_df %>% 
        na.omit() %>% 
        group_by(names) %>% 
        summarize(
          "mean_strat"=(1 + mean(!!sym(s) * return))^(252)-1,
          "mean_alpha_strat" = mean(!!sym(s) * alpha),
          "mean_epsilon_strat" = mean(!!sym(s) * epsilon),
          "vol_strat"=sqrt(252)*sd(!!sym(s) * return),
          "skew_strat"=skewness(!!sym(s) * return),
          
          "mean_lo"=(1 + mean(return))^(252)-1,
          "mean_alpha_lo" = mean(alpha),
          "mean_epsilon_lo" = mean(epsilon),
          "vol_lo"=sqrt(252)*sd(return),
          "skew_lo"=skewness(return),
          
          "cum_mean_strat"=mean_strat - (vol_strat^2)/2,
          "cum_mean_lo"=mean_lo - (vol_lo^2)/2,
          "sharpe_strat"=cum_mean_strat/vol_strat,
          "sharpe_lo"=cum_mean_lo/vol_lo
        )
      
      summary_2 <- data.frame(
        "names"="complete",
        "mean_strat"=(1 + mean(unlist(temp_df[,s])*temp_df$return))^(252)-1,
        "mean_alpha_strat" = mean(unlist(temp_df[,s]) * temp_df$alpha),
        "mean_epsilon_strat" = mean(unlist(temp_df[,s])* temp_df$epsilon),
        "vol_strat"=sqrt(252)*sd(unlist(temp_df[,s])*temp_df$return),
        "skew_strat"=skewness(unlist(temp_df[,s])*temp_df$return),
        
        "mean_lo"=(1 + mean(temp_df$return))^(252)-1,
        "mean_alpha_lo" = mean(temp_df$alpha),
        "mean_epsilon_lo" = mean(temp_df$epsilon),
        "vol_lo"=sqrt(252)*sd(temp_df$return),
        "skew_lo"=skewness(temp_df$return)
      )
      summary_2[, "cum_mean_strat"] =summary_2$mean_strat - (summary_2$vol_strat^2)/2
      summary_2[, "cum_mean_lo"] =summary_2$mean_lo - (summary_2$vol_lo^2)/2
      summary_2[, "sharpe_strat"] =summary_2$cum_mean_strat/summary_2$vol_strat
      summary_2[, "sharpe_lo"] =summary_2$cum_mean_lo/summary_2$vol_lo
      
      summary_1 <- summary_1 %>% 
        rbind(., summary_2) %>% 
        mutate(strategy = s)
      return(summary_1)
      
    }) %>% 
    rbindlist()
}
TestSignal <- function(RETURNS, SIGNALS){
  
  signals <- SIGNALS %>% select(-c(date, names)) %>% colnames()
  
  signals %>% 
    lapply(., function(s){
      
      # Combine
      temp_df <- SIGNALS %>%
        select(date, names, all_of(s)) %>% 
        left_join(., RETURNS) %>% 
        na.omit()
      temp_df[,s] <- temp_df[,s] * temp_df$return
      
      # asses 
      summary_1 <- temp_df %>% 
        na.omit() %>% 
        group_by(names) %>% 
        summarize(
          "mean_strat"=(1 + mean(!!sym(s)))^(252)-1,
          "vol_strat"=sqrt(252)*sd(!!sym(s)),
          "skew_strat"=skewness(!!sym(s)),
          
          "mean_lo"=(1 + mean(return))^(252)-1,
          "vol_lo"=sqrt(252)*sd(return),
          "skew_lo"=skewness(return),
          
          "cum_mean_strat"=mean_strat - (vol_strat^2)/2,
          "cum_mean_lo"=mean_lo - (vol_lo^2)/2,
          "sharpe_strat"=cum_mean_strat/vol_strat,
          "sharpe_lo"=cum_mean_lo/vol_lo
        )
      
      summary_2 <- data.frame(
        "names"="complete",
        "mean_strat"=(1 + mean(unlist(temp_df[,s])))^(252)-1,
        "vol_strat"=sqrt(252)*sd(unlist(temp_df[,s])),
        "skew_strat"=skewness(unlist(temp_df[,s])),
        
        "mean_lo"=(1 + mean(temp_df$return))^(252)-1,
        "vol_lo"=sqrt(252)*sd(temp_df$return),
        "skew_lo"=skewness(temp_df$return)
      )
      summary_2[, "cum_mean_strat"] =summary_2$mean_strat - (summary_2$vol_strat^2)/2
      summary_2[, "cum_mean_lo"] =summary_2$mean_lo - (summary_2$vol_lo^2)/2
      summary_2[, "sharpe_strat"] =summary_2$cum_mean_strat/summary_2$vol_strat
      summary_2[, "sharpe_lo"] =summary_2$cum_mean_lo/summary_2$vol_lo
      
      summary_1 <- summary_1 %>% 
        rbind(., summary_2) %>% 
        mutate(strategy = s)
      return(summary_1)
      
    }) %>% 
    rbindlist()
}
