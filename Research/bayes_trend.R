source("R/dependencies.R")

# Get Data #####################################################################
data <- tq_get("^GSPC", from = "1900-01-01") %>% 
  mutate(
    return = RETURN(adjusted),
    #return = log(1+return)
    ) %>% 
  na.omit()


# Specify Inputs ###############################################################
LAG <- 60
df <- data
th_p <- 0.3


# calculate stuff
df <- df %>% 
  mutate(
    prior_mean_estimate = rollapplyr(return, FUN=mean, align = "right", width = seq_along(return)),
    prior_sd_estimate = (rollapplyr(return, FUN=sd, align = "right", width = seq_along(return)))^2,
    
    sample_mean = SMA(return, LAG),
    sample_var = runSD(return, LAG)^2,
    
    posterior_var_estimate = 1 / (LAG/sample_var + 1/prior_sd_estimate),
    posterior_mean_estimate = posterior_var_estimate * (prior_mean_estimate/ prior_sd_estimate + LAG * sample_mean / sample_var),
    
    signal_p = pnorm(0, mean = posterior_mean_estimate, sd = sqrt(posterior_var_estimate)),
    signal = ifelse(signal_p <= th_p, 1, ifelse(signal_p > (1-th_p),-1,0))
  ) %>% 
  mutate(
    label_mean = SMA(return, LAG) %>% shift(-(1+LAG)),
    label_var = runSD(return, LAG)^2 %>% shift(-(1+LAG)),
    
    perfect_vol_signal_p = pnorm(0, mean = posterior_mean_estimate, sd = sqrt(label_var)),
    perfect_signal_p = pnorm(0, mean = label_mean, sd = sqrt(label_var)),
    
    perfect_vol_signal = ifelse(perfect_vol_signal_p <= th_p, 1, ifelse(perfect_vol_signal_p > (1-th_p),-1,0)),
    perfect_signal = ifelse(perfect_signal_p <= th_p, 1, ifelse(perfect_signal_p > (1-th_p),-1,0)),
    
    backtest_long = lag(ifelse(signal == -1,0,signal)) * return,
    backtest_short = lag(ifelse(signal == 1,0,signal)) * return,
    backtest_combined = lag(signal)*return,
    
    perfect_vol_backtest_long = lag(ifelse(perfect_vol_signal == -1,0,perfect_vol_signal)) * return,
    perfect_vol_backtest_short = lag(ifelse(perfect_vol_signal == 1,0,perfect_vol_signal)) * return,
    perfect_vol_backtest_combined = lag(perfect_vol_signal)*return,
    
    perfect_backtest_long = lag(ifelse(perfect_signal == -1,0,perfect_signal)) * return,
    perfect_backtest_short = lag(ifelse(perfect_signal == 1,0,perfect_signal)) * return,
    perfect_backtest_combined = lag(perfect_signal)*return,
  )
  

df %>% 
  na.omit() %>% 
  select(-symbol, -date) %>% 
  select(posterior_mean_estimate, posterior_var_estimate,
         signal, signal_p,
         label_mean, label_var) %>% 
  cor()

df %>%
  na.omit() %>% 
  ggplot(.) +
  geom_line(aes(x=date, y = cum.ret(return), color = "Buy and Hold")) +
  geom_line(aes(x=date, y = cum.ret(backtest_long), color = "long"))+
  geom_line(aes(x=date, y = cum.ret(backtest_short), color = "short"))+
  geom_line(aes(x=date, y = cum.ret(backtest_combined), color = "combi"))


df %>%
  na.omit() %>% 
  ggplot(.) +
  geom_line(aes(x=date, y = cum.ret(return), color = "Buy and Hold")) +
  geom_line(aes(x=date, y = cum.ret(perfect_vol_backtest_long), color = "long"))+
  geom_line(aes(x=date, y = cum.ret(perfect_vol_backtest_short), color = "short"))+
  geom_line(aes(x=date, y = cum.ret(perfect_vol_backtest_combined), color = "combi"))


df %>%
  na.omit() %>% 
  ggplot(.) +
  geom_line(aes(x=date, y = cum.ret(return), color = "Buy and Hold")) +
  geom_line(aes(x=date, y = cum.ret(perfect_backtest_long), color = "long"))+
  geom_line(aes(x=date, y = cum.ret(perfect_backtest_short), color = "short"))+
  geom_line(aes(x=date, y = cum.ret(perfect_backtest_combined), color = "combi"))

