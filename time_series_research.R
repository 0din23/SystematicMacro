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
  tidyquant::tq_get(., from = "2010-01-01") %>% 
  left_join(., instruments, by = c("symbol"="ticker")) %>%
  na.omit()

benchmark <- tq_get("EXSA.DE", from = "2010-01-01")

################################################################################
# LOGBASED NON OVERLAPPING DECOMPOSITION #
################################################################################

## inputs
h <- 5
regression_lag <- h 

data <- data %>% 
  group_by(names) %>% 
  mutate(
    return_1B = RETURN(adjusted),
    log_return_1B = CHANGE(log(adjusted)),
    
    return_hB = RETURN(adjusted, h),
    log_return_hB = CHANGE(log(adjusted), h)
  ) %>% 
  left_join(., benchmark %>% 
              mutate(
                return_1B = RETURN(adjusted),
                log_return_1B = CHANGE(log(adjusted)),
                
                return_hB = RETURN(adjusted, h),
                log_return_hB = CHANGE(log(adjusted), h)
              ) %>% 
              select(date, return_1B, log_return_1B, return_hB, log_return_hB)
            , by = "date", suffix = c("", "_bench")) %>% 
  na.omit() %>% 
  group_by(names) %>% 
  mutate(
    ß_Mkt_1B = roll_regres(return_1B ~ return_1B_bench, width = regression_lag)[["coefs"]][,2],
    alpha_1B = roll_regres(return_1B ~ return_1B_bench, width = regression_lag)[["coefs"]][,1],
    y_hat_1B = alpha_1B + ß_Mkt_1B * return_1B_bench,
    epsilon_1B = return_1B - y_hat_1B,
    
    
    log_ß_Mkt_1B = roll_regres(log_return_1B ~ log_return_1B_bench, width = regression_lag)[["coefs"]][,2],
    log_alpha_1B = roll_regres(log_return_1B ~ log_return_1B_bench, width = regression_lag)[["coefs"]][,1],
    log_y_hat_1B = log_alpha_1B + log_ß_Mkt_1B * log_return_1B_bench,
    log_epsilon_1B = log_return_1B - log_y_hat_1B,
    
    log_epsilon_1B_bar_hB = SMA(log_epsilon_1B, h),
    log_alpha_1B_bar = log_alpha_1B + log_epsilon_1B_bar_hB,
    log_y_hat_hB = h * log_alpha_1B_bar + log_ß_Mkt_1B * log_return_hB_bench,
    log_epsilon_hB = log_return_hB - log_y_hat_hB,
    log_y_hat_hB_transformed = exp(h*log_alpha_1B_bar) * (1+ return_hB_bench)^(log_ß_Mkt_1B) -1,
    log_epsilon_hB_transformed = return_hB - log_y_hat_hB_transformed
  ) %>% 
  ungroup(names)


################################################################################
# START RESEARCH #
################################################################################
df_ts <- data %>% 
  filter(names =="Banks") %>% 
  select(-names, -symbol) %>% 
  na.omit()
df_ts$index <- c(1:nrow(df_ts))


df_ts %>% colnames()


test_column <- "log_alpha_1B_bar"

test_ts <- df_ts %>% 
  select(index, all_of(test_column)) %>% 
  as_tsibble(index = index)

test_ts %>% 
  ggplot() +
  geom_line(aes(x=index, y = get(test_column)))


test_ts %>% 
  #pull(all_of(test_column)) %>% 
  ACF() %>% 
  autoplot()

test_ts %>%
  ACF(CHANGE(log_alpha_1B_bar)) %>% 
  autoplot()

################################################################################
# FORECASTING TESTS #
################################################################################

df <- data %>% 
  group_by(names) %>% 
  mutate(
    label = (log_alpha_1B_bar *h) %>% shift((-h+1))
  ) %>% 
  ungroup(names) %>% 
  select(-symbol) %>% 
  na.omit()

# Last Value ###################################################################
# Evaluate
df %>% 
  group_by(names) %>% 
  summarize(
    
    RMSE_5B = sqrt(mean((label - log_alpha_1B_bar*h)^2, na.rm=T)),
    DIRECTION_5B = mean(ifelse(label/ log_alpha_1B_bar*h > 0, 1, 0), na.rm =T)
    
  )

# Runnig mean as a forecast ####################################################
# forecast
df <- df %>% 
  group_by(names) %>% 
  mutate(
    forecast_SMA5 = SMA(log_alpha_1B_bar*h, 5),
    forecast_SMA20 = SMA(log_alpha_1B_bar*h, 20),
    forecast_SMA60 = SMA(log_alpha_1B_bar*h, 60),
    forecast_SMA130 = SMA(log_alpha_1B_bar*h, 130)
  )%>% 
  ungroup(names)

# Evaluate
df %>% 
  group_by(names) %>% 
  summarize(
    
    RMSE_5B = sqrt(mean((label - forecast_SMA5)^2, na.rm=T)),
    RMSE_20B = sqrt(mean((label - forecast_SMA20)^2, na.rm=T)),
    RMSE_60B = sqrt(mean((label - forecast_SMA60)^2, na.rm=T)),
    RMSE_130B = sqrt(mean((label - forecast_SMA130)^2, na.rm=T)),
    
    DIRECTION_5B = mean(ifelse(label/ forecast_SMA5 > 0, 1, 0), na.rm =T),
    DIRECTION_20B = mean(ifelse(label/ forecast_SMA20 > 0, 1, 0), na.rm =T),
    DIRECTION_60B = mean(ifelse(label/ forecast_SMA60 > 0, 1, 0), na.rm =T),
    DIRECTION_130B =mean(ifelse(label/ forecast_SMA130 > 0, 1, 0), na.rm =T)
    
  )

# Naive Seasonal Forecast ######################################################

# forecast
df <- df %>% 
  group_by(names) %>% 
  mutate(
    forecast_LAG5 = lag(log_alpha_1B_bar*h, 5),
    forecast_LAG20 = lag(log_alpha_1B_bar*h, 20),
    forecast_LAG60 = lag(log_alpha_1B_bar*h, 60),
    forecast_LAG130 = lag(log_alpha_1B_bar*h, 130)
  ) %>% 
  ungroup(names)

# Evaluate
df %>% 
  group_by(names) %>% 
  summarize(
    
    RMSE_5B = sqrt(mean((label - forecast_LAG5)^2, na.rm=T)),
    RMSE_20B = sqrt(mean((label - forecast_LAG20)^2, na.rm=T)),
    RMSE_60B = sqrt(mean((label - forecast_LAG60)^2, na.rm=T)),
    RMSE_130B = sqrt(mean((label - forecast_LAG130)^2, na.rm=T)),
    
    DIRECTION_5B = mean(ifelse(label/ forecast_LAG5 > 0, 1, 0), na.rm =T),
    DIRECTION_20B = mean(ifelse(label/ forecast_LAG20 > 0, 1, 0), na.rm =T),
    DIRECTION_60B = mean(ifelse(label/ forecast_LAG60 > 0, 1, 0), na.rm =T),
    DIRECTION_130B =mean(ifelse(label/ forecast_LAG130 > 0, 1, 0), na.rm =T)
  )


df



