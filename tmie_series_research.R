df_ts <- data %>% 
  filter(names =="Banks") %>% 
  select(-names, -symbol) %>% 
  na.omit()
df_ts$index <- c(1:nrow(df_ts))


df_ts

test_ts <- df_ts %>% 
  select(index, log_ß_Mkt_1B) %>% 
  as_tsibble(index = index)


test_ts %>% 
  ACF(log_ß_Mkt_1B) %>% 
  autoplot()

test_ts %>% 
  ACF(CHANGE(log_ß_Mkt_1B)) %>% 
  autoplot()
