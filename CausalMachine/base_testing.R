source("R/dependencies.R")


# base data
# data <- tq_get(c("^STOXX", "EXSA.DE"), from = "1900-01-01")
data <- tq_get("^STOXX", from = "1900-01-01")


# Calculate Indicator
data_1 <- data %>% 
  na.omit() %>% 
  mutate(SMA20 = (adjusted - SMA(adjusted, 20))/adjusted) %>% 
  mutate(ret = RETURN(adjusted),
         log_ret = log(1+ret)) %>% 
  na.omit() %>%
  select(date, adjusted, ret, log_ret, SMA20)



# pick random chunk
chunksize <- round(nrow(data_1)*0.1)
chunkstart <- round(runif(1,1,nrow(data_1)-chunksize))
data_chunk <- data_1[seq(chunkstart, chunkstart+chunksize),]
desired_holding_period <- 5 

# maximise feature threshold (for start naive)
stepsize <- max(data_chunk$SMA20) - min(data_chunk$SMA20)
stepsize <-  stepsize / 1000

h_tests <- c(1:1001) %>% 
  lapply(., function(step){
    temp_th <- stepsize * (step-1) + min(data_chunk$SMA20)
    data_chunk$group <- ifelse(data_chunk$SMA20>temp_th, 1, -1)
    
    if(abs(mean(data_chunk$group))==1){
      h_test <- 9999
      aucor <- 0
      sample_diff <- 0
      sample_ratio <- 0
    } else{
      sample_mean_1 <- data_chunk %>% filter(group == 1) %>% pull(log_ret) %>% mean(., na.rm=TRUE)
      sample_mean_2 <- data_chunk %>% filter(group == -1) %>% pull(log_ret) %>% mean(., na.rm=TRUE)
      
      sample_var_1 <- data_chunk %>% filter(group == 1) %>% pull(log_ret) %>% var(., na.rm=TRUE)
      sample_var_2 <- data_chunk %>% filter(group == -1) %>% pull(log_ret) %>% var(., na.rm=TRUE)
      
      sample_n_1 <- data_chunk %>% filter(group == 1) %>% nrow()
      sample_n_2 <- data_chunk %>% filter(group == -1) %>% nrow()
      
      h_test <- (sample_mean_1 - sample_mean_2) / sqrt((sample_var_1 / sample_n_1) + (sample_var_2 / sample_n_2))
      aucor <- data_chunk %>% mutate(group_lag = lag(group)) %>% select(group, group_lag) %>% na.omit() %>% cor() %>% .[1,2]
      sample_diff <- sample_mean_1 - sample_mean_2
      sample_ratio <- sample_n_1 / data_chunk %>% nrow()
    }

    return(data.frame(h_test, aucor, temp_th, sample_diff, sample_ratio))
    
  }) %>% rbindlist()


# filter and decide
filtered_test <- h_tests %>% 
  filter(!is.na(h_test)) %>% 
  filter(aucor >= 0.5) %>% 
  mutate(p_val = dnorm(x = h_test, mean = 0, sd = 1)) %>% 
  filter(p_val < 0.05) %>% 
  filter(sample_ratio < 0.8) %>% 
  filter(sample_ratio > 0.2) %>% 
  arrange(sample_diff)

optimized_th <- filtered_test %>% tail(.,1) %>% pull(temp_th)


# Sample the rest data into two groups -----------------------------------------
data_wo_chunk <- data_1 %>% filter(!(date %in% data_chunk$date))
sample_1 <- sample(seq(1:nrow(data_wo_chunk)), size = round(nrow(data_wo_chunk)/2))

data_wo_chunk$group <- ifelse(data_wo_chunk$SMA20>optimized_th, 1, -1)

data_chunk_2 <- data_wo_chunk[sample_1,]
data_chunk_3 <- data_wo_chunk %>% filter(!(date %in% data_chunk_2$date))




sample_mean_random_group_intervention_1 <- data_chunk_2 %>% filter(group == 1) %>% pull(log_ret) %>% mean(., na.rm=TRUE)
sample_mean_random_group_1 <- data_chunk_3 %>% pull(log_ret) %>% mean(., na.rm=TRUE)

sample_mean_random_group_intervention_2 <- data_chunk_3 %>% filter(group == 1) %>% pull(log_ret) %>% mean(., na.rm=TRUE)
sample_mean_random_group_2 <- data_chunk_3 %>% pull(log_ret) %>% mean(., na.rm=TRUE)




sample_mean_random_group_intervention_1
sample_mean_random_group_1

sample_mean_random_group_intervention_2
sample_mean_random_group_2




