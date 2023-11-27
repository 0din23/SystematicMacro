# create causal grouping

TestCausality <- function(data, label, signal, no_groups = 2){
  
  
  # Split of first random chunk ------------------------------------------------
  chunksize <- round(nrow(data)*0.1)
  chunkstart <- round(runif(1,1,nrow(data)-chunksize))
  data_chunk <- data[seq(chunkstart, chunkstart+chunksize),]
  
  # Test efficacy of different thresholds --------------------------------------
  stepsize <- max(data_chunk[, signal]) - min(data_chunk[, signal])
  stepsize <-  stepsize / 1000
  
  h_tests <- c(1:1001) %>% 
    lapply(., function(step){
      temp_th <- stepsize * (step-1) + min(data_chunk[, signal])
      data_chunk$group <- ifelse(data_chunk[, signal]>temp_th, 1, -1)
      
      if(abs(mean(data_chunk$group))==1){
        h_test <- 9999
        aucor <- 0
        sample_diff <- 0
        sample_ratio <- 0
      } else{
        sample_mean_1 <- data_chunk %>% filter(group == 1) %>% pull(!!label) %>% mean(., na.rm=TRUE)
        sample_mean_2 <- data_chunk %>% filter(group == -1) %>% pull(!!label) %>% mean(., na.rm=TRUE)
        
        sample_var_1 <- data_chunk %>% filter(group == 1) %>% pull(!!label) %>% var(., na.rm=TRUE)
        sample_var_2 <- data_chunk %>% filter(group == -1) %>% pull(!!label) %>% var(., na.rm=TRUE)
        
        sample_n_1 <- data_chunk %>% filter(group == 1) %>% nrow()
        sample_n_2 <- data_chunk %>% filter(group == -1) %>% nrow()
        
        h_test <- (sample_mean_1 - sample_mean_2) / sqrt((sample_var_1 / sample_n_1) + (sample_var_2 / sample_n_2))
        aucor <- data_chunk %>% mutate(group_lag = lag(group)) %>% select(group, group_lag) %>% na.omit() %>% cor() %>% .[1,2]
        sample_diff <- sample_mean_1 - sample_mean_2
        sample_ratio <- sample_n_1 / data_chunk %>% nrow()
      }
      
      return(data.frame(h_test, aucor, temp_th, sample_diff, sample_ratio))
      
    }) %>% rbindlist()

  # filter and decide ----------------------------------------------------------
  filtered_test <- h_tests %>% 
    filter(!is.na(h_test)) %>% 
    filter(aucor >= 0.5) %>% 
    mutate(p_val = dnorm(x = h_test, mean = 0, sd = 1)) %>% 
    filter(p_val < 0.05) %>% 
    filter(sample_ratio < 0.8) %>% 
    filter(sample_ratio > 0.2) %>% 
    arrange(sample_diff)
  
  
  if(no_groups == 2){
    optimized_th <- filtered_test %>% tail(.,1) %>% pull(temp_th)
  } else{
    short_signal_th <- filtered_test %>% tail(.,1) %>% pull(temp_th)
    long_signal_th <- filtered_test %>% head(.,1) %>% pull(temp_th)
  }
  

  # Sample the rest data into two groups ---------------------------------------
  data_wo_chunk <- data %>% filter(!(date %in% data_chunk$date))
  sample_1 <- sample(seq(1:nrow(data_wo_chunk)), size = round(nrow(data_wo_chunk)/2))
  
  if(no_groups == 2){
    
    data_wo_chunk$group <- ifelse(data_wo_chunk[, signal] > optimized_th, 1, -1)
    
    data_chunk_2 <- data_wo_chunk[sample_1,]
    data_chunk_3 <- data_wo_chunk %>% filter(!(date %in% data_chunk_2$date))
    
  } else{
    

    
  }

  # Test if it holds -----------------------------------------------------------
  if(no_groups == 2){
    
    sample_mean_random_group_intervention_1 <- data_chunk_2 %>% filter(group == 1) %>% pull(!!label) %>% mean(., na.rm=TRUE)
    sample_mean_random_group_1 <- data_chunk_3 %>% pull(!!label) %>% mean(., na.rm=TRUE)
    
    sample_mean_random_group_intervention_2 <- data_chunk_3 %>% filter(group == 1) %>% pull(!!label) %>% mean(., na.rm=TRUE)
    sample_mean_random_group_2 <- data_chunk_3 %>% pull(!!label) %>% mean(., na.rm=TRUE)
    
    
    
    
    goup_1_1_test <- sample_mean_random_group_intervention_1 > sample_mean_random_group_1
    goup_1_2_test <- sample_mean_random_group_intervention_1 > sample_mean_random_group_2
    
    goup_2_2_test <- sample_mean_random_group_intervention_2 > sample_mean_random_group_2
    goup_2_1_test <- sample_mean_random_group_intervention_2 > sample_mean_random_group_1
    
    res <- data.frame(
      goup_1_1_test, goup_1_2_test, goup_2_2_test, goup_2_1_test
    )  
    return(res)
    
  } else{
    
    
    
  }
  
}



#  Construct Good feature ------------------------------------------------------

CausalFeature <- function(data, label, signal, no_groups = 2, th = FALSE){
  
  
  # Split of first random chunk ------------------------------------------------
  data_chunk <- data %>% na.omit()
  
  # Test efficacy of different thresholds --------------------------------------
  stepsize <- max(data_chunk[, signal]) - min(data_chunk[, signal])
  stepsize <-  stepsize / 1000
  
  h_tests <- c(1:1001) %>% 
    lapply(., function(step){
      temp_th <- stepsize * (step-1) + min(data_chunk[, signal])
      data_chunk$group <- ifelse(data_chunk[, signal]>temp_th, 1, -1)
      
      if(abs(mean(data_chunk$group))==1){
        h_test <- 9999
        aucor <- 0
        sample_diff <- 0
        sample_ratio <- 0
      } else{
        sample_mean_1 <- data_chunk %>% filter(group == 1) %>% pull(!!label) %>% mean(., na.rm=TRUE)
        sample_mean_2 <- data_chunk %>% filter(group == -1) %>% pull(!!label) %>% mean(., na.rm=TRUE)
        
        sample_var_1 <- data_chunk %>% filter(group == 1) %>% pull(!!label) %>% var(., na.rm=TRUE)
        sample_var_2 <- data_chunk %>% filter(group == -1) %>% pull(!!label) %>% var(., na.rm=TRUE)
        
        sample_n_1 <- data_chunk %>% filter(group == 1) %>% nrow()
        sample_n_2 <- data_chunk %>% filter(group == -1) %>% nrow()
        
        h_test <- (sample_mean_1 - sample_mean_2) / sqrt((sample_var_1 / sample_n_1) + (sample_var_2 / sample_n_2))
        aucor <- data_chunk %>% mutate(group_lag = lag(group)) %>% select(group, group_lag) %>% na.omit() %>% cor() %>% .[1,2]
        sample_diff <- sample_mean_1 - sample_mean_2
        sample_ratio <- sample_n_1 / data_chunk %>% nrow()
      }
      
      return(data.frame(h_test, aucor, temp_th, sample_diff, sample_ratio))
      
    }) %>% rbindlist()
  
  # filter and decide ----------------------------------------------------------
  filtered_test <- h_tests %>% 
    filter(!is.na(h_test)) %>% 
    filter(aucor >= 0.5) %>% 
    mutate(p_val = dnorm(x = h_test, mean = 0, sd = 1)) %>% 
    filter(p_val < 0.05) %>% 
    filter(sample_ratio < 0.8) %>% 
    filter(sample_ratio > 0.2) %>% 
    arrange(sample_diff)
  
  
  if(no_groups == 2){
    optimized_th <- filtered_test %>% tail(.,1) %>% pull(temp_th)
    if(th){
      return(optimized_th)
    } else{
      return(ifelse(data[,signal]>optimized_th,1,0)) 
    }
  } else{
    short_signal_th <- filtered_test %>% tail(.,1) %>% pull(temp_th)
    long_signal_th <- filtered_test %>% head(.,1) %>% pull(temp_th)
  }
  
  
  
}
