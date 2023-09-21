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