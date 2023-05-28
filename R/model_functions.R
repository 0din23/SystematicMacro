regressionEvaluation <- function(label, predictions){
  
  res <- colnames(predictions) %>%
    cbind(., predictions %>%
            apply(.,2, function(x){
              reg <- lm(x~label) %>% summary()
              data.frame(
                "rsq" = reg$r.squared,
                "rsq_adj" = reg$adj.r.squared,
                "rmse" = RMSE(.resid = (label - x), na.rm = T),
                "mae" = MAE(.resid = (label - x), na.rm = T),
                "same_dir" = mean((x>0) == (label > 0), na.rm=T)
              ) %>% 
                return()
            }) %>%
            rbindlist() %>% 
            round(.,4)
    )
  return(res)
}

evaluateMomentumStrategy <- function(df, benchmark, weights){
  
  ## Calculate Performance Metrics
  df_ts <- xts(df[,-1], order.by = as.Date(df$date))
  res <- rbind(
    df_ts %>% Return.annualized(),
    (df_ts %>% Return.annualized()) / (df_ts %>% SharpeRatio.annualized()),
    df_ts %>% SharpeRatio.annualized(),
    df_ts %>% maxDrawdown(),
    df_ts %>% CalmarRatio(),
    df_ts %>% skewness(),
    df_ts %>% kurtosis()
  ) %>% 
    as.data.frame()
  rownames(res) <- c("Return", "Volatility", "Sharpe_Ratio",
                     "Max_Drawdown", "CalmarRatio", "Skew", "Kurtosis")
  
  ## Check Beta against Benchmark
  for(k in 1:(ncol(res)-1)){
    
    reg <- lm(df[[colnames(res)[k]]] ~ df[[benchmark]])
    res["alpha",k] <- reg$coefficients[1]
    res["beta",k] <- reg$coefficients[2]
    res["benchmark_cor",k] <- cor(df[[colnames(res)[k]]], df[[benchmark]])
  }
  
  ## Calculate Turnover
  for(k in 1:length(weights)){
    
    res["turnover",k] <- weights[[k]] %>% 
      select(-c(date, return)) %>%
      apply(.,2,function(x){
        mean(abs((x-lag(x))), na.rm=T) 
      }) %>% sum()
    
  }
  return(res %>% round(.,3))
}

# Ranking Test #################################################################
rank_test <- function(df, strats){
  
  ## Calculate label Ranking
  label_rank <- df %>% 
    select(date, names, all_of(c("opti_alpha"))) %>% 
    pivot_wider(.,names_from = names,
                values_from = "opti_alpha") %>% 
    .[,-1] %>% 
    apply(.,1,rank) %>%
    t() %>%
    as.data.frame() %>% 
    mutate(date = df$date %>%unique()) %>% 
    pivot_longer(.,cols = colnames(.)[!(colnames(.) %in% c("date", "names"))], 
                 values_to = "opti_alpha")
  
  ## Calculate Ranking Correlation
  for(k in 1:length(strats)){
    strat_rank <- df %>% 
      select(date, names, all_of(strats[k])) %>% 
      pivot_wider(.,names_from = names,
                  values_from = strats[k]) %>% 
      .[,-1] %>% 
      apply(.,1,rank) %>%
      t() %>%
      as.data.frame() %>% 
      mutate(date = df$date %>%unique()) %>% 
      pivot_longer(.,cols = colnames(.)[!(colnames(.) %in% c("date", "names"))],
                   values_to = strats[k])
    
    label_rank <- label_rank %>% 
      left_join(., strat_rank)
  }
  
  ### label rank CoMa
  res <- list()
  res$label_rank_CoMa <- label_rank %>% 
    select(-c(date, name)) %>% 
    cor()
  
  
  ### Intra sector rank
  res$intra_sector <- label_rank %>% 
    select(-date) %>% 
    pull(name) %>% 
    unique() %>% 
    lapply(.,function(x){
      temp <- label_rank %>% 
        filter(name == x) %>% 
        select(-c(date, name)) %>% 
        cor() %>% 
        as.data.frame() %>% 
        slice(-1)%>% 
        mutate(symbol = x,
               signal = rownames(.)) %>% 
        select(symbol, signal, opti_alpha)
      
      return(temp)
    }) %>% rbindlist()
  
  return(res)
}


classificationEvaluation <- function(label,  classification_prediction){
  
  res <- colnames(classification_prediction) %>%
    cbind(., classification_prediction %>%
            colnames(.) %>% 
            lapply(., function(x){
              coMa <- caret::confusionMatrix(reference=label,
                                             data=classification_prediction[[x]])
              return(coMa$byClass %>% t() %>% as.data.frame())
            }) %>%
            rbindlist() %>% 
            round(.,4)
    )
  return(res)
}