regressionEvaluation <- function(label, predictions){
  
  res <- colnames(predictions) %>%
    cbind(., predictions %>%
            apply(.,2, function(x){
              reg <- lm(x~label) %>% summary()
              data.frame(
                "rsq" = reg$r.squared,
                "rsq_adj" = reg$adj.r.squared,
                "rmse" = RMSE(x = x, label, na.rm = T),
                "mae" = MAE(x = x, label, na.rm = T),
                "same_dir" = mean((x>=0) & (label >= 0), na.rm=T)
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
    
    reg <- lm(df[,colnames(res)[k]] ~ df[,benchmark])
    res["alpha",k] <- reg$coefficients[1]
    res["beta",k] <- reg$coefficients[2]
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