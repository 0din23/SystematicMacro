## Ranks the Signal
momentumRanking <- function(SIGNAL){
  
  colnames(SIGNAL) <- c("symbol", "date", "signal")
  RANKING <- SIGNAL %>% 
    pivot_wider(.,names_from = symbol,
                values_from = signal) %>% 
    .[,-1] %>% 
    apply(.,1,rank) %>%
    t() %>%
    as.data.frame() %>% 
    cbind(SIGNAL$date %>%
            unique(),.)
  
  colnames(RANKING)[1] <- "date"
  return(RANKING)
}

momentumPortfolio <- function(RANKING, WEIGHTING = NULL, BETA_EXPOSURE = 0,
                              FREQ = "daily", is_ranking =TRUE, ranking_quantile = 0.5,
                              EXPOSURES = NULL){
  ## Equal weighting
  if(is.null(WEIGHTING)){
    
    WEIGHTS <- RANKING %>% 
      .[,-1] %>% 
      apply(.,1,function(x){
        
        if(is_ranking){
          cut <- quantile(x, probs = ranking_quantile) %>% as.numeric()
          w <- ifelse(x>=cut, 1,-1) / length(x)
          w[w>0] <- w[w>0] * (1+BETA_EXPOSURE) 
        } else{
          w <- ifelse(x>0, 1,-1) / length(x)
          w[w>0] <- w[w>0] * (1+BETA_EXPOSURE)
        }
        
        return(w)
        
      }) %>%
      t() %>%
      as.data.frame()
    
    WEIGHTS <- WEIGHTS %>% 
      cbind(RANKING %>% na.omit() %>% pull(date) %>% unique(),.)
    colnames(WEIGHTS)[1] <- "date"
  } else {
    
    ### Calculate Exposure Sign
    if(is_ranking){
      WEIGHTS <- RANKING %>% 
        .[,-1] %>% 
        apply(.,1,function(x){
          ifelse(x>=median(x), 1,-1)
        }) %>%
        t() %>%
        as.data.frame()      
    } else{
      WEIGHTS <- RANKING %>% 
        .[,-1] %>% 
        apply(.,1,function(x){
          ifelse(x>=0, 1,-1)
        }) %>%
        t() %>%
        as.data.frame()
    }
    
    WEIGHTS <- WEIGHTS %>% 
      cbind(RANKING$date %>% unique(),.)
    colnames(WEIGHTS)[1] <- "date"
    
    ### Calculate Scaled Exposure
    WEIGHTS <- WEIGHTS %>% 
      pivot_longer(.,names_to = "symbol",
                   values_to = "ranking", cols = colnames(.)[-1]) %>% 
      left_join(., WEIGHTING, by = c("date", "symbol")) %>% 
      na.omit()  %>% 
      mutate(
        "weighting" = weight_signal * ranking
      ) %>% 
      select(-c(ranking, weight_signal)) 
    WDATE <- WEIGHTS %>% pull(date) %>% unique()
    
    WEIGHTS <- WEIGHTS %>% 
      pivot_wider(values_from = "weighting",
                  names_from = "symbol") %>% 
      .[,-1] %>% 
      apply(.,1,function(x){
        
        x[x>0] <- sum(x[x>0]) / x[x>0]
        x[x>0] <- x[x>0] / sum(x[x>0])
        
        x[x<0] <- sum(abs(x[x<0])) / x[x<0]
        x[x<0] <- x[x<0] / sum(abs(x[x<0]))
        
        x[x>0] <- x[x>0] * (1+BETA_EXPOSURE)
        return(x)
      }) %>%
      t() %>%
      as.data.frame()
    
    WEIGHTS <- WEIGHTS %>% 
      cbind(WDATE,.) %>% 
      na.omit()
    colnames(WEIGHTS)[1] <- "date"
  }
  
  ## Null Exposures 
  if(!is.null(EXPOSURES)){
    
  }
  
  ## Rebalancing
  if(FREQ != "daily"){
    WEIGHTS$rebalance <- FALSE
    WEIGHTS$rebalance[myEndpoints(WEIGHTS$date, FREQ)] <- TRUE
    WEIGHTS[!WEIGHTS$rebalance,-c(1, ncol(WEIGHTS))] <- NA
    WEIGHTS <- WEIGHTS %>% EpsilonUtility::na_last_fill()
    WEIGHTS$rebalance <- NULL
  }
  return(WEIGHTS)
} 

momentumStrategy <- function(SIGNAL, RETURN, BENCHMARK, WEIGHTING = NULL,
                             BETA_EXPOSURE = 0, FREQ = "daily", with_ranking = TRUE,
                             ranking_quantile = 0.5, EXPOSURES = NULL){
  
  ## calculate Ranking
  if(with_ranking){
    RANKING <- momentumRanking(SIGNAL) 
  } else{
    RANKING <- SIGNAL
    colnames(RANKING) <- c("symbol", "date", "value")
    RANKING <- RANKING %>% 
      pivot_wider(., names_from = symbol, values_from = value) %>%
      as.data.frame()
  }
  
  ## calculate Weighting
  WEIGHTS <- momentumPortfolio(RANKING, WEIGHTING, BETA_EXPOSURE, FREQ, is_ranking = with_ranking,
                               ranking_quantile = ranking_quantile, EXPOSURES = EXPOSURES)
  
  ## Shift Date since we can only trade on the next day
  WEIGHTS$date <- c(WEIGHTS$date[-1],NA)
  WEIGHTS <- WEIGHTS %>% na.omit()
  
  ## Calculate Returns
  WEIGHTS$return <-  WEIGHTS$date %>% 
    lapply(.,function(x){
      sum(unlist(WEIGHTS[WEIGHTS$date == x,-1]) *
            unlist(RETURN[RETURN$date == x,-1]))
    }) %>% unlist()
  
  return(WEIGHTS)
}

multiSignalTest <- function(SIGNAL, RETURN, BENCHMARK, WEIGHTING = NULL,
                            BETA_EXPOSURE = 0, with_ranking = TRUE, ranking_quantile = 0.5,
                            EXPOSURES = NULL){
  
  ## return Object
  RES <- list()
  RES$daily <- list()
  RES$weekly <- list()
  RES$monthly <- list()
  RES$eval <- list()
  
  ## Calculate signals
  for(k in 3:ncol(SIGNAL)){
    
    temp_sig <- SIGNAL[,c(1,2,k)]
    temp_signal <- colnames(SIGNAL)[k]
    
    ## Construct Signals
    RES$daily[[temp_signal]] <- momentumStrategy(SIGNAL = temp_sig,
                                                 RETURN = RETURN,
                                                 BENCHMARK = BENCHMARK,
                                                 WEIGHTING = WEIGHTING,
                                                 BETA_EXPOSURE = BETA_EXPOSURE,
                                                 FREQ = "daily",
                                                 with_ranking = with_ranking,
                                                 ranking_quantile = ranking_quantile,
                                                 EXPOSURES = EXPOSURES)
    
    RES$weekly[[temp_signal]] <- momentumStrategy(SIGNAL = temp_sig,
                                                  RETURN = RETURN,
                                                  BENCHMARK = BENCHMARK,
                                                  WEIGHTING = WEIGHTING,
                                                  BETA_EXPOSURE = BETA_EXPOSURE,
                                                  FREQ = "weekly", 
                                                  with_ranking = with_ranking,
                                                  ranking_quantile = ranking_quantile,
                                                  EXPOSURES = EXPOSURES)
    
    RES$monthly[[temp_signal]] <- momentumStrategy(SIGNAL = temp_sig,
                                                   RETURN = RETURN,
                                                   BENCHMARK = BENCHMARK,
                                                   WEIGHTING = WEIGHTING,
                                                   BETA_EXPOSURE = BETA_EXPOSURE,
                                                   FREQ = "monthly", 
                                                   with_ranking = with_ranking,
                                                   ranking_quantile = ranking_quantile,
                                                   EXPOSURES = EXPOSURES)
    
    ## Construct Evaluation
    if(k == 3){
      RES$eval$daily <- RES$daily[[temp_signal]] %>% select(date, return)
      RES$eval$weekly <- RES$weekly[[temp_signal]] %>% select(date,return)
      RES$eval$monthly <- RES$monthly[[temp_signal]] %>% select(date, return)
      
      colnames(RES$eval$daily)[colnames(RES$eval$daily) == "return"] <- temp_signal
      colnames(RES$eval$weekly)[colnames(RES$eval$weekly) == "return"] <- temp_signal
      colnames(RES$eval$monthly)[colnames(RES$eval$monthly) == "return"] <- temp_signal
      
    } else{
      
      RES$eval$daily <- RES$eval$daily %>% 
        left_join(.,RES$daily[[temp_signal]] %>% select(date, return),by =c("date"))
      
      RES$eval$weekly <- RES$eval$weekly %>% 
        left_join(.,RES$weekly[[temp_signal]] %>% select(date, return),by =c("date"))
      
      RES$eval$monthly <- RES$eval$monthly %>% 
        left_join(.,RES$monthly[[temp_signal]] %>% select(date, return), by =c("date"))   
      
      colnames(RES$eval$daily)[colnames(RES$eval$daily) == "return"] <- temp_signal
      colnames(RES$eval$weekly)[colnames(RES$eval$weekly) == "return"] <-temp_signal
      colnames(RES$eval$monthly)[colnames(RES$eval$monthly) == "return"] <-temp_signal
    }
  }
  
  ## Also Join the Benchmark
  RES$eval$daily <- RES$eval$daily %>% 
    left_join(.,BENCHMARK, by =c("date"))
  
  RES$eval$weekly <- RES$eval$weekly %>% 
    left_join(.,BENCHMARK, by =c("date"))
  
  RES$eval$monthly <- RES$eval$monthly %>% 
    left_join(.,BENCHMARK, by =c("date"))  
  
  return(RES)
}