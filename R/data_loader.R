data_loader <- function(KEYS, SOURCE, NAMES = NULL, LAG = NULL){
  
  if(SOURCE== "YAHOO"){
    res <- tq_get(KEYS, from = "1900-01-01")
    if(!is.null(NAMES)){
      res <- res %>% 
        left_join(., NAMES, by = c("symbol"="KEY")) %>% 
        select(-symbol) %>% 
        rename(symbol = EXPL)
    }
    
  } else if(SOURCE == "FRED"){
    
    res <- tq_get(KEYS, from = "1900-01-01", get = "economic.data") %>% 
      left_join(., NAMES, by = c("symbol"="KEY")) %>% 
      select(-symbol)  %>% 
      rename(symbol = EXPL) %>% 
      left_join(., LAG, by = c("symbol" = "EXPL")) %>% 
      mutate(date = as.Date(date) + LAG) %>% 
      select(symbol, date, value=price)
    
  } else if(SOURCE == "BB"){
    
    res <- pdfetch::pdfetch_BUNDESBANK(identifiers = KEYS) %>% 
      as.data.frame() %>% 
      mutate(date = rownames(.))
    
    if(!is.null(NAMES)){
      colnames(res) <- c(NAMES %>% pull(EXPL), "date")
    }
    
    res <- res %>% 
      pivot_longer(cols = colnames(.)[colnames(.)!="date"], names_to = "symbol")
    
    if(!is.null(LAG)){
      res <- res %>% 
        left_join(., LAG, by = c("symbol" = "EXPL")) %>% 
        mutate(date = as.Date(date) + LAG) %>% 
        select(symbol, date, value) 
    }
      
  } else if(SOURCE == "QUANDL"){
      res <- tq_get(KEYS, get = "quandl", from = "1900-01-01") %>% 
        left_join(., NAMES, by = c("symbol"="KEY")) %>% 
        select(-symbol)  %>% 
        rename(symbol = EXPL) %>% 
        left_join(., LAG, by = c("symbol" = "EXPL")) %>% 
        mutate(date = as.Date(date) + LAG) %>% 
        select(symbol, date, value)
    
    }
  return(res)
}

data_loader_df <- function(df){
  
  
  ## HLOC 
  res_1 <-df %>%
    filter(HLOC=="TRUE") %>% 
    pull(SOURCE) %>% 
    unique() %>% 
    lapply(., function(x){
      temp <- df %>% filter(SOURCE == x)
      return(data_loader(KEYS = temp %>% pull(KEY),
                         SOURCE = x,
                         NAMES = temp %>% select(KEY, EXPL),
                         LAG = temp %>% select(EXPL, LAG)))
    }) %>% 
    rbindlist()
  ## Non HLOC
  res_2 <-df %>%
    filter(HLOC!="TRUE") %>%  
    pull(SOURCE) %>% 
    unique() %>% 
    lapply(., function(x){
      temp <- df %>% filter(SOURCE == x)
      return(data_loader(KEYS = temp %>% pull(KEY),
                         SOURCE = x,
                         NAMES = temp %>% select(KEY, EXPL),
                         LAG = temp %>% select(EXPL, LAG)))
    }) %>% 
    rbindlist()

  ## return stuff
  res <- list(
  )
  res[[1]] <- res_1
  res[[2]] <- res_2
  
  return(res) 
  
  
}
