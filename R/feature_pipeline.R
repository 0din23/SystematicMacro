feature_pipe <- function(){
  # assets
  asset_ticker <- openxlsx::read.xlsx(
    "C:/0cap_2/MacroMan/SectorRotationPaper/Data_Catalogue.xlsx") %>% 
    filter(QUELLE == "YAHOO") %>% 
    pull(KEY)
  
  df_assets <- asset_ticker %>% 
    tq_get(., from = "2007-01-01") %>% 
    na.omit() %>% 
    group_by(symbol) %>% 
    mutate(
      
      pct_change_1B = RETURN(adjusted, 1), 
      pct_change_5B = RETURN(adjusted, 5),
      pct_change_20B = RETURN(adjusted, 20),
      pct_change_60B = RETURN(adjusted, 60),
      pct_change_130B = RETURN(adjusted, 130),
      pct_change_260B = RETURN(adjusted, 260),
      
      range=(high-low)/close,
    ) %>% 
    mutate(
      "ATR_5B" = SMA(range, 5),
      "ATR_20B" = SMA(range, 20),
    ) %>% 
    mutate(
      volume_ret_5B = RETURN(volume, 5),
      volume_ret_20B = RETURN(volume, 20),
      volume_ret_60B = RETURN(volume, 60),
    ) %>% 
    mutate(
      SMA_5B = ifelse(SMA(adjusted, 5) < adjusted, 1,0) %>% as.factor(),
      SMA_20B = ifelse(SMA(adjusted, 20) < adjusted, 1,0) %>% as.factor(),
      SMA_60B = ifelse(SMA(adjusted, 60) < adjusted, 1,0) %>% as.factor(),
      SMA_130B = ifelse(SMA(adjusted, 130) < adjusted, 1,0) %>% as.factor(),
      
      SMA_5_20 = ifelse(SMA(adjusted, 20) < SMA(adjusted, 5), 1,0) %>% as.factor(),
      SMA_20_60 = ifelse(SMA(adjusted, 60) <SMA(adjusted, 20), 1,0) %>% as.factor(),
      SMA_20_130 = ifelse(SMA(adjusted, 130) <SMA(adjusted, 20), 1,0) %>% as.factor(),
      SMA_60_130 = ifelse(SMA(adjusted, 130) <SMA(adjusted, 60), 1,0) %>% as.factor()
    ) %>% 
    ungroup(symbol) %>% 
    select(-c(open, high, low, close, volume)) %>% 
    filter(!is.na(adjusted)) %>% 
    pivot_wider(data=., names_from = symbol,
                values_from = colnames(.)[!(colnames(.) %in% c("symbol", "date"))]) %>% 
    filter(!is.na(adjusted_USO)) 
  
  # Calculate ETF Liquidity
  df_liquidity <- tq_get(c("^STOXX", "EXSA.DE"), from = "1900-01-01") %>% 
    mutate(return = RETURN(adjusted)) %>% 
    select(date, symbol, return) %>% 
    pivot_wider(data=., names_from=symbol, values_from =return)
  
  ## Calculate Dispersion Metrics
  ticker <- c("EXV6.DE", "EXH1.DE", "EXV1.DE", "EXV4.DE", "EXV5.DE",
              "EXH4.DE", "EXV3.DE", "EXV8.DE", "EXH8.DE", "EXH3.DE",
              "EXV2.DE", "EXH7.DE", "EXSA.DE")
  ## Dispersion metrics --------------------------------------------------------
  all_df <- ticker %>% 
    lapply(., function(x){
      tq_get(x, from="2008-01-01") %>% 
        mutate(
          "return"=RETURN(adjusted)
        ) %>% 
        select(date, symbol, return)
    }) %>% rbindlist() %>% as.data.frame() %>% 
    pivot_wider(data = ., names_from = symbol, values_from = return) %>% 
    na_last_fill()
  colnames(all_df) <- c("date", "Basic_Resources", "Oil_Gas", "Banks", "Health_Care",
                        "Automobile","Industrials", "Technology", "Construction",
                        "Utilities", "Food","Telecommunication", "Personal_Goods",
                        "STOXX_600")
  
  
  ## Percentage of up vs down returns
  dispersion_df <- data.frame("date"=all_df$date)
  
  dispersion_df$up_ratio <- all_df %>% select(-date, -STOXX_600) %>%
    apply(.,1,function(x){length(x[x>=0])/length(x)})
  
  ## average active return
  dispersion_df$avg_activeRet <- all_df %>% select(-date, -STOXX_600) %>% 
    apply(.,2,function(x){x-all_df$STOXX_600}) %>% 
    abs() %>% 
    rowMeans(., na.rm=T)
  
  ## beta dispersion
  dispersion_df$beta_dispersion <- all_df %>% select(-date, -STOXX_600) %>% 
    apply(.,2,function(x){
      temp <- data.frame(
        "X"=x,
        "bench"=all_df$STOXX_600
      ) %>% na.omit()
      
      reg <-roll_regres(X~bench, data=temp, do_downdates = T, width = 60)
      reg$coefs[,2]
      
    }) %>% 
    apply(.,1, sd, na.rm=T) %>% 
    c(NA,.)
  
  ## alpha dispersion
  dispersion_df$alpha_dispersion <- all_df %>% select(-date, -STOXX_600) %>% 
    apply(.,2,function(x){
      temp <- data.frame(
        "X"=x,
        "bench"=all_df$STOXX_600
      ) %>% na.omit()
      
      reg <-roll_regres(X~bench, data=temp, do_downdates = T, width = 60)
      reg$coefs[,1]
      
    }) %>% 
    apply(.,1, sd, na.rm=T) %>% 
    c(NA,.)
  
  ## correlation dispersion
  dispersion_df$Correlation_dispersion <- all_df %>% select(-date, -STOXX_600) %>% 
    rollapply(data=., FUN=function(x){
      mean(1-cor(x))
    },width = 60, fill = NA, by.column = FALSE, align = "right")
  
  
  
  # ECON DATA ##################################################################
  # assets
  econ_ticker <- openxlsx::read.xlsx(
    "C:/0cap_2/MacroMan/SectorRotationPaper/Data_Catalogue.xlsx") %>% 
    filter(QUELLE == "FRED") %>% 
    filter(FX == "TRUE") %>% 
    select(KEY, EXPL, LAG)
  
  econ_df <- econ_ticker %>% 
    pull(KEY) %>% 
    tq_get(., get = "economic.data", from = "1900-01-01") %>% 
    left_join(., econ_ticker, by = c("symbol" = "KEY")) %>% 
    mutate(date = date + LAG) %>% 
    select(symbol = EXPL, date, price) 
  
  merger <- econ_df %>% pull(symbol) %>% unique() %>% lapply(., function(x){df_assets %>% select(date) %>% mutate(symbol=x)}) %>% rbindlist() %>% as.data.frame()
  
  econ_df <- econ_df %>% 
    left_join(merger,.)
  
  econ_df <- econ_df %>%
    pull(symbol) %>% 
    unique() %>% 
    lapply(., function(x){
      econ_df %>%
        filter(symbol == x) %>% 
        arrange(date) %>% 
        na.locf() %>% 
        mutate(
          GROWTH_60B = RETURN(price, 60),
          GROWTH_260B = RETURN(price, 260),
        ) %>% 
        na.omit() %>% 
        group_by(symbol) %>% 
        arrange(date) %>% 
        mutate(
          GROWTH_60B_SMA60 = SMA(GROWTH_60B, 60),
          GROWTH_60B_SMA130 = SMA(GROWTH_60B, 130),
          GROWTH_60B_SMA260 = SMA(GROWTH_60B, 260),
          
          GROWTH_260B_SMA60 = SMA(GROWTH_260B, 60),
          GROWTH_260B_SMA130 = SMA(GROWTH_260B, 130),
          GROWTH_260B_SMA260 = SMA(GROWTH_260B, 260),
          
        ) %>% 
        ungroup(symbol)
    }) %>%
    rbindlist() %>% 
    as.data.frame() %>% 
    pivot_wider(., values_from = colnames(.)[-c(1:2)],
                names_from = symbol)
  
  
  
  # Combine
  feature_data <- df_assets %>%
    left_join(.,dispersion_df, by = "date") %>%
    left_join(.,df_liquidity, by = "date") %>% 
    left_join(., econ_df, by = "date")
  
  return(feature_data)
  
}





# Feature Pipe for FX Model ----------------------------------------------------
feature_pipe_FX <- function(){
  
  ## Get Data
  # assets
  asset_ticker <- openxlsx::read.xlsx(
    "C:/0cap_2/MacroMan/SectorRotationPaper/Data_Catalogue.xlsx") %>% 
    filter(QUELLE == "YAHOO") %>% 
    filter(FX == "TRUE") %>% 
    select(KEY, EXPL)
  
  data <- asset_ticker %>% 
    pull(KEY) %>% 
    tidyquant::tq_get(., from = "1900-01-01") %>% 
    left_join(., asset_ticker, by = c("symbol"="KEY")) %>%
    na.locf() %>% 
    rename(names = EXPL)
  
  ## Calculate a shit ton of TA features
  data <- data %>%
    select(-symbol) %>% 
    group_by(names) %>% 
    arrange(date) %>% 
    
    ### Return Based Features
    mutate(
      pct_change_1B = RETURN(adjusted, 1), 
      pct_change_5B = RETURN(adjusted, 5),
      pct_change_20B = RETURN(adjusted, 20),
      pct_change_60B = RETURN(adjusted, 60),
      pct_change_130B = RETURN(adjusted, 130),
      pct_change_260B = RETURN(adjusted, 260),
    )  %>% 
    
    ### Rolling Volatility
    mutate(
      vol_5B = c(rep(NA, 4),rollapplyr(pct_change_1B, sd, align = "right", width = 5)),
      vol_20B = c(rep(NA, 19),rollapplyr(pct_change_1B, sd, align = "right", width = 20)),
      vol_60B = c(rep(NA, 59),rollapplyr(pct_change_1B, sd, align = "right", width = 60)),
      vol_130B = c(rep(NA, 129),rollapplyr(pct_change_1B, sd, align = "right", width = 130)),
      vol_260B = c(rep(NA, 259),rollapplyr(pct_change_1B, sd, align = "right", width = 260)),
      
      vol_5B_20B = vol_5B - vol_20B,
      vol_5B_60B = vol_5B - vol_60B,
      vol_5B_130B = vol_5B - vol_130B,
      vol_20B_60B = vol_20B - vol_60B,
      vol_20B_130B = vol_20B - vol_130B,
      vol_60B_130B = vol_60B - vol_130B,
      vol_60B_260B = vol_60B - vol_260B,
    ) %>% 
    
    ### Moving Averages
    mutate(
      
      SMA_5B = SMA(adjusted, 5),
      SMA_20B = SMA(adjusted, 20),
      SMA_60B = SMA(adjusted, 60),
      SMA_130B = SMA(adjusted, 130),
      SMA_260B = SMA(adjusted, 260),
      
      SMA_5B_20B = (SMA_5B / SMA_20B)-1,
      SMA_5B_60B = (SMA_5B / SMA_60B)-1,
      SMA_5B_130B = (SMA_5B / SMA_130B)-1,
      SMA_20B_60B = (SMA_20B / SMA_60B)-1,
      SMA_20B_130B = (SMA_20B / SMA_130B)-1,
      SMA_60B_130B = (SMA_60B / SMA_130B)-1,
      SMA_60B_260B = (SMA_60B / SMA_260B)-1,
    ) %>% 
    ungroup(names) %>% 
    select(-c(high, open, low, close, volume, adjusted)) %>% 
    pivot_wider(data=., names_from = names,
                values_from = colnames(.)[!(colnames(.) %in% c("date", "names"))]) %>% 
    arrange(date) %>% 
    
    ## Equity Market Differences
    mutate(
      "US_EU_DIFF_20B" = pct_change_20B_SPY - pct_change_20B_EUROSTOXX,
      "US_EU_DIFF_60B" = pct_change_60B_SPY - pct_change_60B_EUROSTOXX,
      "US_EU_DIFF_130B" = pct_change_130B_SPY - pct_change_130B_EUROSTOXX,
      "US_EU_DIFF_260B" = pct_change_260B_SPY - pct_change_260B_EUROSTOXX,
      
      "AUS_EU_DIFF_20B" = pct_change_20B_AUS_Stocks - pct_change_20B_EUROSTOXX,
      "AUS_EU_DIFF_60B" = pct_change_60B_AUS_Stocks - pct_change_60B_EUROSTOXX,
      "AUS_EU_DIFF_130B" = pct_change_130B_AUS_Stocks - pct_change_130B_EUROSTOXX,
      "AUS_EU_DIFF_260B" = pct_change_260B_AUS_Stocks - pct_change_260B_EUROSTOXX,
      
      "NZD_EU_DIFF_20B" = pct_change_20B_NZD_Stocks - pct_change_20B_EUROSTOXX,
      "NZD_EU_DIFF_60B" = pct_change_60B_NZD_Stocks - pct_change_60B_EUROSTOXX,
      "NZD_EU_DIFF_130B" = pct_change_130B_NZD_Stocks - pct_change_130B_EUROSTOXX,
      "NZD_EU_DIFF_260B" = pct_change_260B_NZD_Stocks - pct_change_260B_EUROSTOXX,
      
      "CAD_EU_DIFF_20B" = pct_change_20B_CAD_Stocks - pct_change_20B_EUROSTOXX,
      "CAD_EU_DIFF_60B" = pct_change_60B_CAD_Stocks - pct_change_60B_EUROSTOXX,
      "CAD_EU_DIFF_130B" = pct_change_130B_CAD_Stocks - pct_change_130B_EUROSTOXX,
      "CAD_EU_DIFF_260B" = pct_change_260B_CAD_Stocks - pct_change_260B_EUROSTOXX,
      
      "FTSE_EU_DIFF_20B" = pct_change_20B_Foooootsiee_100 - pct_change_20B_EUROSTOXX,
      "FTSE_EU_DIFF_60B" = pct_change_60B_Foooootsiee_100 - pct_change_60B_EUROSTOXX,
      "FTSE_EU_DIFF_130B" = pct_change_130B_Foooootsiee_100 - pct_change_130B_EUROSTOXX,
      "FTSE_EU_DIFF_260B" = pct_change_260B_Foooootsiee_100 - pct_change_260B_EUROSTOXX
    ) %>% 
    
    ### Rates Stuff
    mutate(
      US_rates_5_10 = SMA_5B_130B_treasury_5y - SMA_5B_130B_treasury_10y,
      US_rates_10_30 = SMA_5B_130B_treasury_10y - SMA_5B_130B_treasury_30y
    ) %>% 
    na.locf()
  
  # ECON DATA ##################################################################
  # assets
  econ_ticker <- openxlsx::read.xlsx(
    "C:/0cap_2/MacroMan/SectorRotationPaper/Data_Catalogue.xlsx") %>% 
    filter(QUELLE == "FRED") %>% 
    filter(FX == "TRUE") %>% 
    select(KEY, EXPL, LAG)
  
  econ_df <- econ_ticker %>% 
    pull(KEY) %>% 
    tq_get(., get = "economic.data", from = "1900-01-01") %>% 
    left_join(., econ_ticker, by = c("symbol" = "KEY")) %>% 
    mutate(date = date + LAG) %>% 
    select(symbol = EXPL, date, price) 
  
  merger <- econ_df %>% pull(symbol) %>% unique() %>% lapply(., function(x){data %>% select(date) %>% mutate(symbol=x)}) %>% rbindlist() %>% as.data.frame()
  
  econ_df <- econ_df %>% 
    left_join(merger,.)
  
  econ_df <- econ_df %>%
    pull(symbol) %>% 
    unique() %>% 
    lapply(., function(x){
      econ_df %>%
        filter(symbol == x) %>% 
        arrange(date) %>% 
        na.locf() %>% 
        mutate(
          GROWTH_60B = RETURN(price, 60),
          GROWTH_260B = RETURN(price, 260),
        ) %>% 
        na.omit() %>% 
        group_by(symbol) %>% 
        arrange(date) %>% 
        mutate(
          GROWTH_60B_SMA60 = SMA(GROWTH_60B, 60),
          GROWTH_60B_SMA130 = SMA(GROWTH_60B, 130),
          GROWTH_60B_SMA260 = SMA(GROWTH_60B, 260),
          
          GROWTH_260B_SMA60 = SMA(GROWTH_260B, 60),
          GROWTH_260B_SMA130 = SMA(GROWTH_260B, 130),
          GROWTH_260B_SMA260 = SMA(GROWTH_260B, 260),
          
        ) %>% 
        ungroup(symbol)
    }) %>%
    rbindlist() %>% 
    as.data.frame() %>% 
    pivot_wider(., values_from = colnames(.)[-c(1:2)],
                names_from = symbol)
  
  # Combine everything
  data <- data %>% 
    left_join(., econ_df, by = "date")
  
  return(data)
  
}