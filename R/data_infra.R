# Inputs
source("R/dependencies.R")
START_DATE <- "2010-01-01"
NA_TH <- 260

#Get Metadata and Setup
meta_data <- readxl::read_xlsx(CONFIG("FILE_PATH"))


# Load data 
data <- data_loader_df(df = meta_data)

# 1. Purge features with not enough data
data <- data %>% 
  lapply(., function(df){
    prune <- df %>% group_by(symbol) %>% summarize(min_date = min(as.Date(date)) > START_DATE) %>% filter(min_date) %>% pull(symbol)
    if(length(prune) != 0){
      df <- df %>% filter(!(symbol %in% prune))
    }
    return(df)
  })

# 2. Fill Date & Fill NA
## Fill the Dates
data[[2]] <- data[[2]] %>%
  pull(symbol) %>%
  unique() %>%
  lapply(., function(x){
    res <- businessday_seq(start = START_DATE, end = Sys.Date(), ref = "SPY") %>% 
        data.frame() %>% 
        mutate(symbol=x)
    colnames(res) <- c("date", "symbol")
    return(res)
    }) %>% 
  rbindlist() %>% 
  as.data.frame() %>% 
  left_join(., data[[2]])

## Fill NA
data[[1]] <- data[[1]] %>% 
  group_by(symbol) %>% 
  arrange(date) %>% 
  fill(open, high, low, close, volume,adjusted) %>% 
  ungroup(symbol) %>% 
  na.omit()

data[[2]] <- data[[2]] %>% 
  group_by(symbol) %>% 
  arrange(date) %>% 
  fill(value) %>% 
  ungroup(symbol) %>% 
  na.omit()

# 3. Treat Outliers

# 4. Calculate Transformations and Such
## 4.1 Combine
data[[3]] <- data[[1]] %>%
  select(symbol, date, value = adjusted) %>% 
  rbind(.,data[[2]]) %>% 
  filter(date >= as.Date(START_DATE)- 600)

## 4.2. Calculate Indicators 1
data[[4]] <- data[[3]] %>% 
  pivot_wider(data=., names_from = symbol, values_from = value) %>% 
  mutate(
    Gold_Copper_Ratio = Gold_Price / Copper_Price,
    Oil_Copper_Ratio = Oil_Price / Copper_Price,
    Gold_Oil_Ratio = Gold_Price / Oil_Price
  ) %>% 
  pivot_longer(data=., names_to = "symbol",
               cols = colnames(.)[(colnames(.) != "date")])
  
  
## 4.3. Calculate Transformations
data[[4]] <- data[[4]] %>% 
  group_by(symbol) %>% 
  mutate(
    RETURN_5B = RETURN(value, 5),
    RETURN_20B = RETURN(value, 20),
    RETURN_60B = RETURN(value, 60),
    RETURN_130B = RETURN(value, 130),
    RETURN_260B = RETURN(value, 260)
  ) %>% 
  ungroup(symbol) %>% 
  #filter(symbol %in% c("Basic_Resources", "Oil_Gas")) %>% 
  #filter(symbol == "Basic_Resources") %>% 
  pivot_wider(data = ., names_from = symbol, values_from = colnames(.)[!(colnames(.) %in% c("symbol", "date"))]) %>% 
  pivot_longer(data=., names_to = "symbol", cols = colnames(.)[(colnames(.) != "date")]) %>% 
  group_by(symbol) %>% 
  arrange(date) %>% 
  mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x)) %>% 
  fill(value) %>% 
  mutate(
    
    SMA_5B = SMA(value, 5),
    SMA_20B = SMA(value, 20),
    SMA_60B = SMA(value, 60),
    SMA_130B = SMA(value, 130),
    SMA_260B = SMA(value, 260),
    
    EMA_5B = EMA(value, 5),
    EMA_20B = EMA(value, 20),
    EMA_60B = EMA(value, 60),
    EMA_130B = EMA(value, 130),
    EMA_260B = EMA(value, 260),
    
    SD_5B = sqrt(SMA(value^2, 5) - SMA_5B^2),
    SD_20B = sqrt(SMA(value^2, 20) - SMA_20B^2),
    SD_60B = sqrt(SMA(value^2, 60) - SMA_60B^2),
    SD_130B = sqrt(SMA(value^2, 130) - SMA_130B^2),
    SD_260B = sqrt(SMA(value^2, 260) - SMA_260B^2),
  ) %>% 
  mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x)) %>% 
  fill() %>%   
  mutate(
    ZScore_20B = (value - SMA_20B) / SD_20B,
    ZScore_60B = (value - SMA_60B) / SD_60B,
    ZScore_130B = (value - SMA_130B) / SD_130B,
    ZScore_260B = (value - SMA_260B) / SD_260B,

    SKEW_5B = (SMA(value^3) - 3*SMA_5B*SD_5B^2 - SMA_5B^3) / SD_5B^3,
    SKEW_20B = (SMA(value^3) - 3*SMA_20B*SD_20B^2 - SMA_20B^3) / SD_20B^3,
    SKEW_60B = (SMA(value^3) - 3*SMA_60B*SD_60B^2 - SMA_60B^3) / SD_60B^3,
    SKEW_130B = (SMA(value^3) - 3*SMA_130B*SD_130B^2 - SMA_130B^3) / SD_130B^3,
    SKEW_260B = (SMA(value^3) - 3*SMA_260B*SD_260B^2 - SMA_260B^3) / SD_260B^3,
    
  ) %>% 
  mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x)) %>% 
  fill() %>%  
  ungroup(symbol) %>% 
  pivot_wider(data = ., names_from = symbol, values_from = colnames(.)[!(colnames(.) %in% c("symbol", "date"))])

## 4.4. Calculate Indicators 2

## 4.5. Create Bins and Categories

# 5. Filter Features
## 5.1. Label unrelated filtering

### 5.1.1. To many NA
data[[4]] <- data[[4]] %>% 
  na_col_filter(df= , th=300)

### 5.1.2. Stationarity
stat_filter <- data[[4]] %>% 
  pbapply(.,2, function(x){
    x <- as.numeric(x)
    check <- sum(is.na(x)) != length(x)
    if(check){
      res <- adf.test(na.omit(x)) %>% .[["p.value"]]
      return(as.numeric(res) <= 0.01)
    } else{
      return(TRUE)
    }
  })
data[[4]] <- data[[4]][,stat_filter]


### 5.1.2. Zero Var / Near zero Var
data[[4]] %>% 
  pbapply(., 2,  function(x){
    x <- as.numeric(x)
    check <- sum(is.na(x)) != length(x)
    if(check){
      res <- sd(x, na.rm=T)
      return(as.numeric(res))
    } else{
      return(1)
    }
  })

### 5.1.3. To much NA

## 

feature_data <- data[[4]] %>% list()

save(feature_data, file = "feature_data.RData")

