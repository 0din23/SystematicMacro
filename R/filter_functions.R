# ADF Filter
ADF_Filter <- function(df, th, exclude){
  adf_filter <- c(1:ncol(df)) %>% 
    lapply(., function(x){
      if((class(df[[x]]) == "numeric" & length(na.omit(df[[x]])) != 0)){
        catcher <- tryCatch({
          tmp_test <- adf.test(df[[x]] %>% na.omit())
          return(FALSE)
        }, error = function(cond){
          return(TRUE)
        })
      } else {
        return(0)
      }
      if(catcher){
        return(tmp_test$p.value)
      } else{
        return(1)
      }
    }) %>% 
    unlist()
  
  adf_filter[colnames(df) %in% exclude] <- 0
  
  return(df[,adf_filter < th])
}

# To many NA filter
na_col_filter <- function(df, th){
  na_sum <- df %>% apply(.,2,function(x) sum(is.na(x)))
  df[,na_sum <=th]
}

# Outlier Clipping
Clipping_Filter <- function(df, th =c(0.01, 0.99), exclude = c()){
  
  col_type <- df %>% 
    lapply(.,class) %>%
    unlist()
  
  col_filter <- col_type == "numeric"
  col_filter <- col_filter & !(colnames(df) %in% exclude)  
  for(k in 1:ncol(df)){
    if(col_filter[k]){
      df[,k] <-  DescTools::Winsorize(df[,k], probs =c(0.01, 0.99), na.rm = T)
    }
  }
  return(df)
}


feature_selection_filter <- function(df, split_prop, label_df, h, reg_q = 0.05, rf_q = 0.05){
  
  ## split df
  label_col <- label_df %>% colnames() %>% .[2]
  label_df[,label_col] <- label_df[,label_col] %>% shift(-h)
  
  df <- df %>%
    head(round(nrow(df)*split_prop)) %>% 
    left_join(label_df, by = "date")
  df <- df[!is.na(df[,label_col]),]
  
  df_1 <- df %>%
    head(floor(nrow(df)/2)) %>% 
    na_locf()
  df_2 <- df %>%
    tail(ceiling(nrow(df)/2)) %>% 
    na_locf()
  
  ## test linear feature importance
  r2_1 <- df_1 %>%
    select(-date) %>% 
    apply(., 2, function(x){
      reg_sum <- lm(df_1[[label_col]] ~ x) %>% 
        summary()
      return(reg_sum$r.squared)
    })
  
  r2_2 <- df_2 %>%
    select(-date) %>% 
    apply(., 2, function(x){
      reg_sum <- lm(df_2[[label_col]] ~ x) %>% 
        summary()
      return(reg_sum$r.squared)
    })
  
  res_lin <- sort((r2_1 + r2_2), decreasing = T) %>% tail(round(length(r2_1) * (1-reg_q))) %>% names()
  
  ## test random forrest features
  col_filter <- c(label_col, "date")
  cut <- ncol(df_1[,!(colnames(df_1) %in% col_filter)]) * (1-rf_q)
  cut <- floor(cut / 5)
  for(k in 1:5){
    rf_1 <- ranger::ranger(y = df_1[[label_col]], x = df_1[,!(colnames(df_1) %in% col_filter)] %>% 
                             as.matrix(), importance = "impurity")
    
    rf_2 <- ranger::ranger(y = df_2[[label_col]], x = df_2[,!(colnames(df_1) %in% col_filter)] %>% 
                             as.matrix(), importance = "impurity")
    
    
    col_filter <- col_filter %>% 
      append(.,
             append(
               sort(rf_1$variable.importance, decreasing = T) %>%
                 tail(cut) %>%
                 names(),
               sort(rf_2$variable.importance, decreasing = T) %>%
                 names()
               ) %>% 
               unique() %>%
               sample(x=., size = cut)
             ) %>%
      unique() 
  }
  
  ## Combine
  res_lin <- res_lin[res_lin %in% col_filter]
  col_filter <- col_filter[col_filter %in% res_lin]
  col_filter <- c(col_filter, label_col)
  df[,c(colnames(df)[!(colnames(df) %in% col_filter)])] %>% colnames()
}

linear_filter <- function(df, split_prop, label_df, h, r2_th = 0.01, cor_th = 0.8,
                          max_features = 50){
  
  ## split df and preprocess ---------------------------------------------------
  label_col <- label_df %>% colnames() %>% .[2]
  label_df[,label_col] <- label_df[,label_col] %>% shift(-h)
  
  
  df <- df %>%
    head(round(nrow(df)*split_prop)) %>% 
    left_join(label_df, by = "date")
  df <- df[!is.na(df[,label_col]),]
  
  df_1 <- df %>%
    head(floor(nrow(df)/2)) %>% 
    na_locf()
  df_2 <- df %>%
    tail(ceiling(nrow(df)/2)) %>% 
    na_locf()
  
  ## Check for coefficient flipping --------------------------------------------
  print("Check for Coefficient Flips")
  coef_1 <- df_1 %>%
    select(-date) %>% 
    #select(1:20) %>% 
    pbapply(., 2, function(x){
      reg <- lm(df_1[[label_col]] ~ x)
      return(reg$coefficients[2])
    })
  
  coef_2 <- df_2 %>%
    select(-date) %>% 
    pbapply(., 2, function(x){
      reg <- lm(df_2[[label_col]] ~ x)
      return(reg$coefficients[2])
  })
  coef_1 <- ifelse(coef_1 > 0, 1, 0)
  coef_2 <- ifelse(coef_2 > 0, 1, 0)
  
  coef_flip <- coef_1 == coef_2
  coef_flip <- c(T,coef_flip)
  df <- df[,coef_flip]
  
  ## Predictive filtering ------------------------------------------------------
  print("Check for predictiction ability")
  r2 <- df %>%
    select(-date) %>% 
    pbapply(., 2, function(x){
      reg_sum <- lm(df[[label_col]] ~ x) %>% 
        summary()
      return(reg_sum$r.squared)
    })
  r2_filter <- r2 >= r2_th
  r2_filter <- c(T, r2_filter)
  df <- df[,r2_filter]
  
  ## Intercorrelation Filtering ------------------------------------------------
  print("Check for Colinearity")
  ### Calc CoMa
  CoMa <- df %>% 
    select(-date) %>% 
    cor() %>% 
    as.data.frame()
  
  ### Calc R2
  r2 <- df %>%
    select(-date) %>% 
    pbapply(., 2, function(x){
      reg_sum <- lm(df[[label_col]] ~ x) %>% 
        summary()
      return(reg_sum$r.squared)
    }) %>%
    sort(., decreasing = T)
  
  ### filter
  check <- TRUE
  counter <- 2
  while(check){
    
    #### get correlated features 
    feature_name <- r2[counter] %>% names()
    feature_name <- feature_name[feature_name %in% colnames(CoMa)]
    intercor_filter <- CoMa %>%
      select(all_of(feature_name)) %>% 
      filter(abs(get(!!feature_name)) >= cor_th & get(!!feature_name) != 1) %>% 
      rownames()
    
    #### delete them
    r2 <- r2[!(names(r2) %in% intercor_filter)]
    CoMa <- CoMa[!(rownames(CoMa) %in% intercor_filter), !(colnames(CoMa) %in% intercor_filter)]
    
    #### Check counter 
    counter <- counter + 1
    if(counter == length(r2)){check <- F}
  }
  
  ### Cull them
  r2_filter <- r2 %>%
    sort(., decreasing = T) %>% 
    head(max_features) %>%
    names()
  r2_filter <- c("date", r2_filter)
  r2_filter <- colnames(df) %in% r2_filter
  df <- df[,r2_filter]
  
  df %>%
    select(-all_of(label_col)) %>% 
    colnames() %>%
    return()
}




