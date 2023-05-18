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


feature_selection_filter <- function(df, split_prop, label_df, h ){
  
  ## split df
  label_col <- label_df %>% colnames() %>% .[2]
  label_df[,label_col] <- label_df[,label_col] %>% shift(-h)
  
  df <- df %>%
    head(round(nrow(df)*split_prop)) %>% 
    left_join(label_df, by = "date")
  df_1 <- df %>%
    head(floor(nrow(df)/2)) %>% 
    na.locf()
  df_2 <- df %>%
    tail(ceiling(nrow(df)/2)) %>% 
    na.locf()
  
  ## test linear feature importance
  r2_1 <- df_1 %>%
    select(-date) %>% 
    apply(., 2, function(x){
      
      reg_sum <- lm(df_1[[label_col]] ~ x) %>% 
        summary()
      return(reg_sum$r.squared)
    })
  
  r2_2 <- df_1 %>%
    select(-date) %>% 
    apply(., 2, function(x){
      
      reg_sum <- lm(df_1[[label_col]] ~ x) %>% 
        summary()
      return(reg_sum$r.squared)
    })
  
  ## test random forrest features
  rf_1 <- ranger::ranger(y = df_1[[label_col]], x = df_1[,colnames(df_1)!=label_col] %>% 
                           as.matrix())
  
  
}


# Impute 
# impute_values <- function(df,){
# 
#   col_type <- df %>% 
#     lapply(.,class) %>%
#     unlist()
#   col_filter <- col_type == "numeric"
#   
#   for(k in 1:ncol(df)){
#     if(col_filter[k]){
#       df[,k] <-  SMA()
#     }
#   }
# }



