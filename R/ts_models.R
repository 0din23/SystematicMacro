ts_model <- function(df_decom, h, label_col, split_prop, features){
  
  
  features <- feature_data %>% 
    left_join(., df_decom %>% select(-c(alpha)), by = "date")
  
  features_filter <- linear_filter(df = features,
                                   split_prop = split_prop,
                                   label_df = df_decom %>% select(date, alpha),
                                   h = h , r2_th = 0.035, cor_th = 0.8,
                                   max_features = 25)
  
  filtered_features <- features %>% select(all_of(features_filter))
  filtered_decom <- df_decom %>% select(date, alpha) %>% filter(date %in% filtered_features$date)
  filtered_features <- filtered_features %>% filter(date %in% filtered_decom$date)

  
  # General Preprocessing
  ## output list
  EpsilonUtility::print_timed("Initial Processing")
  res <- list()
  
  ## Combine datasets
  df_decom$label <- df_decom[, label_col] %>% shift(., -h)
  model_data <- df_decom %>% 
    na.omit() %>% 
    left_join(., features, by = "date")
  
  ## filter na
  na_filter <- apply(model_data, 2,function(x){sum(is.na(x)) == 0})
  na_filter[names(na_filter) == "label"] <-TRUE
  model_data <- model_data[,na_filter]
  
  ## Splits 
  train <- model_data %>% head(round(0.7*nrow(df_decom))) 
  test <- model_data %>% filter(!(date %in% train$date))
  
  # Fit ARIMA Models
  wo_xreg <- auto.arima(y = train$label, d = 1,
                        start.p = 5, max.p = 35,
                        start.q = 5, max.q = 20,
                        trace = TRUE)
  
  with_xreg <- auto.arima(y = train$label, d = 1,
                          start.p = 5, max.p = 35,
                          start.q = 5, max.q = 20,
                          trace = TRUE,
                          xreg = train %>% select(return, ß_Mkt, Mkt, epsilon, alpha) %>% as.matrix())
  
  evaluation <- data.frame(
    "alpha"=test$label,
    "with_reg" = forecast(object=with_xreg,
                          h=test %>% nrow(),
                          xreg = test %>%
                            select(return, ß_Mkt, Mkt, epsilon, alpha) %>% 
                            as.matrix())[4] %>% unlist(),
    "wo_reg" = forecast(object=wo_xreg,
                        h=test %>%nrow())[4] %>% unlist()
    
  )
  
  evaluation %>% na.omit() %>% cor()
  
  evaluation %>%
    mutate(index = c(1:nrow(.))) %>% 
    pivot_longer(cols = colnames(.)[colnames(.) != "index"]) %>% 
    ggplot(.) +
    geom_line(aes(x = index, y=value, color = name))
  
  
  predictions <- test_1$model_test %>%
    select(glm_prediction_best,alpha)
  
  regressionEvaluation(label = test_1$model_test$label, predictions = predictions) 
  
  ## Evaluate Classification Quality
  predictions <- test_1$model_test %>%
    select(glm_prediction_best, alpha) %>% 
    apply(.,2, function(x){ifelse(x>0,1,0)}) %>% 
    as.data.frame() %>% 
    mutate_all(as.factor)
  
  classificationEvaluation(label = ifelse(test_1$model_test$label>0,1,0) %>% as.factor(),  classification_prediction = predictions)
  
}
