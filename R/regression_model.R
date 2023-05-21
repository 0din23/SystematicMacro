ml_model <- function(df_decom, h, label_col, split_prop, features, reg_eval = "rsq"){
  
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
  
  ## assessing cores
  cores <- parallel::detectCores()
  
  ## Splits 
  splits <- initial_time_split(model_data, prop = split_prop)
  model_train <- training(splits)
  model_test  <- testing(splits)
  val_set <- validation_time_split(model_train, prop = split_prop)
  
  ##############################################################################
  # Linear Model Regression #
  ##############################################################################
  
  EpsilonUtility::print_timed("GLM")
  
  ## construct recipe ------------------------------------------------------------
  res$glm_recipe <- recipe(label ~ ., data = model_train) %>% 
    #step_date(date) %>% 
    step_rm(date) %>% 
    step_zv(all_predictors()) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    
    step_dummy(all_nominal_predictors()) %>% 
    step_lincomb(all_numeric_predictors()) %>% 
    
    step_corr(all_numeric_predictors(),
              threshold = 0.85) %>% 
    prep(training = model_train, retain = TRUE)
  
  ## model
  res$glm_mod <- linear_reg(penalty = tune(), mixture = tune()) %>% 
    set_engine("glmnet") %>% translate()
  
  ## Workflow
  res$glm_workflow <- workflow() %>% 
    add_model(res$glm_mod) %>% 
    add_recipe(res$glm_recipe)
  
  ## Cross Validation ------------------------------------------------------------
  res$glm_grid <- dials::parameters(
    penalty(), mixture()) %>% 
    grid_max_entropy(., size = 30)
  
  ## Train and Tune
  res$glm_res <- res$glm_workflow %>% 
    tune_grid(val_set,
              grid = res$glm_grid,
              control = control_grid(save_pred = TRUE))
  
  res$glm_best <- res$glm_res %>% 
    collect_metrics() %>%
    filter(.metric == reg_eval) %>%
    filter(!is.na(mean)) %>% 
    filter(mean == max(mean))
  
  ## fit models ------------------------------------------------------------------
  res$glm_best_model <- linear_reg(penalty = res$glm_best$penalty,
                                   mixture = res$glm_best$mixture) %>% 
    set_engine("glmnet")
  
  res$glm_workflow <- res$glm_workflow %>% 
    update_model(res$glm_best_model)
  
  res$glm_fit_best <- res$glm_workflow %>% 
    last_fit(splits)
  
  ## Fit Production Model --------------------------------------------------------
  res$glm_final <- fit(res$glm_workflow, model_data %>% head(nrow(.)-h))
  
  ##############################################################################
  # Random Forrest Regression #
  ##############################################################################
  
  EpsilonUtility::print_timed("RF")
  
  ## recipe
  res$rf_recipe <- recipe(label ~ ., data = model_train) %>% 
    #step_date(date) %>% 
    step_rm(date) %>% 
    step_nzv(all_predictors()) %>% 
    
    step_lincomb(all_numeric_predictors()) %>% 
    step_corr(all_numeric_predictors(),
              threshold = 0.85) %>% 
    
    prep(training = model_train, retain = TRUE)
  
  ## model
  res$rf_model <- rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
    set_engine("ranger",num.threads = cores) %>% 
    set_mode("regression") 
  
  ## Workflow
  res$rf_workflow <- workflow() %>% 
    add_model(res$rf_model) %>% 
    add_recipe(res$rf_recipe)
  
  ## Cross Validation ------------------------------------------------------------
  res$rf_grid <- tibble(mtry = seq(1,30, length.out = 30),
                        trees = seq(500, 1000, length.out = 30),
                        min_n = seq(2,31  ,length.out = 30))
  
  ## Train and Tune
  res$rf_res <- res$rf_workflow %>% 
    tune_grid(val_set,
              grid = res$rf_grid,
              control = control_grid(save_pred = TRUE))
  
  res$rf_best <- res$rf_res %>% 
    collect_metrics() %>%
    filter(.metric == reg_eval) %>%
    filter(!is.na(mean)) %>% 
    filter(mean == max(mean))
  
  ## fit models ------------------------------------------------------------------
  res$rf_best_model <- rand_forest(mtry = res$rf_best$mtry,
                                   min_n = res$rf_best$min_n,
                                   trees = res$rf_best$trees) %>% 
    set_engine("ranger", num.threads = cores) %>% 
    set_mode("regression") 
  
  res$rf_workflow <- res$rf_workflow %>% 
    update_model(res$rf_best_model)
  
  res$rf_fit_best <- res$rf_workflow  %>% 
    last_fit(splits)
  
  ## Fit Production Model --------------------------------------------------------
  res$rf_final <- fit(res$rf_workflow, model_data %>% head(nrow(.)-h))
  
  ##############################################################################
  # XGB Regression #
  ##############################################################################
  
  EpsilonUtility::print_timed("XGB")
  
  ## recipe
  res$xgb_recipe <- recipe(label ~ ., data = model_train) %>%
    #step_date(date) %>% 
    step_rm(date) %>% 
    
    step_nzv(all_predictors()) %>% 
    
    step_string2factor(all_nominal()) %>%
    step_dummy(all_nominal()) %>% 
    
    step_lincomb(all_numeric_predictors()) %>% 
    step_corr(all_numeric_predictors(),
              threshold = 0.85) 
  
  ## model
  res$xgb_model <- boost_tree(mode = "regression", trees = tune(),
                              min_n = tune(),tree_depth = tune(),
                              learn_rate = tune(),
                              loss_reduction = tune()) %>%
    set_engine("xgboost", num.threads = cores)
  
  ## Workflow
  res$xgb_workflow <- workflow() %>% 
    add_model(res$xgb_model) %>% 
    add_recipe(res$xgb_recipe)
  
  ## Cross Validation ------------------------------------------------------------
  res$xgb_grid <- dials::parameters(
    min_n(), tree_depth(), learn_rate(),
    loss_reduction(), trees()) %>% 
    dials::grid_max_entropy(., size = 50)
  
  doParallel::registerDoParallel()
  res$xgb_res <- res$xgb_workflow  %>% 
    tune_grid(
      val_set,
      grid = res$xgb_grid,
      control = control_grid(save_pred = TRUE))
  
  res$xgb_best <- res$xgb_res %>%
    collect_metrics() %>%
    filter(.metric == reg_eval) %>%
    filter(!is.na(mean)) %>%
    filter(mean == max(mean)) %>% 
    .[1,]
  
  ## fit models ------------------------------------------------------------------
  res$xgb_best_model <- boost_tree(mode = "regression",
                                   trees = res$xgb_best$trees,
                                   min_n = res$xgb_best$min_n,
                                   tree_depth = res$xgb_best$tree_depth,
                                   learn_rate = res$xgb_best$learn_rate,
                                   loss_reduction = res$xgb_best$loss_reduction)%>%
    set_engine("xgboost")
  
  res$xgb_workflow <- res$xgb_workflow %>% 
    update_model(res$xgb_best_model)
  
  res$xgb_fit_best <- res$xgb_workflow %>% 
    last_fit(splits)
  
  ## Fit Production Model --------------------------------------------------------
  res$xgb_final <- fit(res$xgb_workflow, model_data %>% head(nrow(.)-h))
  
  ##############################################################################
  # Fit Values #
  ##############################################################################
  ## Regression ----------------------------------------------------------------
  ### GLM Regression
  model_test$glm_prediction_best <- res$glm_fit_best %>% 
    collect_predictions() %>% 
    pull(.pred)
  model_data$glm_prediction_final <- predict(res$glm_final, model_data) %>%
    pull(.pred)
  
  ### RF Regression
  model_test$rf_prediction_best <- res$rf_fit_best %>%
    collect_predictions() %>%
    pull(.pred)
  model_data$rf_prediction_final <- predict(res$rf_final, model_data) %>%
    pull(.pred)
  
  ### XGB Regression 
  model_test$xgb_prediction_best <- res$xgb_fit_best %>%
    collect_predictions() %>% 
    pull(.pred)
  model_data$xgb_prediction_final <- predict(res$xgb_final, model_data) %>%
    pull(.pred)
  
  ## Mean Ensemble -------------------------------------------------------------
  ### for testing
  model_test$simple_mean_ensemble <-(model_test$glm_prediction_best +
                                       model_test$rf_prediction_best + model_test$xgb_prediction_best)/3
  
  ### for prediction
  model_data$simple_mean_ensemble <- (model_data$glm_prediction_final +
                                        model_data$rf_prediction_final + model_data$xgb_prediction_final)/3
  
  ## Similarity Ensemble -------------------------------------------------------
  ### for testing
  model_test$similarity_ensemble <- model_test %>% 
    select(glm_prediction_best, rf_prediction_best, xgb_prediction_best) %>% 
    apply(.,1,function(x){
      temp_distance <- x %>% lapply(.,function(k){ mean(abs(x - k)) }) %>% unlist()
      x[temp_distance != max(temp_distance)] %>% mean() %>%  return()
    }) %>% unlist()
  
  ### for prediction
  model_data$similarity_ensemble <- model_data %>% 
    select(glm_prediction_final, rf_prediction_final, xgb_prediction_final) %>% 
    apply(.,1,function(x){
      temp_distance <- x %>% lapply(.,function(k){mean(abs(x - k))}) %>% unlist()
      x[temp_distance != max(temp_distance)] %>% mean() %>%  return()
    }) %>%unlist()
  
  ## Divergence Ensemble -------------------------------------------------------
  ### for testing
  model_test$divergence_ensemble <- model_test %>% 
    select(glm_prediction_best, rf_prediction_best, xgb_prediction_best) %>% 
    apply(.,1,function(x){
      temp_distance <- x %>% lapply(.,function(k){ mean(abs(x - k)) }) %>% unlist()
      x[temp_distance != min(temp_distance)] %>% mean() %>%  return()
    }) %>% unlist()
  
  ### for prediction
  model_data$divergence_ensemble <- model_data %>% 
    select(glm_prediction_final, rf_prediction_final, xgb_prediction_final) %>% 
    apply(.,1,function(x){
      temp_distance <- x %>% lapply(.,function(k){ mean(abs(x - k))}) %>% unlist()
      x[temp_distance != min(temp_distance)] %>% mean() %>%  return()
    }) %>% unlist()
  
  ##############################################################################
  # RETURN #
  ##############################################################################
  res$model_test <- model_test
  res$model_data <- model_data
  return(res)
  
}

