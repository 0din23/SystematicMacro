enet_model <- function(df_decom, h, label_col, split_prop, features, reg_eval = "rsq"){
  
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
    # 
    # step_corr(all_numeric_predictors(),
    #           threshold = 0.85) %>% 
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
    grid_max_entropy(., size = 100)
  
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
  # Fit Values #
  ##############################################################################
  model_test$glm_prediction_best <- res$glm_fit_best %>% 
    collect_predictions() %>% 
    pull(.pred)
  model_data$glm_prediction_final <- predict(res$glm_final, model_data) %>%
    pull(.pred)
  
  ##############################################################################
  # RETURN #
  ##############################################################################
  res$model_test <- model_test
  res$model_data <- model_data
  return(res)
  
}
################################################################################
# CLASSIFICATION #
################################################################################
enet_classification <- function(df_decom,h, label_col, split_prop, features){
  ##############################################################################
  # Innitialisation #
  ##############################################################################
  EpsilonUtility::print_timed("Initialise Stuff")
  ## output list
  res <- list()
  
  ## Combine datasets
  df_decom$simple_bench <- ifelse(df_decom[, label_col]>0,1,0) %>% as.factor()
  df_decom$class <- df_decom$simple_bench  %>% shift(., -h)
  model_data <- df_decom %>% 
    na.omit() %>% 
    left_join(., features, by = "date")
  
  ## filter columns with na
  na_filter <- apply(model_data, 2,function(x){sum(is.na(x)) == 0})
  na_filter[names(na_filter) == "label"] <-TRUE
  model_data <- model_data[,na_filter]
  
  
  ## Splits 
  splits <- initial_time_split(model_data, prop = split_prop)
  model_train <- training(splits)
  model_test  <- testing(splits)
  val_set <- validation_time_split(model_train, prop = split_prop)
  ##############################################################################
  # Linear Model Classification #
  ##############################################################################
  EpsilonUtility::print_timed("Fitting a linear Model")
  
  ## Recipe
  res$glm_class_recipe <- recipe(class ~ ., data = model_data) %>%
    # step_date(date) %>%
    step_rm(date) %>%
    
    step_zv(all_predictors()) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    
    step_dummy(all_nominal_predictors()) %>%
    step_lincomb(all_predictors()) %>% 
    step_corr(all_numeric_predictors(),
              threshold = 0.9) %>%
    prep(training = model_train, retain = TRUE)
  
  
  ## model
  res$glm_class_mod <-logistic_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet") %>% translate()
  
  ## Workflow
  res$glm_class_workflow <- workflow() %>%
    add_model(res$glm_class_mod) %>%
    add_recipe(res$glm_class_recipe)
  
  ## Cross Validation ------------------------------------------------------------
  res$glm_class_grid <- dials::parameters(
    penalty(), mixture()) %>% 
    grid_max_entropy(., size = 120)
  
  ## Train and Tune
  res$glm_class_res <- res$glm_class_workflow %>%
    tune_grid(val_set, grid = res$glm_class_grid,
              control = control_grid(save_pred = TRUE))
  
  res$glm_class_best <- res$glm_class_res %>%
    collect_metrics() %>%
    filter(.metric == "accuracy") %>%
    filter(!is.na(mean)) %>%
    filter(mean == max(mean)) %>% 
    .[1,]
  
  ## fit models ------------------------------------------------------------------
  res$glm_class_model_best <- multinom_reg(penalty = res$glm_class_best$penalty,
                                           mixture = res$glm_class_best$mixture)%>%
    set_engine("glmnet")
  
  res$glm_class_workflow <- res$glm_class_workflow %>%
    update_model(res$glm_class_model_best)
  
  res$glm_fit_class <- res$glm_class_workflow %>%
    last_fit(splits)
  
  ## Fit Production Model --------------------------------------------------------
  res$glm_class_final <- fit(res$glm_class_workflow, 
                             model_data %>%
                               head(nrow(.)-h)
                             )
  ##############################################################################
  # Fit Values #
  ##############################################################################
  ### Linear Model
  model_test$glm_prediction_class <- res$glm_fit_class %>% 
    collect_predictions() %>%
    pull(.pred_class)
  
  model_test$glm_prediction_class_prob <- res$glm_fit_class %>% 
    collect_predictions() %>% 
    pull(.pred_1)
  
  model_data$glm_prediction_class_final <- predict(res$glm_class_final,
                                                   model_data) %>%
    unlist()
  
  model_data$glm_prediction_class_final_prob <- predict(res$glm_class_final,
                                                        model_data,
                                                        type = "prob") %>%
    pull(".pred_1")
  
  ##############################################################################
  # RETURN #
  ##############################################################################
  res$model_test <- model_test
  res$model_data <- model_data
  return(res)
}

