ml_model_class <- function(df_decom,h, label_col, split_prop, features){
  ##############################################################################
  # Innitialisation #
  ##############################################################################
  EpsilonUtility::print_timed("Initialise Stuff")
  ## output list
  res <- list()
  
  ## assing cores
  cores <- parallel::detectCores()
  
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
  
  ## assing cores
  cores <- parallel::detectCores()
  
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
              threshold = 0.85) %>%
    prep(training = model_train, retain = TRUE)
  
  
  ## model
  res$glm_class_mod <-logistic_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet") %>% translate()
  
  ## Workflow
  res$glm_class_workflow <- workflow() %>%
    add_model(res$glm_class_mod) %>%
    add_recipe(res$glm_class_recipe)
  
  ## Cross Validation ------------------------------------------------------------
  res$glm_class_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30),
                               mixture = seq(0,1,length.out = 30))
  res$glm_class_grid <- dials::parameters(
    penalty(), mixture()) %>% 
    grid_max_entropy(., size = 120)
  
  ## Train and Tune
  res$glm_class_res <- res$glm_class_workflow %>%
    tune_grid(val_set, grid = res$glm_class_grid,
              control = control_grid(save_pred = TRUE))
  
  res$glm_class_best <- res$glm_class_res %>%
    collect_metrics() %>%
    filter(.metric == "roc_auc") %>%
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
                             model_data %>% head(nrow(.)-h))
  
  ##############################################################################
  # Random Forrest Classification #
  ##############################################################################
  EpsilonUtility::print_timed("Fitting the Random Forrest")
  
  ## recipe
  res$rf_class_recipe <- recipe(class ~ ., data = model_train) %>% 
    #step_date(date) %>% 
    step_rm(date) %>% 
    
    step_zv(all_predictors()) %>% 
    #step_normalize(all_numeric_predictors()) %>% 
    
    step_lincomb(all_numeric_predictors()) %>% 
    
    step_corr(all_numeric_predictors(),
              threshold = 0.85) %>% 
    prep(training = model_train, retain = TRUE)
  
  ## model
  doParallel::registerDoParallel()
  res$rf_class_model <- rand_forest(mtry = tune(),
                                    min_n = tune(),
                                    trees = tune())%>% 
    set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
    set_mode("classification") 
  
  ## Workflow
  res$rf_class_workflow <- workflow() %>% 
    add_model(res$rf_class_model) %>% 
    add_recipe(res$rf_class_recipe)
  
  ## Cross Validation ------------------------------------------------------------
  res$rf_class_grid <- tibble(mtry = seq(1,60, length.out = 60),
                              trees = seq(500, 1000, length.out = 60),
                              min_n = seq(2,61  ,length.out = 60))
  
  ## Train and Tune
  res$rf_class_res <- res$rf_class_workflow %>% 
    tune_grid(val_set,
              grid = res$rf_class_grid,
              control = control_grid(save_pred = TRUE))
  
  res$rf_class_best <- res$rf_class_res %>% 
    collect_metrics() %>%
    filter(.metric == "roc_auc") %>%
    filter(!is.na(mean)) %>% 
    filter(mean == max(mean)) %>% 
    .[1,]
  
  ## fit models ------------------------------------------------------------------
  res$rf_class_best_model <- rand_forest(mtry = res$rf_class_best$mtry,
                                         min_n = res$rf_class_best$min_n,
                                         trees = res$rf_class_best$trees) %>% 
    set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
    set_mode("classification") 
  
  res$rf_class_workflow <- res$rf_class_workflow %>% 
    update_model(res$rf_class_best_model)
  
  res$rf_class_fit_best <- res$rf_class_workflow  %>% 
    last_fit(splits)
  
  ## Fit Production Model --------------------------------------------------------
  res$rf_class_final <- fit(res$rf_class_workflow,
                            model_data %>% head(nrow(.)-h))
  
  ##############################################################################
  # XGB Classification #
  ##############################################################################
  EpsilonUtility::print_timed("Fitting XGB")
  
  ## recipe
  res$xgb_class_recipe <- recipe(class ~ ., data = model_train) %>%
    #step_date(date) %>% 
    step_rm(date) %>% 
    
    step_zv(all_predictors()) %>% 
    #step_normalize(all_numeric_predictors()) %>% 
    
    step_string2factor(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>% 
    
    step_lincomb(all_numeric_predictors()) %>% 
    step_corr(all_numeric_predictors(),
              threshold = 0.85) %>% 
    step_impute_roll() %>% 
    step_naomit()
  
  ## model
  res$xgb_class_model <- boost_tree(trees = tune(), min_n = tune(),
                                    tree_depth = tune(), learn_rate = tune(),
                                    loss_reduction = tune()) %>%
    set_engine("xgboost",  num.threads = cores) %>% 
    set_mode("classification")
  
  ## Workflow
  res$xgb_class_workflow <- workflow() %>% 
    add_model(res$xgb_class_model) %>% 
    add_recipe(res$xgb_class_recipe)
  
  ## Cross Validation ------------------------------------------------------------
  ### Train and Tune
  res$xgb_class_grid <- dials::parameters(
    min_n(), tree_depth(), learn_rate(),
    loss_reduction(), trees()) %>% 
    grid_max_entropy(., size = 60)
  
  res$xgb_class_res <- res$xgb_class_workflow  %>% 
    tune_grid(
      val_set,
      grid = res$xgb_class_grid,
      control = control_grid(save_pred = TRUE))
  
  res$xgb_class_best <- res$xgb_class_res %>%
    collect_metrics() %>%
    filter(.metric == "roc_auc") %>%
    filter(!is.na(mean)) %>%
    filter(mean == max(mean)) %>% 
    .[1,]
  
  ## fit models ------------------------------------------------------------------
  res$xgb_class_best_model <- boost_tree(mode = "classification",
                                         trees = res$xgb_class_best$trees,
                                         min_n = res$xgb_class_best$min_n,
                                         tree_depth = res$xgb_class_best$tree_depth,
                                         learn_rate = res$xgb_class_best$learn_rate,
                                         loss_reduction =
                                           res$xgb_class_best$loss_reduction) %>%
    set_engine("xgboost")
  
  res$xgb_class_workflow <- res$xgb_class_workflow %>% 
    update_model(res$xgb_class_best_model)
  
  res$xgb_class_fit_best <- res$xgb_class_workflow  %>% 
    last_fit(splits)
  
  ## Fit Production Model --------------------------------------------------------
  res$xgb_class_final <- fit(res$xgb_class_workflow,
                             model_data %>% head(nrow(.)-h))
  
  ##############################################################################
  # Fit Values #
  ##############################################################################
  ### Linear Model
  model_test$glm_prediction_class <- res$glm_fit_class %>% 
    collect_predictions() %>% pull(.pred_class)
  
  model_test$glm_prediction_class_prob <- res$glm_fit_class %>% 
    collect_predictions() %>% pull(.pred_class)
  
  model_data$glm_prediction_class_final <- predict(res$glm_class_final,
                                                   model_data) %>% unlist()
  
  model_data$glm_prediction_class_final_prob <- predict(res$glm_class_final,
                                                        model_data,
                                                        type = "prob") %>%
    pull(".pred_1")
  
  ### Random Forrest
  model_test$rf_prediction_class <- res$rf_class_fit %>%
    collect_predictions() %>% pull(.pred_class)
  
  model_test$rf_prediction_class_prob <- res$rf_class_fit %>%
    collect_predictions() %>% pull(.pred_class)
  
  model_data$rf_prediction_class_final <- predict(res$rf_class_final,
                                                  model_data) %>% unlist()
  
  model_data$rf_prediction_class_final_prob <- predict(res$rf_class_final,
                                                       model_data,
                                                       type = "prob") %>%
    pull(".pred_1")
  
  ### XGB
  model_test$xgb_prediction_class <- res$xgb_class_fit %>%
    collect_predictions() %>% pull(.pred_class)
  
  model_test$xgb_prediction_class_prob <- res$xgb_class_fit %>%
    collect_predictions() %>% pull(.pred_class)
  
  
  model_data$xgb_prediction_class_final <- predict(res$xgb_class_final,
                                                   model_data) %>% unlist()
  
  model_data$xgb_prediction_class_final_prob <- predict(res$xgb_class_final,
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
