source("R/dependencies.R")
################################################################################
# DATA #
################################################################################
instruments <-  data.frame(
  "ticker" = c("EXV6.DE", "EXH1.DE", "EXV1.DE", "EXV4.DE", "EXV5.DE",
               "EXH4.DE", "EXV3.DE", "EXV8.DE", "EXH8.DE", "EXH3.DE",
               "EXV2.DE", "EXH7.DE"),
  "names" = c("Basic_Resources", "Oil_Gas", "Banks", "Health_Care", "Automobile",
              "Industrials", "Technology", "Construction", "Utilities", "Food",
              "Telecommunication", "Personal_Goods")
)
data <- instruments %>% 
  pull(ticker) %>% 
  tidyquant::tq_get(., from = "2010-01-01") %>% 
  left_join(., instruments, by = c("symbol"="ticker")) %>%
  na.omit()

################################################################################
# DECOMPOSE RETURNS #
################################################################################
## Input and Setup
df_return <- data %>% 
  group_by(names) %>% 
  mutate(return = RETURN(adjusted)) %>% 
  ungroup(names) %>%          
  select(names, date, return)

X <- tq_get("^STOXX", from = "2000-01-01") %>% 
  select(date, adjusted) %>%
  filter(date %in% df_return$date) %>% 
  mutate(
    Mkt = RETURN(adjusted, 1)
  ) %>%
  select(-adjusted)

DEC <- decompose_returns(df_return, X, regression_lag = 30) 

# Check Alpha vs. Epsilon
DEC %>% 
  group_by(names) %>% 
  summarize(
    alpha_perc = mean(abs(alpha), na.rm=T) / mean(abs(return), na.rm=T),
    epsilon_perc = mean(abs(epsilon), na.rm=T) / mean(abs(return), na.rm=T),
  )

# Check R^2
reg <- lm(DEC$return ~ DEC$y_hat)
reg %>% summary()

################################################################################
# ML-MODEL (REGRESSION) #
################################################################################
# Get Feature Set --------------------------------------------------------------
feature_data <- feature_data[[1]]

# Preprocessing Data -----------------------------------------------------------
X <- "Basic_Resources"
h <- 20

df_decom <- DEC %>%
  filter(names == X) %>%
  na.omit() %>% 
  select(-names, -symbol) %>% 
  mutate(actuall_alpha = h*log_alpha_1B_bar) %>% 
  as.data.frame() 


label_col <- "actuall_alpha"
split_prop <- 0.7

## Filter Features
features <- feature_data %>% 
  left_join(., df_decom %>% select(-all_of(label_col)), by = "date")

features_filter <- linear_filter(df = features,
                                 split_prop = split_prop,
                                 label_df = df_decom %>% select(date, all_of(label_col)),
                                 h = h , r2_th = 0.01, cor_th = 0.8,
                                 max_features = 25)

filtered_features <- features %>% select(all_of(features_filter))
filtered_decom <- df_decom %>% select(date, all_of(label_col)) %>% filter(date %in% filtered_features$date)
filtered_features <- filtered_features %>% filter(date %in% filtered_decom$date)

date_filter <- myEndpoints(filtered_features$date, endpoint = "weekly")


test_1 <- enet_model(df_decom = filtered_decom,#[date_filter,],
                     h = h, label_col = label_col,
                     split_prop = split_prop,
                     features = filtered_features,#[date_filter,]
                     )

test_2 <- enet_classification(df_decom = filtered_decom,#[date_filter,],
                              h = h, label_col = label_col,
                              split_prop = split_prop, features = filtered_features#[date_filter,]
                              )

## Evaluation Table
predictions <- test_1$model_test %>%
  select(glm_prediction_best, all_of(label_col))

regressionEvaluation(label = test_1$model_test$label, predictions = predictions) 

## Evaluate Classification Quality
predictions <- test_1$model_test %>%
  select(glm_prediction_best, all_of(label_col)) %>% 
  apply(.,2, function(x){ifelse(x>0,1,0)}) %>% 
  as.data.frame() %>% 
  mutate_all(as.factor)

classificationEvaluation(label = ifelse(test_1$model_test$label>0,1,0) %>% as.factor(),  classification_prediction = predictions)

### Classification model
predictions <- test_2$model_test %>%
  select(glm_prediction_class)#, alpha, cum_alpha) %>% 
  # mutate(alpha = as.factor(ifelse(alpha>0,1,0)),
  #        cum_alpha = as.factor(ifelse(cum_alpha>0,1,0)))

classificationEvaluation(label = test_2$model_test$class,  classification_prediction = predictions)

## Evaluation Plot
p <- test_1$model_test %>% 
  ggplot(.) +
  geom_line(aes(x=date, y = label, color= "Label"), linewidth=0.7) +
  geom_line(aes(x=date, y = glm_prediction_best,
                color = "GLM Prediction"), linewidth=0.6) +
  geom_line(aes(x=date, y = alpha,
                color = "Simple Benchmark"), linewidth=0.6) +
  ylab("20B Forward Return") + xlab("Date") + ggtitle("Out-off Sample fit (XGB)")+
  scale_color_manual(name = "Legend",
                     values = c("Label" = "black",
                                "GLM Prediction" = "cyan",
                                "Simple Benchmark" = "yellow")) +
  theme_tq()

p %>% ggplotly()

### Feature Importance
plot_df <- data.frame(
  "names" = test_1$glm_final$fit$fit$fit$beta %>% rownames(),
  "values"  = test_1$glm_final$fit$fit$fit$beta %>% as.numeric()
) 

plot_df%>%
  filter(values != 0) %>% 
  filter(abs(values) >=   as.numeric(quantile(abs(plot_df$values),prob = c(0.90)))) %>% 
  arrange(desc(values)) %>% 
  ggplot(.) + 
  geom_col(aes(x = names, y = values)) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

################################################################################
# BACKTEST #
################################################################################

# Specify Inputs
DATA <- DEC
split_prop <- 0.7
h <- 20
label_col <- "actuall_alpha"

# Train And forecast
FORECAST_lin_sector <- DATA %>% 
  pull(names) %>% 
  unique() %>% 
  lapply(., function(x){
    
    ## Get dataframe
    tmp_decom <- DEC %>%
      filter(names == x) %>%
      na.omit() %>% 
      select(-names, -symbol) %>% 
      mutate(actuall_alpha = h*log_alpha_1B_bar) %>% 
      as.data.frame() 
    
    # Forecast Alpha
    print_timed("Alpha for: ", x, " --------------------------------------")
    features <- feature_data %>% 
      left_join(., tmp_decom %>% select(-all_of(label_col)), by = "date")
    
    features_filter <- linear_filter(df = features,
                                     split_prop = split_prop,
                                     label_df = tmp_decom %>% select(date, all_of(label_col)),
                                     h = h , r2_th = 0.01, cor_th = 0.9,
                                     max_features = 25)
    
    filtered_features <- features %>% select(all_of(features_filter))
    filtered_decom <- tmp_decom %>% select(date, all_of(label_col)) %>% filter(date %in% filtered_features$date)
    filtered_features <- filtered_features %>% filter(date %in% filtered_decom$date)
    
    forc_alpha <- enet_model(df_decom = filtered_decom,
                             h = h, label_col = label_col,
                             split_prop = split_prop, features = filtered_features)
    
    forc_alpha_class <- enet_classification(df_decom = filtered_decom,
                                            h = h, label_col = label_col,
                                            split_prop = split_prop, features = filtered_features)
    
    
    forc_alpha$model_test %>%
      select(date, all_of(label_col),
             glm_prediction_best_alpha = glm_prediction_best) %>%
      left_join(.,
                forc_alpha_class$model_test %>%
                  mutate(alpha_class = ifelse(all_of(label_col)>0,1,0),
                         glm_prediction_class = as.numeric(glm_prediction_class)
                  ) %>% 
                  select(date, alpha_class, glm_prediction_class, glm_prediction_class_prob),
                by = "date") %>%
      mutate(names = x) %>%
      return()
  })

save(FORECAST_lin_sector, file =  "FORECAST_gen_lin.RData")

# Evaluating the Forecast ------------------------------------------------------
## alpha
predictions <- FORECAST_lin_sector %>% 
  rbindlist() %>%
  mutate(glm_prediction_class = glm_prediction_class-1) %>% 
  select(symbol = names,date, all_of(label_col),
         glm_prediction_best_alpha,glm_prediction_class, alpha_class) %>% 
  as.data.frame() %>% 
  group_by(symbol) %>% 
  #mutate(opti_alpha = all_of(label_col) %>% shift(-h)) %>% 
  ungroup(symbol) %>% 
  select(-date, -symbol)
predictions[,"opti_alpha"] <- predictions %>% pull(all_of(label_col)) %>% shift(-h)

regressionEvaluation(label = predictions$opti_alpha,
                     predictions = predictions %>%
                       select(glm_prediction_best_alpha, all_of(label_col))) 

## Evaluate Classification Quality
predictions <- predictions %>% 
  mutate(
         glm_prediction_class = glm_prediction_class %>% as.factor(),
         glm_prediction_best_alpha = ifelse(glm_prediction_best_alpha > 0,1,0) %>% 
           as.factor(),
         opti_alpha = ifelse(opti_alpha >0,1,0) %>% as.factor()) %>% 
  as.data.frame() %>% 
  select(opti_alpha, alpha_class, glm_prediction_best_alpha, glm_prediction_class)

classificationEvaluation(label = predictions$opti_alpha,
                         classification_prediction = predictions %>% select(-opti_alpha))

## Evaluate ranking
df <- FORECAST_gen_lin %>% 
  rbindlist() %>%
  group_by(names) %>% 
  mutate(opti_alpha = alpha %>% shift(-h)) %>% 
  ungroup(names) %>% 
  select(date, names,opti_alpha, alpha, glm_prediction_best_alpha, glm_prediction_class_prob)
strats <- c("alpha", "glm_prediction_best_alpha", "glm_prediction_class_prob")
rank_test(df = df, strats = strats)



### Probability evaluation
FORECAST_gen_lin %>% 
  rbindlist() %>%
  group_by(names) %>% 
  mutate(opti_alpha = alpha %>% shift(-h)) %>% 
  ungroup(names) %>%
  mutate(opti_alpha_p = ifelse(opti_alpha >0,1,0),
         opti_alpha_n = ifelse(opti_alpha >0,0,1)) %>% 
  mutate(pp_prob = glm_prediction_class_prob* opti_alpha_p,
         np_prob = glm_prediction_class_prob* opti_alpha_n) %>% 
  group_by(names) %>% 
  summarize(
    positive_prediction_probabilita = mean(pp_prob, na.rm=T),
    negative_prediction_probabilita = 1-mean(np_prob, na.rm=T),
  )

################################################################################
# SIMPLE BACKTEST #
################################################################################
# Setup ------------------------------------------------------------------------
SIGNALS <- FORECAST_lin_sector %>%
  rbindlist() %>%
  select(symbol = names,date, all_of(label_col), glm_prediction_best_alpha,
         alpha_class, glm_prediction_class, glm_prediction_class_prob) %>% 
  mutate(glm_prediction_class = glm_prediction_class-1) %>% 
  as.data.frame() %>% 
  na.omit()
SIGNALS[,"opti_alpha"] <- SIGNALS %>% pull(all_of(label_col)) %>% shift(-h)



BENCHMARK <- tq_get("EXSA.DE", from = "1900-01-01") %>% 
  mutate(return = RETURN(adjusted)) %>% 
  select(date, return)

Returns <- instruments$ticker %>% 
  tidyquant::tq_get(., from = "2010-01-01") %>% 
  left_join(., instruments, by = c("symbol"="ticker")) %>%
  group_by(symbol) %>% 
  mutate(return=RETURN(adjusted)) %>% 
  ungroup(symbol) %>% 
  select(date, symbol, return) %>% 
  pivot_wider(data=., names_from=symbol, values_from=return) %>% 
  na.omit()

betas <- paste0("ß_", DATA$symbol %>% unique())

BETAS <- DATA %>% 
  select(all_of(betas))

# Construct Weighting ----------------------------------------------------------
equal_weight_ranking <- multiSignalTest(SIGNAL = SIGNALS, RETURN = Returns,
                                        BENCHMARK = BENCHMARK, WEIGHTING = NULL,
                                        BETA_EXPOSURE = 0.5, with_ranking = TRUE,
                                        ranking_quantile = 0.7)

equal_weight_no_ranking <- multiSignalTest(SIGNAL = SIGNALS, RETURN = Returns,
                                           BENCHMARK = BENCHMARK, WEIGHTING = NULL,
                                           BETA_EXPOSURE = 0.5, with_ranking = FALSE)

# Evaluate ---------------------------------------------------------------------
## Daily
evaluateMomentumStrategy(
  df = equal_weight_ranking$eval$daily,
  benchmark = "return", 
  weights = equal_weight_ranking$daily
)
evaluateMomentumStrategy(
  df = equal_weight_no_ranking$eval$daily,
  benchmark = "return", 
  weights = equal_weight_no_ranking$daily
)

## Weekly
evaluateMomentumStrategy(
  df = equal_weight_ranking$eval$weekly,
  benchmark = "return", 
  weights = equal_weight_ranking$weekly
)
evaluateMomentumStrategy(
  df = equal_weight_no_ranking$eval$weekly,
  benchmark = "return", 
  weights = equal_weight_no_ranking$weekly
)

## Monthly
evaluateMomentumStrategy(
  df = equal_weight_ranking$eval$monthly,
  benchmark = "return", 
  weights = equal_weight_ranking$monthly
)
evaluateMomentumStrategy(
  df = equal_weight_no_ranking$eval$monthly,
  benchmark = "return", 
  weights = equal_weight_no_ranking$monthly
)

p <- equal_weight_ranking$eval$monthly %>% 
  pivot_longer(.,cols = colnames(.)[-1],
               names_to = "Signal", values_to = "Return") %>% 
  group_by(Signal) %>%
  arrange(date) %>% 
  mutate(Return = cumRet(Return)) %>% 
  ungroup(Signal) %>% 
  ggplot(.)+
  geom_line(aes(x=date,y=Return, color = Signal), linewidth=0.5)+
  ylab("Return") + xlab("Date") + ggtitle("Weekly rebalance")+
  theme_tq()
p %>% ggplotly()

p <- equal_weight_no_ranking$eval$monthly %>% 
  pivot_longer(.,cols = colnames(.)[-1],
               names_to = "Signal", values_to = "Return") %>% 
  group_by(Signal) %>%
  arrange(date) %>% 
  mutate(Return = cumRet(Return)) %>% 
  ungroup(Signal) %>% 
  ggplot(.)+
  geom_line(aes(x=date,y=Return, color = Signal), linewidth=0.5)+
  ylab("Return") + xlab("Date") + ggtitle("Weekly rebalance")+
  theme_tq()
p %>% ggplotly()
