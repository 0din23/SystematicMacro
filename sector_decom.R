
################################################################################
# DATA #
################################################################################
# ETF Data
instruments <-  data.frame(
  "ticker" = c("EXV6.DE", "EXH1.DE", "EXV1.DE", "EXV4.DE", "EXV5.DE",
               "EXH4.DE", "EXV3.DE", "EXV8.DE", "EXH8.DE", "EXH3.DE",
               "EXV2.DE", "EXH7.DE", "EXSA.DE"),
  "names" = c("Basic_Resources", "Oil_Gas", "Banks", "Health_Care", "Automobile",
              "Industrials", "Technology", "Construction", "Utilities", "Food",
              "Telecommunication", "Personal_Goods", "STOXX_600")
)
data <- instruments %>% 
  filter(ticker != "EXSA.DE") %>% 
  pull(ticker) %>% 
  tidyquant::tq_get(., from = "2012-01-01") %>% 
  left_join(., instruments, by = c("symbol"="ticker")) %>%
  na.omit()

# Fama French Data
ff_data <- read.csv(
  "C:/0cap_2/MacroMan/SectorRotationPaper/Europe_5_Factors_Daily.csv",
  sep = ";") %>% 
  mutate(date = as.Date(Date, format=c("%d.%m.%Y")))

# Get Exess Return
data <- data %>%
  left_join(., ff_data,by = c("date")) %>% 
  mutate(
    "excess_return" = RETURN(adjusted, 1) - (RF/100)
  ) %>% 
  select(names, date, excess_return) %>% 
  na.omit()

################################################################################
# DECOMPOSE RETURNS #
################################################################################
## Input and Setup
return_lags <- c(5, 20)
regression_lags <- c(20, 40)
RES_market <- list()

for(k in 1:length(return_lags)){
  RES_market[[k]] <- regression_lags %>% 
    lapply(., function(rl){
      
      # Construct Excess Sector Return
      df_return <- data %>% 
        group_by(names) %>% 
        mutate(return = RETURN(cum.ret(excess_return), return_lags[k])) %>% 
        ungroup(names) %>%          
        select(names, date, return)
      
      X <- tq_get("^STOXX", from = "2000-01-01") %>% 
        select(date, adjusted) %>%
        filter(date %in% df_return$date) %>% 
        mutate(
          Mkt = RETURN(adjusted, return_lags[k])
        ) %>%
        select(-adjusted)
      
      res <- decompose_returns(df_return, X, rl)
      return(res)
    })
}
save(RES_market, file = "RES_market.RData")
################################################################################
# EVALUATE DECOMPOSITION #
################################################################################
K <- return_lags %>% length()
J <- regression_lags %>% length()

# Decomposition Analysis -------------------------------------------------------
decom_ana <- c(1:K) %>% 
  lapply(., function(k){
    c(1:J) %>% 
      lapply(., function(j){
        RES_market[[k]][[j]] %>%
          na.omit() %>% 
          group_by(names) %>% 
          summarize(
            alpha_perc = mean(abs(alpha)) / mean(abs(return)),
            epsilon_perc = mean(abs(epsilon)) / mean(abs(return)),
            ß_perc = mean(abs(ß_Mkt * Mkt)) / mean(abs(return))
          ) %>% 
          as.data.frame() %>% 
          mutate(
            return_lag = k,
            regression_lag = j
          ) %>% 
          return()
      }) %>% 
      rbindlist() %>% 
      return()
  }) %>% 
  rbindlist()


decom_ana %>% 
  group_by(return_lag, regression_lag) %>% 
  summarize(
    alpha = mean(alpha_perc),
    epsilon = mean(epsilon_perc),
    ß = mean(ß_perc),
  )
################################################################################
# BUILD TS-MODEL #
################################################################################
df_decom <- RES_market[[1]][[2]] %>%
  filter(names == "Banks") %>% 
  select(-names)

df_decom %>% pull(alpha) %>% na.omit() %>% stl(s.window='periodic') %>% seasadj()
autoplot(eeadj)

df_decom %>% pull(alpha)  %>% diff() %>% ggtsdisplay(main="")

(fit <- Arima(df_decom %>% pull(alpha), order=c(33,1,15)))


autoplot(fit)
checkresiduals(fit)


train <- df_decom %>% head(round(0.7*nrow(df_decom))) %>% mutate(label = ß_Mkt %>% shift(-6))
test <- df_decom %>% filter(!(date %in% train$date))%>% mutate(label = ß_Mkt %>% shift(-6))

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

################################################################################
# BUILD ML-MODEL #
################################################################################
# Get Feature Set --------------------------------------------------------------
feature_data <- feature_pipe()

# Preprocessing Data -----------------------------------------------------------
df_decom <- RES_market[[1]][[1]] %>%
  filter(names == "Automobile") %>% 
  select(-names)
h <- 6
label_col <- "alpha"
split_prop <- 0.7

## Filter Features
features <- feature_data %>%
  na_col_filter(df=.,th=400) %>% 
  .[-c(1:400),] %>% 
  #ADF_Filter(df=., th = 0.01, exclude = c("date")) %>% 
  Clipping_Filter(df=.) %>% 
  as.data.frame()

features[!is.factor(features)][is.na(features)] <- 0

test_1 <- ml_model(df_decom, h, label_col, split_prop, features = features)
# Test Model -------------------------------------------------------------------

# Evaluate Testset -------------------------------------------------------------
## Evaluation Table
predictions <- test_1$model_test %>%
  select(glm_prediction_best, rf_prediction_best, xgb_prediction_best,
         simple_mean_ensemble,similarity_ensemble, divergence_ensemble,
         alpha)

regressionEvaluation(label = test_1$model_test$label, predictions = predictions) 

## Plots

### Equity Line
p <- test_1$model_test %>% 
  ggplot(.) +
  geom_line(aes(x=date, y = label, color= "Return 20B (forward)"), size=0.7) +
  geom_line(aes(x=date, y = xgb_prediction_best,
                color = "XGB Prediction (rmse)"), size=0.7) +
  geom_line(aes(x=date, y = glm_prediction_best,
                color = "GLM Prediction (rsq)"), size=0.7) +
  geom_line(aes(x=date, y = rf_prediction_best,
                color = "RF Prediction (rmse)"), size=0.7) +
  geom_line(aes(x=date, y = simple_mean_ensemble,
                color = "Simple Mean Ensemble"), size=0.7) +
  geom_line(aes(x=date, y = similarity_ensemble,
                color = "Similarity Ensemble"), size=0.7) +
  geom_line(aes(x=date, y = divergence_ensemble,
                color = "Divergence Ensemble"), size=0.7) +
  ylab("20B Forward Return") + xlab("Date") + ggtitle("Out-off Sample fit (XGB)")+
  scale_color_manual(name = "Legend",
                     values = c("Return 20B (forward)" = "black",
                                "GLM Prediction (rsq)" = "cyan",
                                "RF Prediction (rmse)" = "chartreuse3",
                                "Simple Mean Ensemble" = "red",
                                "Similarity Ensemble" = "green",
                                "Divergence Ensemble" = "blue",
                                "XGB Prediction (rmse)" = "deeppink")) +
  theme_tq()

p %>% ggplotly()

### Difference 
test_1$model_test %>% 
  ggplot(.) +
  geom_line(aes(x=date,
                y = label - xgb_prediction_best,
                color= "Difference"), size=0.5) +
  ylab("20B Forward Return") + xlab("Date") +
  ggtitle("Out-off Sample fit (Linear)")+
  scale_color_manual(name = "Legend",
                     values = c("Difference" = "black")) +
  theme_tq()

### Feature Importance
plot_df <- data.frame(
  "names" = test_1$glm_final$fit$fit$fit$beta %>% rownames(),
  "values"  = test_1$glm_final$fit$fit$fit$beta %>% as.numeric()
) 

plot_df%>%
  filter(values != 0) %>% 
  #  filter(abs(values) >=   as.numeric(quantile(abs(plot_df$values),prob = c(0.90)))) %>% 
  arrange(desc(values)) %>% 
  ggplot(.) + 
  geom_col(aes(x = names, y = values)) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


################################################################################
# BUILD ML-MODEL (Class) #
################################################################################
# Get Feature Set --------------------------------------------------------------
feature_data <- feature_pipe()

# Preprocessing Data -----------------------------------------------------------
df_decom <- RES_market[[2]][[1]] %>%
  filter(names == "Banks") %>% 
  select(-names) %>% 
  mutate(excess = alpha + epsilon)
h <- 21
label_col <- "alpha"
split_prop <- 0.7

## Filter Features
features <- feature_data %>%
  select(-starts_with("adjusted_")) %>% 
  select(-c("^STOXX", "EXSA.DE" )) %>% 
  na_col_filter(df=.,th=400) %>% 
  .[-c(1:400),] %>% 
  ADF_Filter(df=., th = 0.05, exclude = c("date")) %>% 
  Clipping_Filter(df=.)

# features[!is.factor(features)][is.na(features)] <- 0

test_2 <- ml_model_class(df_decom, h, label_col, split_prop, features = features)

# Evaluate Testset -------------------------------------------------------------
## Evaluation Table
predictions <- test_2$model_test %>%
  select(glm_prediction_class, rf_prediction_class, xgb_prediction_class, simple_bench)

classificationEvaluation(label = test_2$model_test$class,  classification_prediction = predictions)

# Feature Importance -----------------------------------------------------------
test_2$rf_class_final %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20)

test_2$glm_class_final$fit$fit$fit$beta
test_2$xgb_class_final$fit$fit$fit$params$
  ################################################################################
# FORECAST #
################################################################################
# Specify Inputs
DATA <- RES_market[[1]][[3]]
h <- 6
split_prop <- 0.5
features <- feature_data %>%
  select(-starts_with("adjusted_")) %>% 
  select(-c("^STOXX", "EXSA.DE" )) %>% 
  na_col_filter(df=.,th=400) %>% 
  .[-c(1:400),] %>% 
  ADF_Filter(df=., th = 0.05, exclude = c("date")) %>% 
  Clipping_Filter(df=.)

# Train And forecast
FORECAST <- DATA %>% 
  pull(names) %>% 
  unique() %>% 
  lapply(., function(x){
    
    ## Get dataframe
    tmp_decom <- DATA %>%
      filter(names == x) %>% 
      select(-names)
    
    # Forecast Alpha
    print_timed("Alpha for: ", x, " --------------------------------------")
    forc_alpha<- ml_model(tmp_decom, h, label_col = "alpha",
                          split_prop = split_prop, features = features)
    
    print_timed("Beta for: ", x, " --------------------------------------")
    forc_beta <- ml_model(tmp_decom, h, label_col = "ß_Mkt",
                          split_prop = split_prop, features = features)
    
    # Combine
    forc_alpha$model_test %>% 
      select(date, alpha,
             glm_prediction_best_alpha = glm_prediction_best,
             rf_prediction_best_alpha = rf_prediction_best,
             xgb_prediction_best_alpha = xgb_prediction_best,
             simple_mean_ensemble_alpha = simple_mean_ensemble,
             similarity_ensemble_alpha = similarity_ensemble,
             divergence_ensemble_alpha = divergence_ensemble) %>% 
      left_join(., 
                forc_beta$model_test %>% 
                  select(date, ß_Mkt,
                         glm_prediction_best_ß = glm_prediction_best,
                         rf_prediction_best_ß = rf_prediction_best,
                         xgb_prediction_best_ß = xgb_prediction_best,
                         simple_mean_ensemble_ß = simple_mean_ensemble,
                         similarity_ensemble_ß = similarity_ensemble,
                         divergence_ensemble_ß = divergence_ensemble),
                by = "date") %>% 
      mutate(names = x) %>% 
      return()
  })

save(FORECAST, file =  "FORECAST.RData")

# Evaluating the Forecast ------------------------------------------------------
## alpha
predictions <- FORECAST %>% rbindlist() %>%
  select(symbol = names,date, alpha, glm_prediction_best_alpha, rf_prediction_best_alpha,
         xgb_prediction_best_alpha, simple_mean_ensemble_alpha, similarity_ensemble_alpha,
         divergence_ensemble_alpha) %>% 
  as.data.frame() %>% 
  group_by(symbol) %>% 
  mutate(
    "opti_alpha" = alpha %>% shift(-21)
  ) %>% 
  ungroup(symbol) %>% 
  select(-date, -symbol)

regressionEvaluation(label = predictions$opti_alpha, predictions = predictions %>% select(-opti_alpha)) 

## beta
predictions <- FORECAST %>% rbindlist() %>%
  select(symbol = names,date, ß_Mkt, glm_prediction_best_ß, rf_prediction_best_ß,
         xgb_prediction_best_ß, simple_mean_ensemble_ß, similarity_ensemble_ß,
         divergence_ensemble_ß) %>% 
  as.data.frame() %>% 
  group_by(symbol) %>% 
  mutate(
    "opti_ß" = ß_Mkt %>% shift(-21)
  ) %>% 
  ungroup(symbol) %>% 
  select(-date, -symbol)

regressionEvaluation(label = predictions$opti_ß, predictions = predictions %>% select(-opti_ß)) 

################################################################################
# SIMPLE BACKTEST #
################################################################################
# Setup ------------------------------------------------------------------------
SIGNALS <- FORECAST %>% rbindlist() %>%
  select(symbol = names,date, alpha, glm_prediction_best_alpha, rf_prediction_best_alpha,
         xgb_prediction_best_alpha, simple_mean_ensemble_alpha, similarity_ensemble_alpha,
         divergence_ensemble_alpha) %>% 
  as.data.frame() %>% 
  group_by(symbol) %>% 
  mutate(
    "opti_alpha" = alpha %>% shift(-21)
  ) %>% 
  ungroup(symbol) %>% 
  na.omit()

BETA_WEIGHTING <- FORECAST %>% rbindlist() %>% 
  select(symbol = names, date, weight_signal = glm_prediction_best_ß) %>% 
  as.data.frame()

OLD_BETA_WEIGHTING <- FORECAST %>% rbindlist() %>% 
  select(symbol = names, date, weight_signal = ß_Mkt) %>% 
  as.data.frame()

BENCHMARK <- tq_get("EXSA.DE", from = "1900-01-01") %>% 
  mutate(return = RETURN(adjusted)) %>% 
  select(date, return)

Returns <- c("EXV6.DE", "EXH1.DE", "EXV1.DE", "EXV4.DE", "EXV5.DE",
             "EXH4.DE", "EXV3.DE", "EXV8.DE", "EXH8.DE", "EXH3.DE",
             "EXV2.DE", "EXH7.DE") %>% 
  tidyquant::tq_get(., from = "2010-01-01") %>% 
  left_join(., instruments, by = c("symbol"="ticker")) %>%
  group_by(symbol) %>% 
  mutate(return=RETURN(adjusted)) %>% 
  ungroup(symbol) %>% 
  select(date, symbol, return) %>% 
  pivot_wider(data=., names_from=symbol, values_from=return) %>% 
  na.omit()

# Construct Weighting ----------------------------------------------------------
equal_weight <- multiSignalTest(SIGNAL = SIGNALS, RETURN = Returns,
                                BENCHMARK = BENCHMARK, WEIGHTING = NULL,
                                BETA_EXPOSURE = 0, with_ranking = FALSE)
old_beta_weight <- multiSignalTest(SIGNAL = SIGNALS, RETURN = Returns,
                                   BENCHMARK = BENCHMARK,
                                   WEIGHTING = OLD_BETA_WEIGHTING,
                                   BETA_EXPOSURE = 0, with_ranking = FALSE)
beta_weight <- multiSignalTest(SIGNAL = SIGNALS, RETURN = Returns,
                               BENCHMARK = BENCHMARK,
                               WEIGHTING = BETA_WEIGHTING,
                               BETA_EXPOSURE = 0, with_ranking = FALSE)
# Evaluate ---------------------------------------------------------------------
# Daily Rebalancing ----
## Equal Weight
evaluateMomentumStrategy(
  df = equal_weight$eval$daily,
  benchmark = "return", 
  weights = equal_weight$daily
)

## Old Beta
evaluateMomentumStrategy(
  df = old_beta_weight$eval$daily,
  benchmark = "return", 
  weights = old_beta_weight$daily
)

## New Beta
evaluateMomentumStrategy(
  df = beta_weight$eval$daily,
  benchmark = "return", 
  weights = beta_weight$daily
)

# Weekly Rebalancing ----
## Equal Weight
evaluateMomentumStrategy(
  df = equal_weight$eval$weekly,
  benchmark = "return", 
  weights = equal_weight$weekly
)

## Old Beta
evaluateMomentumStrategy(
  df = old_beta_weight$eval$weekly,
  benchmark = "return", 
  weights = old_beta_weight$weekly
)

## New Beta
evaluateMomentumStrategy(
  df = beta_weight$eval$weekly,
  benchmark = "return", 
  weights = beta_weight$weekly
)

# Monthly Rebalancing ----
## Equal Weight
evaluateMomentumStrategy(
  df = equal_weight$eval$monthly,
  benchmark = "return", 
  weights = equal_weight$monthly
)

## Old Beta
evaluateMomentumStrategy(
  df = old_beta_weight$eval$monthly,
  benchmark = "return", 
  weights = old_beta_weight$monthly
)

## New Beta
evaluateMomentumStrategy(
  df = beta_weight$eval$monthly,
  benchmark = "return", 
  weights = beta_weight$monthly
)
# Plot -------------------------------------------------------------------------
## Equally Weighted
# Daily Rebalancing
equal_weight$eval$daily %>% 
  pivot_longer(.,cols = colnames(.)[-1],
               names_to = "Signal", values_to = "Return") %>% 
  group_by(Signal) %>% 
  mutate(Return = cumRet(Return)) %>% 
  ungroup(Signal) %>% 
  ggplot(.)+
  geom_line(aes(x=date,y=Return, color = Signal), size=0.7)+
  ylab("Return") + xlab("Date") + ggtitle("Daily rebalance")+
  theme_tq()

# Weekly Rebalancing
equal_weight$eval$weekly %>% 
  pivot_longer(.,cols = colnames(.)[-1],
               names_to = "Signal", values_to = "Return") %>% 
  group_by(Signal) %>% 
  mutate(Return = cumRet(Return)) %>% 
  ungroup(Signal) %>% 
  ggplot(.)+
  geom_line(aes(x=date,y=Return, color = Signal), size=0.7)+
  ylab("Return") + xlab("Date") + ggtitle("Weekly rebalance")+
  theme_tq()

# Monthly Rebalancing
equal_weight$eval$monthly %>% 
  pivot_longer(.,cols = colnames(.)[-1],
               names_to = "Signal", values_to = "Return") %>% 
  group_by(Signal) %>% 
  mutate(Return = cumRet(Return)) %>% 
  ungroup(Signal) %>% 
  ggplot(.)+
  geom_line(aes(x=date,y=Return, color = Signal), size=0.7)+
  ylab("Return") + xlab("Date") + ggtitle("Monthly rebalance")+
  theme_tq()

## Weighted by past Beta
# Daily Rebalancing
old_beta_weight$eval$daily %>% 
  pivot_longer(.,cols = colnames(.)[-1],
               names_to = "Signal", values_to = "Return") %>% 
  group_by(Signal) %>% 
  arrange(date) %>% 
  mutate(Return = cumRet(Return)) %>% 
  ungroup(Signal) %>% 
  ggplot(.)+
  geom_line(aes(x=date,y=Return, color = Signal), size=0.7)+
  ylab("Return") + xlab("Date") + ggtitle("Daily rebalance")+
  theme_tq()

# Weekly Rebalancing
old_beta_weight$eval$weekly %>% 
  pivot_longer(.,cols = colnames(.)[-1],
               names_to = "Signal", values_to = "Return") %>% 
  group_by(Signal) %>% 
  mutate(Return = cumRet(Return)) %>% 
  ungroup(Signal) %>% 
  ggplot(.)+
  geom_line(aes(x=date,y=Return, color = Signal), size=0.7)+
  ylab("Return") + xlab("Date") + ggtitle("Weekly rebalance")+
  theme_tq()

# Monthly Rebalancing
old_beta_weight$eval$monthly %>% 
  pivot_longer(.,cols = colnames(.)[-1],
               names_to = "Signal", values_to = "Return") %>% 
  group_by(Signal) %>% 
  arrange(date) %>% 
  mutate(Return = cumRet(Return)) %>% 
  ungroup(Signal) %>% 
  ggplot(.)+
  geom_line(aes(x=date,y=Return, color = Signal), size=0.7)+
  ylab("Return") + xlab("Date") + ggtitle("Monthly rebalance")+
  theme_tq()

## Weighted by forecast Beta
# Daily Rebalancing
beta_weight$eval$daily %>% 
  pivot_longer(.,cols = colnames(.)[-1],
               names_to = "Signal", values_to = "Return") %>% 
  group_by(Signal) %>% 
  mutate(Return = cumRet(Return)) %>% 
  ungroup(Signal) %>% 
  ggplot(.)+
  geom_line(aes(x=date,y=Return, color = Signal), size=0.7)+
  ylab("Return") + xlab("Date") + ggtitle("Daily rebalance")+
  theme_tq()

# Weekly Rebalancing
p <- beta_weight$eval$weekly %>% 
  pivot_longer(.,cols = colnames(.)[-1],
               names_to = "Signal", values_to = "Return") %>% 
  group_by(Signal) %>%
  arrange(date) %>% 
  mutate(Return = cumRet(Return)) %>% 
  ungroup(Signal) %>% 
  ggplot(.)+
  geom_line(aes(x=date,y=Return, color = Signal), size=0.7)+
  ylab("Return") + xlab("Date") + ggtitle("Weekly rebalance")+
  theme_tq()
p %>% ggplotly()

# Monthly Rebalancing
beta_weight$eval$monthly %>% 
  pivot_longer(.,cols = colnames(.)[-1],
               names_to = "Signal", values_to = "Return") %>% 
  group_by(Signal) %>% 
  mutate(Return = cumRet(Return)) %>% 
  ungroup(Signal) %>% 
  ggplot(.)+
  geom_line(aes(x=date,y=Return, color = Signal), size=0.7)+
  ylab("Return") + xlab("Date") + ggtitle("Monthly rebalance")+
  theme_tq()

################################################################################
# CONCLUSION #
################################################################################