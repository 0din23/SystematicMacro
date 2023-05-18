# Dependencies
library(tidymodels)
library(tidyverse)
library(tidyquant)
library(fable)
library(tsibbledata)
library(tsibble)
library(rollRegres)
library(EpsilonUtility)
library(data.table)
library(purrr)
library(tseries)
library(kernlab)
library(feasts)
library(PortfolioAnalytics) 
library(DEoptim)
library(bidask)
library(plotly)
library(vip)
library(forecast)

## Source Scripts
source("R/decomposition_functions.R")
source("R/feature_pipeline.R")
source("R/model_functions.R")
source("R/filter_functions.R")
source("R/helper_functions.R")
source("R/portfolio_construction.R")
source("R/regression_model.R")
source("R/classification_model.R")

################################################################################
# DATA #
################################################################################
instruments <-  data.frame(
  "ticker" = c("USDEUR=X", "CADEUR=X", "GBPEUR=X", "NZDEUR=X", "AUDEUR=X"),
  "names" = c("USD", "CAD", "GBP", "NZD", "AUD")
)
data <- instruments %>% 
  pull(ticker) %>% 
  tidyquant::tq_get(., from = "1900-01-01") %>% 
  left_join(., instruments, by = c("symbol"="ticker")) %>%
  na.omit()

################################################################################
# DECOMPOSE RETURNS #
################################################################################
## Input and Setup
return_lags <- c(5, 20)
regression_lags <- c(20, 40, 60)
DEC <- list()

df_return <- data %>% 
  group_by(symbol) %>% 
  mutate(
    return = RETURN(adjusted, 20)
  ) %>% 
  ungroup(symbol) %>% 
  select(-symbol) %>% 
  select(symbol = names, date, return) %>% 
  pivot_wider(data=., names_from = symbol, values_from=return) %>% 
  na.omit()

DEC <- gen_decompose(df_return, regression_lag = 20)

# Check Alpha vs. Epsilon
DEC %>% 
  group_by(symbol) %>% 
  summarize(
    alpha_perc = mean(abs(alpha), na.rm=T) / mean(abs(return), na.rm=T),
    epsilon_perc = mean(abs(epsilon), na.rm=T) / mean(abs(return), na.rm=T),
  )


# Check R^2
reg <- lm(DEC$return ~ DEC$y_hat)
reg %>% summary()

################################################################################
# FEATURE ENGINEERRING #
################################################################################
## Load
feature_data <- feature_pipe_FX()

## filter and Process
features <- feature_data %>%
  na_col_filter(df=.,th=400) %>% 
  .[-c(1:400),] %>% 
  ADF_Filter(df=., th = 0.05, exclude = c("date")) %>% 
  Clipping_Filter(df=.) %>% 
  as.data.frame()


################################################################################
# ML-MODEL (REGRESSION) #
################################################################################

# Testing on a single Pair -----------------------------------------------------
## Preprocess
df_decom <- DEC %>%
  filter(symbol == "USD") %>% 
  select(-symbol, -ß_USD, -USD) %>%
  as.data.frame() 

h <- 21
label_col <- "alpha"
split_prop <- 0.7

features <- features %>% 
  left_join(., df_decom %>% select(-c(alpha)), by = "date")

features_filter <- feature_selection_filter(df=features,
                                            split_prop = 0.6,
                                            label_df = df_decom %>% select(date, alpha),
                                            h = 21, reg_th = 0.025, rf_q = 0.85)

# data.frame(
#   "var" = features_filter$filtered %>% names(),
#   "r2" = features_filter$reg_val[-length(features_filter$reg_val)]
# ) %>% 
#   arrange(desc(r2)) %>% 
#   filter(r2 >= 0.025)

filtered_decom <- df_decom %>% select(date, alpha) %>% filter(date %in% features$date)
filtered_features <- features[,c(T,features_filter$filtered)] %>% filter(date %in% filtered_decom$date)

test_1 <- ml_model(df_decom = filtered_decom, h, label_col, split_prop, features = filtered_features, reg_eval = "rsq")

## Evaluation Table
predictions <- test_1$model_test %>%
  select(glm_prediction_best, rf_prediction_best, xgb_prediction_best,
         simple_mean_ensemble,similarity_ensemble, divergence_ensemble,
         alpha)

regressionEvaluation(label = test_1$model_test$label, predictions = predictions) 

## Evaluate Classification Quality
predictions <- test_1$model_test %>%
  select(glm_prediction_best, rf_prediction_best, xgb_prediction_best,
         simple_mean_ensemble,similarity_ensemble, divergence_ensemble,
         alpha) %>% 
  apply(.,2, function(x){ifelse(x>0,1,0)}) %>% 
  as.data.frame() %>% 
  mutate_all(as.factor)

classificationEvaluation(label = ifelse(test_1$model_test$label>0,1,0) %>% as.factor(),  classification_prediction = predictions)

## Evaluation Plot
p <- test_1$model_test %>% 
  ggplot(.) +
  geom_line(aes(x=date, y = label, color= "Label"), linewidth=0.7) +
  geom_line(aes(x=date, y = xgb_prediction_best,
                color = "XGB Prediction"), linewidth=0.6) +
  geom_line(aes(x=date, y = glm_prediction_best,
                color = "GLM Prediction"), linewidth=0.6) +
  geom_line(aes(x=date, y = rf_prediction_best,
                color = "RF Prediction"), linewidth=0.6) +
  geom_line(aes(x=date, y = simple_mean_ensemble,
                color = "Simple Mean Ensemble"), linewidth=0.6) +
  geom_line(aes(x=date, y = similarity_ensemble,
                color = "Similarity Ensemble"), linewidth=0.6) +
  geom_line(aes(x=date, y = divergence_ensemble,
                color = "Divergence Ensemble"), linewidth=0.6) +
  geom_line(aes(x=date, y = alpha,
                color = "Simple Benchmark"), linewidth=0.6) +
  ylab("20B Forward Return") + xlab("Date") + ggtitle("Out-off Sample fit (XGB)")+
  scale_color_manual(name = "Legend",
                     values = c("Label" = "black",
                                "GLM Prediction" = "cyan",
                                "RF Prediction" = "chartreuse3",
                                "Simple Mean Ensemble" = "red",
                                "Similarity Ensemble" = "green",
                                "Divergence Ensemble" = "blue",
                                "XGB Prediction" = "deeppink",
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
  #  filter(abs(values) >=   as.numeric(quantile(abs(plot_df$values),prob = c(0.90)))) %>% 
  arrange(desc(values)) %>% 
  ggplot(.) + 
  geom_col(aes(x = names, y = values)) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

################################################################################
# ML-MODEL (CLASSIFICATION) #
################################################################################

# Testing on a single Pair -----------------------------------------------------
## Preprocess
df_decom <- DEC %>%
  filter(symbol == "USD") %>% 
  select(-symbol, -ß_USD, -USD) %>%
  as.data.frame() 
h <- 21
label_col <- "alpha"
split_prop <- 0.7


test_2 <-  ml_class(df_decom, h, label_col, split_prop, features = features)

test_2$model_tes

# Evaluate Testset -------------------------------------------------------------
## Evaluation Table
predictions <- test_2$model_test %>%
  select(glm_prediction_class, rf_prediction_class, xgb_prediction_class, simple_bench)

classificationEvaluation(label = test_2$model_test$class,  classification_prediction = predictions)


## correlation
test_2$model_test %>%
  select(glm_prediction_class_prob, rf_prediction_class_prob, xgb_prediction_class_prob)


