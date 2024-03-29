# Dependencies
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(fpp3)
library(tsibble)
library(fable)
library(lubridate)
library(IBrokers)
library(tidymodels)
library(fable)
library(tsibbledata)
library(tsibble)
library(rollRegres)
library(EpsilonUtility)
library(data.table)
library(purrr)
library(tseries)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(kernlab)
library(feasts)
library(PortfolioAnalytics) 
library(DEoptim)
library(bidask)
library(plotly)
library(vip)
library(forecast)
library(zoo)
library(imputeTS)
library(pbapply)
library(ape)
library(igraph)
library(signnet)
library(RQuantLib)

## Source Scripts
source("R/decomposition_functions.R")
source("R/feature_pipeline.R")
source("R/model_functions.R")
source("R/filter_functions.R")
source("R/helper_functions.R")
source("R/portfolio_construction.R")
source("R/regression_model.R")
source("R/classification_model.R")
source("R/CONFIG.R")
source("R/data_loader.R")
source("R/Elastic_Net.R")
source("R/IBKR_functions.R")
source("FuturesSystem/BacktestEngine.R")
