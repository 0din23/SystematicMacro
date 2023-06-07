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

benchmark <- tq_get("EXSA.DE", from = "2010-01-01")

ff_data <- read.csv(
  "C:/0cap_2/MacroMan/SectorRotationPaper/Europe_5_Factors_Daily.csv",
  sep = ";") %>% 
  mutate(date = as.Date(Date, format=c("%d.%m.%Y")))


################################################################################
# REQUIREMENTS #
################################################################################
#' The end goal of this whole exercise is to build a long short portfolio that earns a 
#' hopefully significant amount of alpha. To do this I want to decompose the
#' time series of returns of the assets into its additive components. Those components are
#' the linear betas to various factors, the assets alpha as well as the remaining noise.
#' 
#' There are a few parameters that have to be set. First and foremost is the length of the 
#' rolling window of the regression. The approach is used since one would expect negligible alpha in such a decomposition.
#' The 
#' 

################################################################################
# LOGBASED NON OVERLAPPING DECOMPOSITION #
################################################################################

## inputs
h <- 20
regression_lag <- h 

data <- data %>% 
  group_by(names) %>% 
  mutate(
    return_1B = RETURN(adjusted),
    log_return_1B = CHANGE(log(adjusted)),
    
    return_hB = RETURN(adjusted, h),
    log_return_hB = CHANGE(log(adjusted), h)
  ) %>% 
  left_join(., benchmark %>% 
              mutate(
                return_1B = RETURN(adjusted),
                log_return_1B = CHANGE(log(adjusted)),
                
                return_hB = RETURN(adjusted, h),
                log_return_hB = CHANGE(log(adjusted), h)
              ) %>% 
              select(date, return_1B, log_return_1B, return_hB, log_return_hB)
            , by = "date", suffix = c("", "_bench")) %>% 
  na.omit() %>% 
  group_by(names) %>% 
  mutate(
    ß_Mkt_1B = roll_regres(return_1B ~ return_1B_bench, width = regression_lag)[["coefs"]][,2],
    alpha_1B = roll_regres(return_1B ~ return_1B_bench, width = regression_lag)[["coefs"]][,1],
    y_hat_1B = alpha_1B + ß_Mkt_1B * return_1B_bench,
    epsilon_1B = return_1B - y_hat_1B,
    
    
    log_ß_Mkt_1B = roll_regres(log_return_1B ~ log_return_1B_bench, width = regression_lag)[["coefs"]][,2],
    log_alpha_1B = roll_regres(log_return_1B ~ log_return_1B_bench, width = regression_lag)[["coefs"]][,1],
    log_y_hat_1B = log_alpha_1B + log_ß_Mkt_1B * log_return_1B_bench,
    log_epsilon_1B = log_return_1B - log_y_hat_1B,
    
    ## aggregate to hB
    # epsilon_1B_bar_hB = SMA(epsilon_1B, h),
    # alpha_1B_bar = alpha_1B + epsilon_1B_bar_hB,
    
    log_epsilon_1B_bar_hB = SMA(log_epsilon_1B, h),
    log_alpha_1B_bar = log_alpha_1B + log_epsilon_1B_bar_hB,
    log_y_hat_hB = h * log_alpha_1B_bar + log_ß_Mkt_1B * log_return_hB_bench,
    log_epsilon_hB = log_return_hB - log_y_hat_hB,
    log_y_hat_hB_transformed = exp(h*log_alpha_1B_bar) * (1+ return_hB_bench)^(log_ß_Mkt_1B) -1,
    log_epsilon_hB_transformed = return_hB - log_y_hat_hB_transformed
  ) %>% 
  ungroup(names)
  


## Plot some stuff
data %>% 
  filter(names == "Basic_Resources") %>% 
  tail(260) %>% 
  ggplot(.) +
  geom_line(aes(x=date, y = h*log_alpha_1B_bar))


################################################################################
# TESTING BENCHMARK DECOMPOSITION #
################################################################################
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


RES_1 <- c(5, 20, 30, 40, 80, 130, 260, 520) %>% 
  lapply(., function(regression_lag){
    decompose_returns(df_return, X, regression_lag) %>% 
      mutate(LAG =regression_lag )
  })

#' The important question now is how to test and decide which regression lag is 
#' suitable for meassuring the "structural" outperformance of a equity sector.

## 
RES_1 %>% 
  lapply(., function(decom){
    
    reg <- lm(decom$return ~ decom$y_hat) %>% 
      summary()
    decom %>% 
      group_by(names) %>% 
      summarize(
        "LAG"= mean(LAG),
        "y_hat_perc" = mean(abs(y_hat), na.rm=T) / mean(abs(return), na.rm=T),
        "alpha_perc" = mean(abs(alpha), na.rm=T) / mean(abs(return), na.rm=T),
        "epsilon_perc" = mean(abs(epsilon), na.rm=T) / mean(abs(return), na.rm=T),
        "ß_dispersion" = sd(ß_Mkt, na.rm=T),
        "r2" = reg$r.squared
      )
  }) %>% rbindlist() %>% 
  as.data.frame() %>% 
  group_by(LAG) %>% 
  summarize(
    "y_hat_perc" = mean(y_hat_perc, na.rm=T),
    "alpha_perc" =  mean(alpha_perc, na.rm=T),
    "epsilon_perc" =  mean(epsilon_perc, na.rm=T),
    "ß_dispersion" =mean(ß_dispersion, na.rm=T),
    "r2_mean" = mean(r2, na.rm=T)
  )


#' Now, how to chose a lag?
#' 
#' 


################################################################################
# Decomposing the alpha further #
################################################################################

df <- RES_1[[3]] %>% 
  select(date, names, alpha) %>% 
  pivot_wider(data = ., names_from = names, values_from = alpha) %>% 
  na.omit()

RES_2 <- c(20, 30, 40, 80, 130, 260, 520) %>% 
  lapply(., function(regression_lag){
    print(regression_lag)
    gen_decompose(df, regression_lag = regression_lag) %>% 
      mutate(LAG =regression_lag )
  })

count <-1
RES_2 %>% 
  lapply(., function(decom){
    print(count)
    count <<- count + 1
    reg <- lm(decom$return ~ decom$y_hat) %>% 
      summary()
    decom %>% 
      group_by(symbol) %>% 
      summarize(
        "LAG"= mean(LAG),
        "y_hat_perc" = mean(abs(y_hat), na.rm=T) / mean(abs(return), na.rm=T),
        "alpha_perc" = mean(abs(alpha), na.rm=T) / mean(abs(return), na.rm=T),
        "epsilon_perc" = mean(abs(epsilon), na.rm=T) / mean(abs(return), na.rm=T),
        "r2" = reg$r.squared
      )
  }) %>% rbindlist() %>% 
  as.data.frame() %>% 
  group_by(LAG) %>% 
  summarize(
    "y_hat_perc" = mean(y_hat_perc, na.rm=T),
    "alpha_perc" =  mean(alpha_perc, na.rm=T),
    "epsilon_perc" =  mean(epsilon_perc, na.rm=T),
    "r2_mean" = mean(r2, na.rm=T)
  )


#' What exactly is this? It means that the outperformance stocks show over the shorter term is hardly explainable 
#' by other 


################################################################################
# Regime Clustering #
################################################################################
CoMa <- df %>% 
  select(-date) %>% 
  cor() %>% 
  #round(.,4) %>% 
  as.data.frame()

tree <- mst(CoMa)

plot(tree)

CoMa %>% 
  apply(.,2,function(x){
    data.frame(
      "names"=rownames(CoMa),
      "corr"=x
    ) %>% 
      arrange(desc(corr)) %>% 
      pull(names)
  }) %>% 
  as.data.frame() 



for (k in 1:ncol(CoMa)){
  
  if(k == 1){
    BlMa <- CoMa %>% arrange(desc(all_of(colnames(CoMa)[1]))) %>% select(1)
  }
  
}

DiMa <- CoMa %>% 
  apply(.,2,function(x){
    1-x
  }) %>% 
  as.data.frame()


g <- sample_islands_signed(10,10,1,20)
signed_blockmodel(CoMa,k = 10,alpha = 0.5)


## Calculate DiMa
DiMa <- CoMa %>% 
  apply(.,2,function(x){
    1-x
  }) %>% 
  as.data.frame()

## Create initila Clusters
Clusters <- DiMa
res <- list()

for(k in 1:ncol(Clusters)){
  
  ## Find Cluster
  min_dist <- min(Clusters[Clusters!=0])
  if(abs(min_dist) >= 0.8) {
    break
  }
  temp_cl <- Clusters %>% 
    apply(.,2,function(x){min(Clusters[Clusters!=0]) %in% x}) 
  temp_cl <- temp_cl[temp_cl] %>% names()
  
  ## Check if already cluster, if not create if yes, add
  if(sum(temp_cl %in% names(res)) == 1){
    
    ### Find the cluster 
    cluster_no <- temp_cl[temp_cl %in% names(res)]
    new_add <- temp_cl[!(temp_cl %in% names(res))]
    res[[cluster_no]] <-  res[[cluster_no]] %>% append(., new_add)
    
  } else if(sum(temp_cl %in% names(res)) == 2){
    print("Done")
    break
  } else{
    cluster_no <- paste0("Cluster_", length(res)+1)
    res[[cluster_no]]<- temp_cl
  }
  
  ## create new DiMa
  Clusters_new <- Clusters[!(rownames(Clusters) %in% temp_cl), !(colnames(Clusters) %in% temp_cl)]
  
  ### If something has to be added to a cluster
  if(sum(temp_cl %in% names(res)) != 0){
    cluster_size <- length(res[[cluster_no]])
    cluster_distance <- Clusters[!(rownames(Clusters) %in% temp_cl), (colnames(Clusters) %in% temp_cl)]
    cluster_distance[,new_add] <- cluster_distance[,new_add] * (1/cluster_size)
    cluster_distance[,cluster_no] <- cluster_distance[,cluster_no] * ((cluster_size-1)/cluster_size)
    cluster_distance <- cluster_distance %>% rowSums()

  } else{
    cluster_distance <- Clusters[!(rownames(Clusters) %in% temp_cl), (colnames(Clusters) %in% temp_cl)] %>% 
      rowMeans()
  }
  
  Clusters_new[,cluster_no] <- cluster_distance
  Clusters_new <- Clusters_new %>% 
    rbind(., c(cluster_distance,0))
  
  rownames(Clusters_new)[nrow(Clusters_new)] <- cluster_no
  Clusters <- Clusters_new
  
}

res


df %>% 
  mutate(
    Cluster_1 = rowMeans(df %>% all_of(res$Cluster_1))
  )
################################################################################
# What exactly is this #
################################################################################











