decompose_returns <- function(df_return, X, regression_lag){
  
  ## Prepare
  feature_length <- (X %>% ncol())
  feature_names <- colnames(X)[-1]
  
  res_df <- df_return %>% 
    left_join(X, by = "date") %>% 
    na.omit()
  
  res_df <- res_df %>%
    pull(names) %>% 
    unique() %>% 
    lapply(., function(x){
      
      print_timed("Decomposing: ", x, " with regression_lag: ", regression_lag)
      
      # Filter and prepare
      temp_df <- res_df %>% 
        filter(names == x) 
      
      df_coe <- matrix(ncol= ncol(X), nrow = nrow(temp_df)) %>%
        as.data.frame()
      colnames(df_coe) <- c("alpha", paste0("ß_", feature_names))
      
      ## Estimated regression
      df_coe[-c(1:(regression_lag-1)),] <- temp_df %>% 
        .[,-c(1:2)] %>% 
        rollapplyr(., width = regression_lag, FUN = function(data) {
          reg <- lm(as.numeric(data[,1]) ~ data[,c(2:feature_length)])
          return(coef(reg))
        }, by.column = FALSE)
      
      ## Calculate y_hat anf Epsilon
      temp_df <- temp_df %>%
        cbind(., df_coe)
      
      temp_df$y_hat <- temp_df$alpha +
        rowSums(df_coe[,-c(1)] *(temp_df %>%
                                   select(all_of(feature_names))))
      
      temp_df$epsilon <-temp_df$return -
        temp_df$y_hat 
      
      return(temp_df)
      
    }) %>% 
    rbindlist() %>% 
    as.data.frame()
  
  return(res_df)
}


gen_decompose <- function(df_return, regression_lag){
  
  c(2:ncol(df_return)) %>% 
    lapply(., function(x){
      
      ## Slice and dice
      X <- df_return[,-c(1,x)]
      y <- df_return[[x]]
      feature_names <- colnames(df_return)[-c(1,x)]
      feature_length <- length(feature_names)
      
      ## Prep result df
      df_coe <- matrix(ncol= ncol(X)+1, nrow = nrow(df_return)) %>%
        as.data.frame()
      colnames(df_coe) <- c("alpha", paste0("ß_", feature_names))
      
      ## Estimated regression
      df_coe[-c(1:(regression_lag-1)),] <- df_return %>% 
        .[,-1] %>% 
        rollapplyr(., width = regression_lag, FUN = function(reg_data) {
          reg <- lm(as.numeric(reg_data[,x-1]) ~ reg_data[,-(x-1)])
          return(coef(reg))
        }, by.column = FALSE)
      
      ## Join Other Stuff
      df_coe$y_hat <- df_coe$alpha +
        rowSums(df_coe[,-1] *(df_return %>%
                                select(all_of(feature_names))))
      df_coe$date <- df_return$date
      df_coe <- df_coe %>% 
        cbind(., X)
      df_coe$return <- y
      df_coe$epsilon <- df_coe$return - df_coe$y_hat
      df_coe$symbol <- colnames(df_return)[x]
      return(df_coe)
    }) %>%
    rbindlist(., fill = T) %>% 
    return()
}


cor_Clustering <- function(){
  
  CoMa <- df %>% 
    select(-date) %>% 
    cor() %>% 
    as.data.frame()

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
  
}



