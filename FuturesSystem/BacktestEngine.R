# The first backtest function
#' all the data should be in long format
# Test Values
BETA=0
MAX_POSITION=0.4
MIN_POSITION = 0.05
DOLLAR=0
LONG_LEG_DOLLAR=1
SHORT_LEG_DOLLAR=1


BacktestEngine_1 <- function(RETURNS, OBJECTIVES, SIGNAL, BETAS, REBALANCE_FREQ="weekly",
                             BETA=NULL,MAX_POSITION=NULL,MIN_POSITION=NULL,
                             DOLLAR=NULL, LONG_LEG_DOLLAR=NULL, SHORT_LEG_DOLLAR=NULL){
  
  # Get rebalancing dates
  rebalance_dates <- RETURNS %>%
    na.omit() %>% 
    pull(date) %>%
    unique() %>%
    .[myEndpoints(., endpoint=REBALANCE_FREQ)] %>% 
    na.omit()
  
  # calculate rebalance weights
  rebalance_weights <- rebalance_dates %>% 
    lapply(., function(d){
      print(d)
      
      objectives <- OBJECTIVES %>% filter(date==d) %>% arrange(names) %>%  pull(OBJECTIVE) %>% as.numeric()
      beta <- BETAS %>% filter(date==d) %>% arrange(names) %>%  pull(ß_Mkt) %>% as.numeric() %>% round(.,3)
      exposure <- SIGNAL %>% filter(date==d) %>% arrange(names) %>%  pull(SIGNAL) %>% as.numeric()
      c_names <- SIGNAL %>% arrange(names) %>%  pull(names) %>% unique()
      
      checksum <- sum(is.na(objectives)) + sum(is.na(beta)) + sum(is.na(exposure))
      check1 <- checksum == 0
      check2 <- (length(objectives) == length(beta)) & (length(objectives) == length(exposure))
      check3 <- length(objectives) != 0
      
      print(c_names)
      if(check1&check2&check3){
        # print(d)
        # print(check2)
        # print(objectives)
        weight <- PortfolioOptimizer(objectives=objectives,
                                     exposure=exposure,
                                     beta=beta,
                                     BETA=BETA,
                                     MAX_POSITION=MAX_POSITION,
                                     MIN_POSITION=MIN_POSITION,
                                     DOLLAR=DOLLAR,
                                     LONG_LEG_DOLLAR=LONG_LEG_DOLLAR,
                                     SHORT_LEG_DOLLAR=SHORT_LEG_DOLLAR,
                                     VERBOSE=FALSE)
        weight <- data.frame(
          "date"=d,
          weight %>% t()
        )  
      } else{
        weight <- data.frame(
          "date"=d,
          rep(NA, length(c_names)) %>% t()
        )
      }
      
      colnames(weight) <- c("date", c_names)
      return(weight)
    }) 
  
  rebalance_weights <-rebalance_weights %>% 
    rbindlist()
  
  # Merge wiht whole dataset 
  rebalance_weights <- rebalance_weights %>% 
    pivot_longer(cols = colnames(.)[colnames(.)!="date"],
                 names_to = "names", values_to = "weight") %>% 
    left_join(RETURNS, .) %>% 
    arrange(names, date)
  
  # Calculate weights in between rebalancing
  res <- rebalance_weights %>% 
    na.omit() %>% 
    pull(names) %>% 
    unique() %>% 
    lapply(., function(n){
      tmp_df <- rebalance_weights %>% filter(names==n)
      
      for(row in 2:(nrow(tmp_df)-1)){
        if(is.na(tmp_df[row, "weight"])){
          exposure <- tmp_df[row-1, "weight"] / abs(tmp_df[row-1, "weight"])
          tmp_df[row, "weight"] <- tmp_df[row-1, "weight"] * (1 + exposure * tmp_df[row, "return"])
        }
      }
      return(tmp_df)
    }) %>% 
    rbindlist() %>% 
    na.omit() %>% 
    as.data.frame() %>% 
    mutate(contribution = return*weight)
  
  # Aggregate to portfolio level
  final_res <- res %>% 
    select(date, names, contribution) %>% 
    pivot_wider(names_from = names, values_from = contribution) %>% 
    replace(is.na(.), 0) %>% 
    mutate(portfolio_return = rowSums(.[,-1])) %>% 
    select(date, portfolio_return)
  
  
  return(list("portfolio"=final_res,
              "contribution"=res))
}
# 
# 
# tester <- final_res %>% 
#   left_join(., Benchmark) %>% 
#   mutate(portEquity = cum.ret(portfolio_return),
#          benchEquity = cum.ret(Mkt))
# tester %>% 
#   ggplot(.) +
#   geom_line(aes(x=date, y=portEquity, color="Portfolio")) +
#   geom_line(aes(x=date, y=benchEquity, color="Benchmark")) +
#   theme_tq()
# 
# res %>% 
#   select(date, names, weight) %>% 
#   ggplot(.) +
#   geom_line(aes(x=date, y=weight, color=names))

# Maximises a first order objective based on some constraints
PortfolioOptimizer <- function(objectives, exposure, beta,
                               BETA=NULL,MAX_POSITION=NULL, MIN_POSITION=0.01,
                               DOLLAR=0, LONG_LEG_DOLLAR=1, SHORT_LEG_DOLLAR=1,
                               VERBOSE=FALSE){
  
  n_assets <- length(objectives)
  
  # Construct initial model
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:n_assets, type = "continuous") %>%
    # maximize the preferences
    set_objective(sum_expr(objectives[i] * x[i], i = 1:n_assets), sense = "max") %>%
    # Constrain to desired exposure direction
    add_constraint((ifelse(exposure[i]<0,-1,1) * x[i]) >= MIN_POSITION, i = 1:n_assets)
    
  # Add Investment Constraints -------------------------------------------------
  # Total Dollar exposure
  if(!is.null(DOLLAR)){
    model <- model %>%
      # Dollar Neutral
      add_constraint(sum_expr(x[i], i = 1:n_assets) == DOLLAR)
  }
  # Leg dollar exposure
  if(!is.null(SHORT_LEG_DOLLAR)){
    model <- model %>%
      add_constraint(sum_expr(ifelse(exposure[i]<0,-1,0) * x[i], i = 1:n_assets) == SHORT_LEG_DOLLAR) 
  }
  if(!is.null(LONG_LEG_DOLLAR)){
    model <- model %>%
      add_constraint(sum_expr(ifelse(exposure[i]>0,1,0) * x[i], i = 1:n_assets) == LONG_LEG_DOLLAR)
  }
  
  # Constrain max position size (Important Constraint)
  if(!is.null(MAX_POSITION)){
    model <- model %>%
      add_constraint((ifelse(exposure[i]<0,-1,1) * x[i]) <= MAX_POSITION, i = 1:n_assets)
  }

  # Add Beta Constraints -------------------------------------------------------
  # Total Beta exposure
  if(!is.null(BETA)){
    model <- model %>%
      # Beta Neutral
      add_constraint(sum_expr(beta[i] * x[i], i = 1:n_assets) == BETA) 
      # add_constraint(sum_expr(beta[i] * x[i], i = 1:n_assets) >= BETA-0.1) %>%
      # add_constraint(sum_expr(beta[i] * x[i], i = 1:n_assets) <= BETA+0.1)
  }
  
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = VERBOSE))
  return(as.numeric(result$solution))
}
