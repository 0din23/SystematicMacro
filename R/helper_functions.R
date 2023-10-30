cum.ret <- function(vec){
  cumprod(x = c(1, vec +1))[-1]
}

businessday_seq <- function(start, end = Sys.Date(), ref = NULL){
  if(is.null(ref)){
    as.Date(as.Date(start):as.Date(end)) %>% 
      .[!(weekdays(.) %in% c("Samstag", "Sonntag"))] %>% 
      return()
  } else{
    tq_get(ref, from=start, to = end) %>% 
      pull(date) %>% 
      return()
  }
}

excess_autocorr <- function(a,b){
  a <- unlist(a)
  b <- unlist(b)
  
  reg_a <- lm(a ~ lag(a))
  reg_b <- lm(b ~ lag(b))
  
  res <- data.frame(
    "auto_correlation_alpha" = reg_a$coefficients[2],
    "auto_correlation_return" = reg_b$coefficients[2],
    "auto_correlation_excess" = reg_a$coefficients[2]- reg_b$coefficients[2],
    "alpha_r2" = summary(reg_a)$r.squared,
    "return_r2" = summary(reg_b)$r.squared,
    "excess_r2" = summary(reg_a)$r.squared-summary(reg_b)$r.squared
  )
  return(res)
}

myEndpoints <- function(date_vec, endpoint){
  
  if(endpoint == "monthly"){
    date_vec <- date_vec %>%
      month()
    date_vec_2 <- date_vec %>%
      lag()
  } else if(endpoint == "weekly"){
    date_vec <- date_vec %>%
      week()
    date_vec_2 <- date_vec %>%
      lag()
  } else if(endpoint == "quarterly"){
    date_vec <- date_vec %>%
      quarter()
    date_vec_2 <- date_vec %>%
      lag()
  } else if(endpoint == "halfyear"){
    date_vec <- date_vec %>%
      quarter()
    date_vec <- ifelse(date_vec<3,1,2)
    date_vec_2 <- date_vec %>%
      lag()
  }
  
  return(!(date_vec == date_vec_2))
}
