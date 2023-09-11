# Idea
#' L/S as well as LO
#' Maximise Curve Carry
#' 
#' Constrain beta to 1 for LO and 0 for LS
#' Dollar Neutral for LS and 100% Invested for LO
#' 
source("R/dependencies.R")
library(Rsolnp)

beta <- c(1.4, 1.2, 0.8, 0.92)
carry <- c(-0.03, -0.025, -0.032, -0.02)


opt_func <- function(x) {
  sum(x*carry)
}


equal <- function(x) {
  sum(x*beta) 
}

#the optimiser - minimises by default
solnp(pars = rep(1/length(carry), length(carry)), #starting values (random - obviously need to be positive and sum to 15)
      opt_func, #function to optimise
      eqfun=equal, #equality function 
      eqB=0,   #the equality constraint
      LB=rep(-1, length(carry)),
      UB=rep(-1, length(carry))) 


# Constraints stuff
library(lpSolve)



beta <- c(1.4, 1.2, 0.8, 0.92)
carry <- c(-0.03, -0.025, -0.032, -0.02)
exposure <- c(-1, 1, -1, 1)


# Constraint Matrix
f.constr <- matrix(data = 0, nrow=length(carry), ncol = length(carry))
diag(f.constr) <- 1
# f.constr <- 

# Constraint Direction
f.dir <- ifelse(exposure >= 0,">", "<")

# Constraint right hand side
f.rhs <- rep(0, length(carry))



mod <- lp(direction = "max",
          objective.in = carry,
          const.mat = f.constr,
          const.dir = f.dir,
          const.rhs = f.rhs)



library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

beta <- c(1.4, 1.2, 0.8, 0.92)
carry <- c(-0.03, -0.025, -0.032, -0.02)
exposure <- c(-1, 1, -1, 1)
price <- c(12, 20, 40, 16)
leg_exposure <- 10000
max_weight <- 3 * 1/length(carry)

n_sectors <- length(carry)

model <- MIPModel() %>%
  
  add_variable(x[i], i = 1:n_sectors, type = "integer") %>%
  
  # maximize the preferences
  set_objective(sum_expr(carry[i] * price[i]/leg_exposure * x[i], i = 1:n_sectors), sense = "max") %>%
  
  # Dollar Neutral
  add_constraint(sum_expr(price[i] * x[i], i = 1:n_sectors) == 0) %>%
  
  # Constrain nominal exposure on each leg
  add_constraint(sum_expr(ifelse(exposure[i]<0,-1,0) * price[i] * x[i], i = 1:n_sectors) == leg_exposure) %>%
  add_constraint(sum_expr(ifelse(exposure[i]>0,1,0) * price[i] * x[i], i = 1:n_sectors) == leg_exposure) %>%

  # Constrain Long/Short exposure
  add_constraint((ifelse(exposure[i]<0,1,0) * x[i]) <= -1, i = 1:n_sectors) %>%
  add_constraint((ifelse(exposure[i]>0,1,0) * x[i]) >= 1, i = 1:n_sectors) %>%
  
  # Constrain max size
  add_constraint((ifelse(exposure[i]<0,-1,1) * x[i] * price[i]/leg_exposure) <= max_weight, i = 1:n_sectors) %>%
  
  # Beta Neutral
  # add_constraint(sum_expr(beta[i] * price[i]/leg_exposure * x[i], i = 1:n_sectors) == 0)
  add_constraint(sum_expr(beta[i] * price[i]/leg_exposure * x[i], i = 1:n_sectors) >= -0.1) %>%
  add_constraint(sum_expr(beta[i] * price[i]/leg_exposure * x[i], i = 1:n_sectors) <= 0.1)

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

result$solution



# Continous
model <- MIPModel() %>%
  
  add_variable(x[i], i = 1:n_sectors, type = "continuous") %>%
  
  # maximize the preferences
  set_objective(sum_expr(carry[i] * x[i], i = 1:n_sectors), sense = "max") %>%
  
  
  # Dollar Neutral
  add_constraint(sum_expr(x[i], i = 1:n_sectors) == 0) %>%
  
  # Constrain nominal exposure on each leg
  add_constraint(sum_expr(ifelse(exposure[i]<0,-1,0) * x[i], i = 1:n_sectors) == 1) %>%
  add_constraint(sum_expr(ifelse(exposure[i]>0,1,0) * x[i], i = 1:n_sectors) == 1) %>%
  
  # Constrain Long/Short exposure
  add_constraint((ifelse(exposure[i]<0,1,0) * x[i]) <= -0.01, i = 1:n_sectors) %>%
  add_constraint((ifelse(exposure[i]>0,1,0) * x[i]) >= 0.01, i = 1:n_sectors) %>%
  
  # Constrain max size
  add_constraint((ifelse(exposure[i]<0,-1,1) * x[i]) <= max_weight, i = 1:n_sectors) %>%
  
  # Beta Neutral
  add_constraint(sum_expr(beta[i] * x[i], i = 1:n_sectors) == 0)
  # add_constraint(sum_expr(beta[i] * x[i], i = 1:n_sectors) >= -0.1) %>%
  # add_constraint(sum_expr(beta[i] * x[i], i = 1:n_sectors) <= 0.1)
model

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

sum(result$solution * beta)
