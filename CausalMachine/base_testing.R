source("R/dependencies.R")


# base data
# data <- tq_get(c("^STOXX", "EXSA.DE"), from = "1900-01-01")
data <- tq_get("^STOXX", from = "1900-01-01")


# Calculate 