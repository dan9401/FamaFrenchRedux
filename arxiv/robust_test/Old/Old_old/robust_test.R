require(tidyverse)
require(lubridate)
require(RobStatTM)
rm(list = ls())

# functions used
winsorize <- function(x, vars, level) {
  for (i in seq_along(vars)) {
    q <- quantile(x[[vars[i]]], c(level, 1-level), na.rm = TRUE)
    x[[ vars[i] ]][ x[[ vars[i] ]] < q[[1]] ] = q[[1]]
    x[[ vars[i] ]][ x[[ vars[i] ]] > q[[2]] ] = q[[2]]
  }
  return(x)
}

trim <- function(x, vars, level) {
  for (i in seq_along(vars)) {
    q <- quantile(x[[vars[i]]], c(level, 1-level), na.rm = TRUE)
    x <- x[ x[[vars[i] ]] > q[[1]] & x[[ vars[i] ]] < q[[2]] ,]
  }
  return(x)
}

regFunc <- function(data, variables, regression = c('linear', 'robustMM'), 
                    outliers = c("winsorize", "trim"), control = lmrobdet.control(),
                    robust_trim = FALSE) {
  formula <- as.formula(paste('adj_ret * 100 ~', paste(variables, collapse = ' + ')))
  regression <- match.arg(regression)
  outliers <- match.arg(outliers)
  
  if (regression == 'linear') {
    data <- data[, c("permno", "adj_ret", variables)] %>% 
      na.omit()
    if (dim(data)[1] == 0) {
      list()
    }
    if (outliers == "winsorize") {
      data <- winsorize(data, variables, level = 0.005)
    } else if (outliers == "trim") {
      data <- trim(data, variables, level = 0.005)
    } else {
      stop("outliers must be one of c('winsorize', 'trim')")
    }
    lm(formula, data)
  } else if (regression == 'robustMM') {
    data <- data[, c("permno", "adj_ret", variables)] %>% 
      na.omit()
    if (dim(data)[1] == 0) {
      list()
    }
    if (robust_trim == TRUE) {
      if (outliers == "winsorize") {
        data <- winsorize(data, variables, level = 0.005)
      } else if (outliers == "trim") {
        data <- trim(data, variables, level = 0.005)
      } else {
        stop("outliers must be one of c('winsorize', 'trim')")
      }
    }
    lmrobdetMM(formula, data, control = control)
  } else {
    stop("regression must be one of c('linear', 'robustMM')")
  }
}

# data used
load(file = "e2p_80_15_slopes.Rdata")
load(file = "data_80_15.Rdata")

# 7 months that e2p slopes have a diff
e2p_slope_diff <- e2p_80_15_slopes[,1] - e2p_80_15_slopes[,2]
test_months <- names(e2p_slope_diff[abs(e2p_slope_diff) > 5])
test_data <- data_80_15[test_months]

# regression example
test_res <- regFunc(test_data[[1]], "e2p", regression = "robustMM", outliers = "win", robust_trim = TRUE)

