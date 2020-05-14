# main regression function, data is a month
regFunc <- function(data, variables, regression = c('linear', 'robustMM'),
                    outliers = c("winsorize", "trim"), control = lmrobdet.control(),
                    rob_trim = FALSE) {
  formula <- as.formula(paste('adj_ret * 100 ~', paste(variables, collapse = ' + ')))
  regression <- match.arg(regression)
  outliers <- match.arg(outliers)

  if (regression == 'linear') {
      data <- data[, c("permno", "adj_ret", variables)] %>%
        na.omit()
      if (dim(data)[1] == 0) {
        list()
      }
      data <- outliers_by_var(data, variables, outliers, level = 0.005)
      lm(formula, data)
    } else if (regression == 'robustMM') {
    data <- data[, c("permno", "adj_ret", variables)] %>%
      na.omit()
    if (dim(data)[1] == 0) {
      list()
    }
    if (rob_trim) {
      # in the case of robust regression, only e2p related variables need to be winsorized/trimmed
      variables <- variables[grepl("e2p", variables)]
      data <- outliers_by_var(data, variables, outliers, level = rob_trim)
    }
    lmrobdetMM(formula, data, control = control)
  } else {
    stop("regression must be one of c('linear', 'robustMM')")
  }
}

# apply regfunc on the list of months
repFunc <- function(estList, reg) {
    res <- t(sapply(estList, function(x) {
        coef <- summary(x)$coefficients[, c(1,3)]
        scale <- x$scale
        tmp <- c(coef[1,], coef[2,], scale)
        if (reg == "linear") {
            names(tmp) <- c(rownames(coef)[1], "t value", rownames(coef)[2], "t value")
        } else if (reg == "robust") {
            names(tmp) <- c(rownames(coef)[1], "t value", rownames(coef)[2], "t value", "scale")
        } else {
            stop("reg must be one of c('linear','robust').")
        }
        tmp
    }))
    est <- t(sapply(estList, function(x) x$coefficients))
    mean <- colMeans(est)
    tStat <- mean/apply(est, 2, function(x) sd(x)/sqrt(length(x)))
    NWtStat <- mean/apply(est, 2, function(x) sqrt(sandwich::lrvar(x, type = "Newey-West")))
    list(res = res, stat = t(rbind(mean, tStat, NWtStat)))
}

# apply different regressions on a data set for a(some) variable(s)
repFunc2 <- function(data, variables, rob_trim = FALSE) {
    lm_win <- lapply(data, regFunc, variables = variables, regression = "linear", outliers = "winsorize", rob_trim = rob_trim)
    lm_trim <- lapply(data, regFunc, variables = variables, regression = "linear", outliers = "trim", rob_trim = rob_trim)
    mm_95 <- lapply(data, regFunc, variables = variables, regression = "robustMM", control = lmrobdet.control(efficiency = 0.95), rob_trim = rob_trim)
    mm_99 <- lapply(data, regFunc, variables = variables, regression = "robustMM", control = lmrobdet.control(efficiency = 0.99), rob_trim = rob_trim)
    mm_999 <- lapply(data, regFunc, variables = variables, regression = "robustMM", control = lmrobdet.control(efficiency = 0.999), rob_trim = rob_trim)

    est_lm_win <- repFunc(lm_win, reg = "linear")
    est_lm_trim <- repFunc(lm_trim, reg = "linear")
    est_mm_95 <- repFunc(mm_95, reg = "robust")
    est_mm_99 <- repFunc(mm_99, reg = "robust")
    est_mm_999 <- repFunc(mm_999, reg = "robust")

    list(lm_win = list(res = lm_win, est = est_lm_win),
         lm_trim = list(res = lm_trim, est = est_lm_trim),
         mm_95 = list(res = mm_95, est = est_mm_95),
         mm_99 = list(res = mm_99, est = est_mm_99),
         mm_999 = list(res = mm_999, est = est_mm_999))
}

repFunc3 <- function(data, n, rob_trim=FALSE, e2p=FALSE) {
  res <- list()
  combs <- vars_comb(n)
  for (i in 1:length(combs)) {
    vars = combs[[i]]

    if (e2p==FALSE) {
      res[[paste(vars, collapse = "_")]] <- extractRes(repFunc2(data, variables = vars))
    } else if ("e2p" %in% vars) {
      res[[paste(vars, collapse = "_")]] <- extractRes(repFunc2(data, variables = vars, rob_trim = rob_trim))
    }
    
  }
  return(res)
}

repFuncEP <- function(data, variables, rob_trim = FALSE) {
  mm_50 <- lapply(data, regFunc, variables = variables, regression = "robustMM", control = lmrobdet.control(efficiency = 0.5), rob_trim = rob_trim)
  mm_70 <- lapply(data, regFunc, variables = variables, regression = "robustMM", control = lmrobdet.control(efficiency = 0.7), rob_trim = rob_trim)

  est_mm_50 <- repFunc(mm_50, reg = "robust")
  est_mm_70 <- repFunc(mm_70, reg = "robust")

  extractRes(list(mm_50 = list(res = mm_50, est = est_mm_50),
                  mm_70 = list(res = mm_70, est = est_mm_70)))
}

repFuncEP2 <- function(data, variables, rob_trim = FALSE) {
  mm_50 <- lapply(data, regFunc, variables = variables, regression = "robustMM", control = lmrobdet.control(efficiency = 0.5), rob_trim = rob_trim)
  mm_70 <- lapply(data, regFunc, variables = variables, regression = "robustMM", control = lmrobdet.control(efficiency = 0.7), rob_trim = rob_trim)
  mm_95 <- lapply(data, regFunc, variables = variables, regression = "robustMM", control = lmrobdet.control(efficiency = 0.95), rob_trim = rob_trim)
  mm_99 <- lapply(data, regFunc, variables = variables, regression = "robustMM", control = lmrobdet.control(efficiency = 0.99), rob_trim = rob_trim)
  mm_999 <- lapply(data, regFunc, variables = variables, regression = "robustMM", control = lmrobdet.control(efficiency = 0.999), rob_trim = rob_trim)
  
  est_mm_50 <- repFunc(mm_50, reg = "robust")
  est_mm_70 <- repFunc(mm_70, reg = "robust")
  est_mm_95 <- repFunc(mm_95, reg = "robust")
  est_mm_99 <- repFunc(mm_99, reg = "robust")
  est_mm_999 <- repFunc(mm_999, reg = "robust")
  
  extractRes(list(mm_50 = list(res = mm_50, est = est_mm_50),
                  mm_70 = list(res = mm_70, est = est_mm_70),
                  mm_95 = list(res = mm_95, est = est_mm_95),
                  mm_99 = list(res = mm_99, est = est_mm_99),
                  mm_999 = list(res = mm_999, est = est_mm_999)))
}


extractEst <- function(rep) {
  est <- list()
  for (i in 1:length(rep)) {
    tmp <- rep[[i]]$stat
    est[[ names(rep)[[i]] ]] <- tmp[rownames(tmp) != "(Intercept)", colnames(tmp) != "NWtStat"]
  }
  return(do.call(rbind, est))
}

extractEst2 <- function(rep) {
  est <- list()
  for (i in 1:length(rep)) {
    est[[ names(rep)[[i]] ]] <- extractEst(rep[[i]])
  }
  return(est)
}

# find all combinations of a set of variables
# and their interactions
vars_comb <- function(n) {
  vars<- c("size", "b2m", "beta", "e2p")
  comb <- combn(vars, n)
  combs <- list()
  interaction <- function(combi) {
    interactions <- combi
    if (length(combi) > 1) {
      comb2 <- combn(combi, 2)
      for(i in 1:ncol(comb2)) {
        interactions <- c(interactions, paste(comb2[,i], collapse = "_"))
      }
    }
    interactions
  }
  for (i in 1:ncol(comb)) {
    combs[[i]] <- interaction(comb[,i])
  }
  return(combs)
}

extractRes <- function(rep2) {
  rep = list()
  for (i in 1:length(rep2)) {
    rep[[names(rep2)[[i]] ]] <- rep2[[i]]$est
  }
  return(rep)
}

# get the post beta: 1 for each size-beta group
post_beta <- function(df){
  df_tmp <- df %>% na.omit()
  coeff <- lm(avg_rtn ~ vwretd + vwretd_l1, data = df_tmp)$coef
  # df <- filter(df, !is.na(beta))
  coeff[2:3]
}

# get data as a list of month's data
data_by_month <- function(start="1963-07-01", end="1990-12-31") {
  
  tmp_postBeta <- beta_processed %>%
    filter((date >= start) & (date < end))
  postBeta <- as.data.frame(t(sapply(split(tmp_postBeta, tmp_postBeta$g), post_beta)))
  postBeta$g <- rownames(postBeta)
  postBeta <- postBeta %>%
    separate(g, c("size_g", "beta_g"), "_") %>%
    mutate(beta = vwretd + vwretd_l1) %>%
    select(size_g, beta_g, beta) %>%
    mutate(size_g = as.numeric(size_g)) %>%
    mutate(beta_g = as.numeric(beta_g))
  
  tmp_postBeta <- left_join(data_groups, postBeta, by= c("size_g", "beta_g"))
  
  data_prepared <- data_processed %>%
    left_join(tmp_postBeta, by = c("date", "permno"))
  
  data_reg <- data_prepared %>%
    mutate(adj_ret = adj_ret.x) %>%
    select(date, permno, adj_ret, size, b2m, e2p, beta) %>%
    mutate(size_b2m = size * b2m) %>% 
    mutate(size_e2p = size * e2p) %>% 
    mutate(size_beta = size * beta) %>% 
    mutate(b2m_e2p = e2p * b2m) %>% 
    mutate(b2m_beta = beta * b2m) %>% 
    mutate(beta_e2p = beta * e2p) %>% 
    data.frame()
  
  data_months <- split(data_reg, data_reg$date)
  data_months <- data_months[start <= names(data_months) & names(data_months) <= end]
  data_months
}

# winsorize and trimming, only the variables in vars are winsorized / trimmed upon
# in the case of winsorize, the returns are not affected
# in the case of trimming, the returns are removed
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

# beta should not be winsorized/trimmed
# 
outliers_by_var <- function(data, variables, outliers, level) {
  if ("beta" %in% variables) {
    variables <- variables[variables != "beta"]
  }
  if (outliers == "winsorize") {
    data <- winsorize(data, variables, level = level)
  } else if (outliers == "trim") {
    data <- trim(data, variables, level = level)
  } else {
    stop("outliers must be one of c('winsorize', 'trim')")
  }
  return(data)
}
