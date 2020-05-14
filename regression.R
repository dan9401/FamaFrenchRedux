# require(RPostgres) # query
require(tidyverse) # data cleaning & wrangling
require(lubridate) # dates
require(RobStatTM) # robust regressions
rm(list = ls())

# source("queries.R")
# source("preprocess.R")
# source("beta.R")
# source("cleanup.R")
source("regFunc.R")

# load("data/data_groups.Rdata")
# load("data/data_processed.Rdata")
# load("data/beta_processed.Rdata")
# data_63_90 <- data_by_month("1963-07-01", "1990-12-31")
# data_80_18 <- data_by_month("1980-01-01", "2018-12-31")
load("data/data_63_90.Rdata")
load("data/data_80_18.Rdata")

# repFunc3: lm_win, lm_trim, mm_95, mm_99, mm_999
# repFunc3: for regression of all combinations
# to do: 1) beta trimming problem 2) interaction terms
# e2p argument: if FALSE then does not winsorize e2p before regression
# rob_trim: FALSE then no robust winsorization, or a numeric for the trimming ratio at each side (e.g. 0.005)
# n: the number of variables to be considered

# 60-90 regression result for all 4 variables
# lm_win, lm_trim, mm_95, mm_99, mm_999
# data for e2p regression is winsorized before robust regressions
res_1_63_90 <- repFunc3(data_63_90, n=1, rob_trim=0.005)
# the e2p regressions with 95, 99, 999 efficiency without winsorization
e2p_1_63_90 <- repFunc3(data_63_90, n=1, rob_trim=FALSE, e2p=TRUE)
# the e2p regressions with 50, 70 efficiency without winsorization
e2p_57_63_90 = repFuncEP(data_63_90, "e2p")
# the e2p regressions with 50, 70 efficiency with winsorization
e2p_57t_63_90 = repFuncEP(data_63_90, "e2p", rob_trim = 0.005)
save(res_1_63_90, file = "data/res/res_1_63_90.Rdata")
save(e2p_1_63_90, file = "data/res/e2p_1_63_90.Rdata")
save(e2p_57_63_90, file = "data/res/e2p_57_63_90.Rdata")
save(e2p_57t_80_18, file = "data/res/e2p_57t_80_18.Rdata")


res_1_80_18 <- repFunc3(data_80_18, n=1, rob_trim=0.005)
e2p_1_80_18 <- repFunc3(data_80_18, n=1, rob_trim=FALSE, e2p=TRUE)
e2p_57_80_18 = repFuncEP(data_80_18, "e2p")
e2p_57t_80_18 = repFuncEP(data_80_18, "e2p", rob_trim = 0.005)
save(res_1_80_18, file = "data/res/res_1_80_18.Rdata")
save(e2p_1_80_18, file = "data/res/e2p_1_80_18.Rdata")
save(e2p_57_63_90, file = "data/res/e2p_57__63_90.Rdata")
save(e2p_57t_80_18, file = "data/res/e2p_57t_80_18.Rdata")


# 2 warnings for res_2_63_90
res_2_63_90 <- repFunc3(data_63_90, n=2, rob_trim=0.005)
e2p_2_63_90 <- repFunc3(data_63_90, n=2, rob_trim=FALSE, e2p=TRUE)
save(res_2_63_90, file = "data/res/res_2_63_90.Rdata")
save(e2p_2_63_90, file = "data/res/e2p_2_63_90.Rdata")


# 1 warning & 1 warning
res_2_80_18 <- repFunc3(data_80_18, n=2, rob_trim=0.005)
e2p_2_80_18 <- repFunc3(data_80_18, n=2, rob_trim=FALSE, e2p=TRUE)
save(res_2_80_18, file = "data/res/res_2_80_18.Rdata")
save(e2p_2_80_18, file = "data/res/e2p_2_80_18.Rdata")


# warning for res_4_63_90 & e2p_4_63_90 too
# all same msg
# In lmrob.fit(X, y, control, init = S.init, mf = mf) :
#   M-step did NOT converge. Returning unconverged lM-estimate
res_4_63_90 <- repFunc3(data_63_90, n=4, rob_trim=0.005)
e2p_4_63_90 <- repFunc3(data_63_90, n=4, rob_trim=FALSE, e2p=TRUE)
save(res_4_63_90, file = "data/res/res_4_63_90.Rdata")
save(e2p_4_63_90, file = "data/res/e2p_4_63_90.Rdata")


# warning for res_4_80_18 * 2 as well
res_4_80_18 <- repFunc3(data_80_18, n=4, rob_trim=0.005)
e2p_4_80_18 <- repFunc3(data_80_18, n=4, rob_trim=FALSE, e2p=TRUE)
save(res_4_80_18, file = "data/res/res_4_80_18.Rdata")
save(e2p_4_80_18, file = "data/res/e2p_4_80_18.Rdata")


e2p_t = repFuncEP2(data_80_18, "e2p")
e2p_tt = repFuncEP2(data_80_18, "e2p", rob_trim = 0.005)
extractEst(e2p_t)
extractEst(e2p_tt)
