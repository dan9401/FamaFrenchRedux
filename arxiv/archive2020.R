# data_80_15 <- data_by_month("1980-01-01", "2015-12-31")
# data_63_15 <- data_by_month("1963-07-01", "2015-12-31")

# beta63_90 <- repFunc2(time = "63_90", variables = "beta")
# beta80_15 <- repFunc2(time = "80_15", variables = "beta")
# beta63_15 <- repFunc2(time = "63_15", variables = "beta")
# beta63_18 <- repFunc2(time = "63_18", variables = "beta")

# pltFunc("63_90", "beta", "win", "0.99")

# d1980 <- tmp_link %>% data.frame() %>% filter(year(date) == 1980) %>% select(date, gvkey, permno, linkdt, linkenddt)
# 
# q <- "select column_name 
# from information_schema.columns
# where table_schema = 'crsp'
# and table_name = 'msenames'"
# get_data(q)
# 
# q <- "select column_name 
# from information_schema.columns
# where table_schema = 'comp'
# and table_name = 'funda'"
# get_data(q)
# 
# q <- "select comnam from crsp.msenames where permno = 22779"
# get_data(q)
# 
# q <- "select conm from comp.funda where gvkey = '007585'"
# get_data(q)

# Jan 25 e2p

##########################
# data_63_90 <- data_by_month("1963-07-01", "1990-12-31")
# data_80_15 <- data_by_month("1980-01-01", "2015-12-31")
# data_63_15 <- data_by_month("1963-07-01", "2015-12-31")
# data_63_18 <- data_by_month("1963-07-01", "2018-12-31")
# 
# beta63_90 <- repFunc2(time = "63_90", variables = "beta")
# beta80_15 <- repFunc2(time = "80_15", variables = "beta")
# beta63_15 <- repFunc2(time = "63_15", variables = "beta")
# beta63_18 <- repFunc2(time = "63_18", variables = "beta")
# 
# size63_90 <- repFunc2(time = "63_90", variables = "size")
# size80_15 <- repFunc2(time = "80_15", variables = "size")
# size63_15 <- repFunc2(time = "63_15", variables = "size")
# size63_18 <- repFunc2(time = "63_18", variables = "size")
# 
# b2m63_90 <- repFunc2(time = "63_90", variables = "b2m")
# b2m80_15 <- repFunc2(time = "80_15", variables = "b2m")
# b2m63_15 <- repFunc2(time = "63_15", variables = "b2m")
# b2m63_18 <- repFunc2(time = "63_18", variables = "b2m")
# 
# e2p63_90 <- repFunc2(time = "63_90", variables = "e2p", rob_trim = 0.005)
# e2p80_15 <- repFunc2(time = "80_15", variables = "e2p", rob_trim = 0.005)
# e2p63_15 <- repFunc2(time = "63_15", variables = "e2p", rob_trim = 0.005)
# e2p63_18 <- repFunc2(time = "63_18", variables = "e2p", rob_trim = 0.005)
# 
# 
# e2p63_90$est$est_lm_win$stat
# e2p63_90$est$est_lm_trim$stat
# e2p63_90$est$est_mm_95$stat
# e2p63_90$est$est_mm_99$stat
# e2p63_90$est$est_mm_999$stat
# 
# e2p80_15$est$est_lm_win$stat
# e2p80_15$est$est_lm_trim$stat
# e2p80_15$est$est_mm_95$stat
# e2p80_15$est$est_mm_99$stat
# e2p80_15$est$est_mm_999$stat
# 
# e2p63_15$est$est_lm_win$stat
# e2p63_15$est$est_lm_trim$stat
# e2p63_15$est$est_mm_95$stat
# e2p63_15$est$est_mm_99$stat
# e2p63_15$est$est_mm_999$stat
# 
# beta63_18$est$est_lm_win$stat
# beta63_18$est$est_lm_trim$stat
# beta63_18$est$est_mm_95$stat
# beta63_18$est$est_mm_99$stat
# beta63_18$est$est_mm_999$stat
# 
# e2p

