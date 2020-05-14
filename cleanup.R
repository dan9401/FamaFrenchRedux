# data_groups has return of each stock and its group
save(data_groups, file = "data/data_groups.Rdata")
# data_procesed has the fundamentals
save(data_processed, file = "data/data_processed.Rdata")
# beta_processed has the average return and groupings
save(beta_processed, file = "data/beta_processed.Rdata")

source("regFunc.R")
data_63_90 <- data_by_month("1963-07-01", "1990-12-31")
data_80_18 <- data_by_month("1980-01-01", "2018-12-31")
save(data_63_90, file = "data/data_63_90.Rdata")
save(data_80_18, file = "data/data_80_18.Rdata")

rm(list = ls())

load("data/data_63_90.Rdata")
load("data/data_80_18.Rdata")