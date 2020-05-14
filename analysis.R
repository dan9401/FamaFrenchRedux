require(tidyverse)
require(lubridate)
require(RobStatTM)

source("regFunc.R")

# data prep for Doug

e2p80_18 <- repFunc2(data_80_18, variables = "e2p")
e2p80_18_t <- repFunc2(data_80_18, variables = "e2p", rob_trim = 0.005)

s1 <- e2p80_18$mm_95$est$res[,3]
s2 <- e2p80_18_t$mm_95$est$res[,3]

s_diff <- abs(s1 - s2)
test_months <- names(s_diff[s_diff > 5])
test_data <- data_80_18[test_months]

rob_dataset <- function(data, var) {
    
    remove2 <- function(var) {
        if (grepl("2", var)) {
            var <- gsub("2", "", var)
        }
        var
    }
    var <- remove2(var)
    for (i in 1:length(data)) {
        colnames(data[[i]]) <- lapply(colnames(data[[i]]), remove2)
    }
    
    raw = lapply(data, function(x) x[c("date", "permno", "adj_ret", var)])
    names(raw) <- lapply(names(raw), function(x) {
        paste(substr(x,3,4), 
              switch(as.numeric(substr(x,6,7)), 
                     "jan", "feb", "mar", "apr",
                     "may", "jun", "july", "aug",
                     "sep", "oct", "nov", "dec"), sep="")
        
        })
    nona <- lapply(raw, na.omit)
    win <- lapply(nona, function(x) winsorize(x, var, 0.005))
    tm <- lapply(nona, function(x) trim(x, var, 0.005))

    saveit <- function(..., string, file) {
        x <- list(...)
        names(x) <- string
        save(list=names(x), file=file, envir=list2env(x))
    }

    saveit(raw, string = paste(var, "_raw", sep=""), file = paste("data/for Doug/", var, "_raw.Rdata", sep=""))
    saveit(nona, string = paste(var, "_nona", sep=""), file = paste("data/for Doug/", var, "_nona.Rdata", sep=""))
    saveit(win, string = paste(var, "_win", sep=""), file = paste("data/for Doug/", var, "_win.Rdata", sep=""))
    saveit(tm, string = paste(var, "_trim", sep=""), file = paste("data/for Doug/", var, "_trim.Rdata", sep=""))
}

rob_dataset(test_data, "size")
rob_dataset(test_data, "b2m")
rob_dataset(test_data, "beta")
rob_dataset(test_data, "e2p")

load("data/for Doug/bm_raw.Rdata")
load("data/for Doug/bm_nona.Rdata")
load("data/for Doug/bm_win.Rdata")
load("data/for Doug/bm_trim.Rdata")


# high missing ratio for b2m
# high missing ratio for beta
# list variables for calculating each variable
# 16 month outliers
# skew-t fit for e2p trimmed
# found normal month for qqplots
# spgmi
# NYSE deciles & MSCI indexes
# 1. subset by deleting lowest 5% and 10%
# 2. do a few exp

# regression oversize
# save outlier portfolio
# make equally-weighted portfolio
# rolling basis
# summarize the behavior of portfolios
# 
# take out the low market equity stocks
