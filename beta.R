data_beta_reg <- data_processed %>%
    data.frame() %>% 
    select(permno, date, adj_ret, size, b2m, e2p, exchcd, vwretd, at, be, ib)

tmp_prebeta <- data_beta_reg %>%
    data.frame() %>% 
    select(permno, date, adj_ret) %>%
    na.omit(adj_ret) %>%
    group_by(permno) %>%
    arrange(permno, date) %>%
    mutate(date = begin_of_month(date)) %>%
    mutate(past5 = date %m-% months(60)) %>%
    mutate(num = row_number()) %>%
    mutate(index = date) %>%
    complete(index = seq(min(past5), max(date), by = "month")) %>%
    arrange(permno, index) %>%
    fill(num, .direction = "up")

tmp_prebeta1 <- tmp_prebeta
tmp_prebeta2 <- left_join(tmp_prebeta, tmp_prebeta1, by = c("permno", "index" = "past5")) %>%
    data.frame() %>% 
    mutate(past5.y = index) %>%
    arrange(permno, date.y) %>%
    mutate(num = num.y - num.x) %>%
    mutate(date = end_of_month(date.y)) %>%
    mutate(adj_ret = adj_ret.y) %>%
    select(permno, date, adj_ret, num)

# data used for calculating beta
data_beta <- tmp_prebeta2 %>%
    data.frame() %>% 
    left_join(select(data_beta_reg, permno, date, size, exchcd, vwretd, at, be, ib), by = c("permno", "date")) %>%
    drop_na(date) %>%
    data.frame()

pre_beta <- function(df){
    df <- df %>%
        data.frame() %>% 
        mutate(beta = NA) %>%
        mutate(beta1 = NA) %>%
        mutate(beta0 = NA) %>%
        mutate(vwretd_l1 = lag(vwretd)) %>%
        mutate(reg = ifelse((month(date) == 6) & (year(date) >= 1962) & (num >=24) &
                                !is.na(at) & !is.na(be) & !is.na(ib) & (at != 0) & (be != 0), 1, NA))
    for (i in 1:nrow(df)) {
        if (!is.na(df$reg[i])) {
            df_tmp <- df[(i - df$num[i]):i, ]
            coeff <- lm(adj_ret ~ vwretd + vwretd_l1, data = df_tmp)$coef
            # print(coeff)
            df$beta0[i] <- coeff[2]
            df$beta1[i] <- coeff[3]
            df$beta[i] <- sum(coeff[2:3])
        }
    }
    df <- filter(df, !is.na(beta))
    df
}

# calculates the pre ranking beta for each stock at each June
preBeta<- do.call(rbind, lapply(split(data_beta, data_beta$permno), pre_beta))
nyse_preBeta <- preBeta %>% filter(exchcd %in% c(1, 31))

breakpoints <- function(df) {
    bps <- list()
    bps$sizes <- quantile(df$size, seq(0.1, 0.9, 0.1), na.rm = TRUE)
    df <- df %>%
        mutate(sg = ntile(size, 10))
    dfls <- split(data.frame(df), df$sg)
    for (i in 1:length(dfls)) {
        bps[[ paste("beta_", as.character(dfls[[i]]$sg[1]), sep = "") ]] <- quantile(dfls[[i]]$beta, seq(0.1, 0.9, 0.1), na.rm = TRUE)
    }
    bps
}
nyse_breakpoints <- lapply(split(nyse_preBeta, nyse_preBeta$date), breakpoints)

preBetaGroup <- function(df, bps) {
    bps <- bps[[ as.character(df$date[1]) ]]
    gps <- df %>%
        mutate(size_g = findInterval(size, c(-Inf, bps$size, Inf)))
    gpl <- lapply(split(gps, gps$size_g), function(df) {
        df <- df %>% mutate(beta_g = findInterval(beta, c(-Inf, bps[[ paste("beta_", size_g[1], sep="") ]], Inf)))
    })
    do.call(rbind, gpl)
}
# each stock at each june with corresponding groups
beta_groups <- do.call(rbind, lapply(split(preBeta, preBeta$date), preBetaGroup, nyse_breakpoints))
data_groups <- data_beta %>%
    data.frame() %>% 
    # should be 6 months, there's an error in front
    mutate(testDate = last_june(date)) %>%
    left_join(select(beta_groups, permno, date, size_g, beta_g), by = c("permno", "testDate" = "date")) %>% 
    select(date, permno, adj_ret, size_g, beta_g)

data_idx <- data_idx %>% mutate(vwretd_l1 = lag(vwretd))
beta_processed <- data_beta %>%
    data.frame() %>% 
    # should be 6 months, there's an error in front
    mutate(testDate = last_june(date)) %>%
    left_join(select(beta_groups, permno, date, size_g, beta_g), by = c("permno", "testDate" = "date")) %>%
    group_by(date, size_g, beta_g) %>%
    summarize(avg_rtn = mean(adj_ret, na.rm = TRUE)) %>%
    na.omit() %>%
    left_join(data_idx, by = "date") %>%
    mutate(g = paste(size_g, beta_g, sep = "_")) %>% 
    data.frame()


