# load packages
require(RPostgres)
require(tidyverse)
require(lubridate)
rm(list = ls())

# total of 6 tables needed
# data_price
# data_delist
# data_names
# data_idx
# data_link
# data_fund

get_data <- function(query) {
    res <- dbSendQuery(wrds, query)
    data <- dbFetch(res, n=-1)
    dbClearResult(res)
    data
}

user = 'dan94'
password = 'TNafphzQBiM923W'

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user = user,
                  password = password)

#### Queries ####
q_price <- "select permno, date, prc, hsiccd, shrout, ret, retx
            from crsp.msf
            where date >= '1957-06-30' and date <= '2019-06-30'"
q_delist <- "select permno, dlret, dlstdt as date
             from crsp.msedelist"
q_names <- "select permno, shrcd, exchcd, siccd, hsiccd, namedt, nameendt
            from crsp.msenames"
q_idx <- "select date, vwretd
          from crsp.msi"
q_link <- "select gvkey, lpermno as permno, linktype, linkprim, linkdt, linkenddt
           from crsp.ccmxpf_linktable"
q_fund <- "select gvkey, datadate as date, seq, txditc, pstkrv, pstkl, pstk, upstk,
            lt, fyr, ib, at, csho, bkvlps
           from comp.funda"

#### Get data ####
data_price <- get_data(q_price)
data_delist <- get_data(q_delist)
data_names <- get_data(q_names)
data_idx <- get_data(q_idx)
data_link <- get_data(q_link)
data_fund <- get_data(q_fund)

adjust_date <- function(df) {
    df <- df %>% mutate(date = ceiling_date(date, "month") - days(1))
    df
}

#### Adjust date to end of month ####
data_price <- adjust_date(data_price)
data_delist <- adjust_date(data_delist)
data_idx <- adjust_date(data_idx)
data_fund <- adjust_date(data_fund)

adjust_return <- function(ret, dlret) {
    adj_ret <- ifelse(!is.na(ret), ifelse(!is.na(dlret), (1 + ret) * (1 + dlret) - 1, ret), dlret)
    adj_ret
}

tmp_return <- data_price %>%
    left_join(data_delist, by = c("permno", "date")) %>%
    mutate(adj_ret = adjust_return(ret, dlret))

me <- data_price %>%
    mutate(me = abs(prc * shrout)) %>%
    select(permno, date, me)

# december market equity
me_dec <- me %>%
    mutate(date1 = floor_date(date %m-% months(6), "year") - days(1)) %>%
    left_join(me, by = c("date1" = "date", "permno" = "permno")) %>%
    select(date, permno, me.y) %>%
    `colnames<-`(c("date", "permno", "me_dec"))

# June market equity
me_june <- me %>%
    mutate(date2 = floor_date(date %m-% months(6), "year") + months(6) - days(1)) %>%
    left_join(me, by = c("date2" = "date", "permno" = "permno")) %>%
    select(date, permno, me.y) %>%
    `colnames<-`(c("date", "permno", "me_june"))

tmp_me <- me %>%
    left_join(me_dec, by = c("date", "permno")) %>%
    left_join(me_june, by = c("date", "permno"))

tmp_crsp <- tmp_return %>%
    left_join(tmp_me, by = c("date", "permno")) %>%
    left_join(data_names, by = "permno") %>%
    filter(date >= namedt & date <= nameendt) %>%
    filter(siccd < 6000 | siccd > 6999) %>%
    filter(exchcd %in% c(1, 2, 3, 31, 32, 33)) %>%
    left_join(data_idx, by = "date")

tmp_link <- tmp_crsp %>%
    left_join(data_link, by = "permno") %>%
    filter(date >= linkdt & date <= linkenddt) %>%
    filter(linktype %in% c("LC", "LU", "LS", "LN")) %>%
    filter(linkprim %in% c("P", "C", "J"))

tmp_fund <- data_fund %>%
    group_by(gvkey, date) %>%
    fill(colnames(data_fund)[3:length(colnames(data_fund))], .direction = "up") %>%
    slice(1) %>%
    ungroup() %>%
    # fiscal year end related with which june
    # fiscal year ends from July, t-1 to June, t is matched with June, t
    mutate(date = ceiling_date(date %m-% months(6), "year") + months(6) - days(1))
    # mutate(year = year(date))

data_filtered <- tmp_link %>%
    # date3: July,t to June,t+1 is matched with June,t
    mutate(date3 = floor_date(date %m-% months(6), "year") + months(6) - days(1)) %>%
    # mutate(year = year(date %m+% months(6)) - 1) %>%
    left_join(tmp_fund, by = c("gvkey", "date3" = "date"))
    # left_join(tmp_fund, by = c("gvkey", "year"))

# some extra filters
data_processed <- data_filtered %>%
    group_by(permno, date) %>%
    mutate(n = n()) %>%
    filter(n == 1) %>%
    filter(hsiccd.x < 6000 | hsiccd.x > 6999) %>%
    filter(hsiccd.y < 6000 | hsiccd.y > 6999) %>%
    filter(siccd != 0) %>%
    # filter(hsiccd.x != 0 | siccd != 0 | hsiccd.y != 0) %>%
    filter(linktype %in% c("LC", "LU")) %>%
    filter(linkprim %in% c("P", "C"))

data_needed <- data_processed %>%
    mutate(book_pref = ifelse(!is.na(pstkrv), pstkrv, ifelse(!is.na(pstkl), pstkl, pstk))) %>%
    mutate(book_eq = bkvlps * csho + pstk * 1000) %>%
    mutate(she = ifelse(!is.na(seq), seq, ifelse(!is.na(book_eq), book_eq, at - lt))) %>% # problem with bkvlps
    mutate(book_pref = replace_na(book_pref, 0) * 1000) %>%
    mutate(txditc = replace_na(txditc, 0)) %>%
    mutate(be = she + txditc - book_pref) %>%
    mutate(me_dec = me_dec/1000) %>%
    mutate(me_june = me_june/1000) %>%
    select(permno, date, adj_ret, me_dec, me_june, be, ib, exchcd)

data_reg <- data_needed %>%
    mutate(size = log(me_june)) %>%
    mutate(be = ifelse(be > 0, be, NA)) %>%
    mutate(b2m = log(be/me_dec)) %>%
    mutate(e2p = ib/me_dec) %>%
    select(permno, date, adj_ret, size, b2m, e2p, exchcd)

save(data_reg, file = "C:/Users/danau/data_reg.Rdata")
load("C:/Users/danau/data_reg.Rdata")

###### beta ######

som <- function(date) {
    floor_date(date, "month")
}

eom <- function(date) {
    ceiling_date(date, "month") - days(1)
}

tmp_beta <- data_reg %>% select(permno, date, adj_ret) %>% na.omit(adj_ret)

tmp_beta <- tmp_beta %>%
    group_by(permno) %>% 
    arrange(permno, date) %>%
    mutate(date = som(date)) %>%
    mutate(past5 = date %m-% months(60)) %>%
    mutate(num = row_number()) %>%
    mutate(index = date) %>%
    complete(index = seq(min(past5), max(date), by = "month")) %>%
    arrange(permno, index) %>%
    fill(num, .direction = "up")

tmp_beta1 <- tmp_beta
data_beta <- left_join(tmp_beta, tmp_beta1, by = c("permno", "index" = "past5")) %>%
    mutate(past5.y = index) %>%
    arrange(permno, date.y) %>%
    mutate(num = num.y - num.x) %>%
    mutate(date = eom(date.y)) %>%
    mutate(adj_ret = adj_ret.y) %>% 
    select(permno, date, adj_ret, num) %>% 
    filter(num >= 24)



save(df1, file = "C:/Users/danau/beta_rtns.Rdata")
load("C:/Users/danau/beta_rtns.Rdata")
