begin_of_month <- function(date) {
    floor_date(date, "month")
}

end_of_month <- function(date) {
    ceiling_date(date, "month") - days(1)
}

last_june <- function(date) {
    floor_date(date %m-% months(6), "year") + months(6)- days(1)
}

last_fy_dec <- function(date) {
    floor_date(date %m-% months(6), "year") - days(1)
}

adjust_date <- function(df) {
    df <- df %>% mutate(date = end_of_month(date))
    df
}

adjust_return <- function(ret, dlret) {
    adj_ret <- ifelse(!is.na(ret), ifelse(!is.na(dlret), (1 + ret) * (1 + dlret) - 1, ret), dlret)
    adj_ret
}

# adjust date to end of month date for consistency
data_price <- adjust_date(data_price)
data_delist <- adjust_date(data_delist)
data_idx <- adjust_date(data_idx)
data_fund <- adjust_date(data_fund)

# adjust for delete return
tmp_return <- data_price %>%
    left_join(data_delist, by = c("permno", "date")) %>%
    mutate(adj_ret = adjust_return(ret, dlret))

# calculate market equity
tmp_me <- data_price %>%
    mutate(me = abs(prc * shrout)) %>%
    select(permno, date, me)

# find ME of december of last fiscal year
# for e2p, b2m ratio calculation
tmp_me_dec <- tmp_me %>%
    data.frame() %>% 
    mutate(date1 = last_fy_dec(date)) %>%
    left_join(tmp_me, by = c("date1" = "date", "permno" = "permno")) %>%
    select(date, permno, me.y) %>%
    `colnames<-`(c("date", "permno", "me_dec"))

# find ME of last june, as size measure
tmp_me_june <- tmp_me %>%
    data.frame() %>% 
    mutate(date2 = last_june(date)) %>%
    left_join(tmp_me, by = c("date2" = "date", "permno" = "permno")) %>%
    select(date, permno, me.y) %>%
    `colnames<-`(c("date", "permno", "me_june"))

tmp_me <- tmp_me %>%
    data.frame() %>% 
    left_join(tmp_me_dec, by = c("date", "permno")) %>%
    left_join(tmp_me_june, by = c("date", "permno"))

# US traded non financial stocks
# todo: verify namedt & nameendt filter
tmp_crsp <- tmp_return %>%
    data.frame() %>% 
    left_join(tmp_me, by = c("date", "permno")) %>%
    left_join(data_names, by = "permno") %>%
    filter(date >= namedt & date <= nameendt) %>%
    filter(siccd < 6000 | siccd > 6999) %>%
    filter(exchcd %in% c(1, 2, 3, 31, 32, 33)) %>%
    left_join(data_idx, by = "date")

#### Important ####
# link filters in the crsp.ccmxpf_linktable
tmp_link <- tmp_crsp %>%
    data.frame() %>% 
    left_join(data_link, by = "permno") %>%
    mutate(keep = ifelse(date >= linkdt, ifelse(date <= linkenddt | is.na(linkenddt), 1, 0), 0)) %>% 
    filter(keep == 1) %>%
    filter(linktype %in% c("LC", "LU", "LS", "LN")) %>%
    filter(linkprim %in% c("P", "C", "J"))

# construct complete data for each month
# (several records of a company in a month may be found in funda)
# (different factors may be missing)
tmp_fund <- data_fund %>%
    data.frame() %>% 
    group_by(gvkey, date) %>%
    fill(colnames(data_fund)[3:length(colnames(data_fund))], .direction = "up") %>%
    slice(1) %>%
    ungroup() %>%
    # fiscal year end related with which june
    # fiscal year ends from July, t-1 to June, t is matched with June, t
    # this means as long as the fundamental is reported prior to the June, t (July, t in paper)
    # it is counted as last fy year's data and used
    mutate(date = ceiling_date(date %m-% months(6), "year") + months(6) - days(1))
    # mutate(year = year(date))

tmp_filtered <- tmp_link %>%
    data.frame() %>% 
    # date3: return from July,t to June,t+1 is matched with funda of June,t ( July, t-1 to June, t)
    mutate(date3 = last_june(date)) %>%
    left_join(tmp_fund, by = c("gvkey", "date3" = "date"))
    # left_join(tmp_fund, by = c("gvkey", "year"))

# some extra filters
tmp_processed <- tmp_filtered %>%
    data.frame() %>% 
    group_by(permno, date) %>%
    mutate(n = n()) %>%
    filter(n == 1) %>%
    filter(hsiccd.x < 6000 | hsiccd.x > 6999) %>%
    filter(hsiccd.y < 6000 | hsiccd.y > 6999) %>%
    filter(siccd != 0) %>%
    # filter(hsiccd.x != 0 | siccd != 0 | hsiccd.y != 0) %>%
    filter(linktype %in% c("LC", "LU")) %>%
    filter(linkprim %in% c("P", "C"))

# calculate the factors
# log of me and log of book to market
data_processed <- tmp_processed %>%
    data.frame() %>% 
    mutate(book_pref = ifelse(!is.na(pstkrv), pstkrv, ifelse(!is.na(pstkl), pstkl, pstk))) %>%
    mutate(book_eq = bkvlps * csho + pstk * 1000) %>%
    mutate(she = ifelse(!is.na(seq), seq, ifelse(!is.na(book_eq), book_eq, at - lt))) %>% # problem with bkvlps
    mutate(book_pref = replace_na(book_pref, 0) * 1000) %>%
    mutate(txditc = replace_na(txditc, 0)) %>%
    mutate(be = she + txditc - book_pref) %>%
    mutate(me_dec = me_dec/1000) %>%
    mutate(me_june = me_june/1000) %>%
    mutate(size = log(me_june)) %>%
    mutate(be = ifelse(be > 0, be, NA)) %>%
    mutate(b2m = log(be/me_dec)) %>%
    mutate(e2p = ib/me_dec)
