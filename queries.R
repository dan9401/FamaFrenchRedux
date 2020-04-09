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

data_price <- get_data(q_price)
data_delist <- get_data(q_delist)
data_idx <- get_data(q_idx)
data_names <- get_data(q_names)
data_link <- get_data(q_link)
data_fund <- get_data(q_fund)
