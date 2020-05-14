# beta
colnames(data_groups)
test = data_groups %>% filter(date=="1980-03-31")
head(test)
dim(test)[1] # 3234
sum(is.na(test$size_g)) # 1312
sum(is.na(test$beta_g)) # 1312
sum(is.na(test$adj_ret)) # 0
3234 - 1312 # 1922

# 10057 size test: size was used to assign size group
t = data_processed %>% filter(permno=="10057" & date == "1980-03-31")
t$size # it has a size variable

# look for size & beta in splite(preBeta, preBeta$date)
# most likely the problem of size group, since size groups are assigned first
# and the number of missing values are the same

# time series sliced on market cap
# newy-west and HAC
# outliers portfolio
# time series object

names(beta_nona)[1]
dim(beta_nona[[1]])[1] # 1922
dim(beta_raw[[1]])[1] # 3286
3286 - 1922 # 1364

p1 = test$permno
p2 = beta_raw[[1]]$permno
t1 = setdiff(p2, p1)
length(t1) # 52
3286 - 3234 # 52
# some stock in beta_raw is not in data_groups




# size



# b2m

# e2p



extractXTS <- function(time, var) {
  res <- get(paste("res_1_", time, sep=""))
  lm <- res[[var]]$lm_win$res[,3]
  rob <- res[[var]]$mm_95$res[,3]
  xtsName <- paste(var, "_", time, sep="")
  var_xts <- as.xts(cbind(lm, rob))
  index(var_xts) <- as.Date(index(var_xts))
  assign(xtsName, var_xts)
  save(list=xtsName, file=paste("data/for Doug 2/", xtsName, ".Rdata", sep=""))
}

extractXTS("63_90", "size")
extractXTS("63_90", "beta")
extractXTS("63_90", "b2m")
extractXTS("63_90", "e2p")

extractXTS("80_18", "size")
extractXTS("80_18", "beta")
extractXTS("80_18", "b2m")
extractXTS("80_18", "e2p")

midcap.zoo = read.zoo("midcap.csv",sep = ",", header = T, format = "%m/%d/%Y")
midcap.ts = as.xts(midcap.zoo)
names(midcap.ts) # Character ticker names
row.names(midcap.ts) # Null as it should be
class(index(midcap.ts)) # Date class
head(index(midcap.ts))

res = res_1_63_90
var = 'size'
lm_win <- res[[var]]$lm_win$res[,3]
mm_95 <- res[[var]]$lmm_95$res[,3]
var_xts <- as.xts(cbind(lm_win, mm_95))