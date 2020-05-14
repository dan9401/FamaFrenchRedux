require(xts)       # time series plots (and else)
require(hexbin)    # hexbin plots
require(Hmisc)     # hexbin plots
require(grid)      # hex bin plots

rm(list=ls())

source("regFunc.R")


# scatter plot for showing the winsorization effect

load(file="data/data_80_18.Rdata")


#e2p80_18 <- repFunc2(data_80_18, variables = "e2p")
#e2p80_18_t <- repFunc2(data_80_18, variables = "e2p", rob_trim = 0.005)
#save(e2p80_18, file="e2p80_18.Rdata")
#save(e2p80_18_t, file="e2p80_18_t.Rdata")

load(file="e2p80_18.Rdata")
load(file="e2p80_18_t.Rdata")

s1 <- e2p80_18$mm_95$est$res[,3]
s2 <- e2p80_18_t$mm_95$est$res[,3]

s_diff <- abs(s1 - s2)
test_months <- names(s_diff[s_diff > 5])
test_data <- data_80_18[test_months]
estimates_16 <- list(robust =e2p80_18_t$mm_95$est$res[test_months, ], 
                     linear = e2p80_18_t$lm_win$est$res[test_months, ])

save(test_data, file="test_data.Rdata")
save(estimates_16, file="estimates_16.Rdata")
load(file="test_data.Rdata")
load(file="estimates_16.Rdata")

require(tidyverse) # regression that happens
require(RobStatTM)

require(xts)       # time series plots (and else)
require(hexbin)    # hexbin plots
require(Hmisc)     # hexbin plots
require(grid)      # hex bin plots

source("pltFunc.R")
pltFunc_hex(test_data, "e2p", estimates_16, file = "test.pdf", nbins = 30,
            cropping = list(type = "hv", xcrop = 0.005, ycrop = 0.005), median = FALSE)


data_scatter = data_80_18$`1980-10-31`
plot(data_scatter$b2m, data_scatter$adj_ret,
     main="1980-10-31 BM Scatter", xlab="BM", ylab="ret")

data_s_t = winsorize(data_scatter, "b2m", 0.005)
plot(data_s_t$b2m, data_s_t$adj_ret,
     main="1980-10-31 Winsorized BM Scatter", xlab="BM", ylab="ret")

pdf(file = "test2.pdf")
for (i in 1:length(test_data)) {
  pd = test_data[[i]]
  plot(pd$b2m, pd$adj_ret,
       main=paste(names(test_data)[[i]], "BM Scatter"), xlab="BM", ylab="ret",
       pch=16, pex=0.2)
}
dev.off()


pdf(file = "test3.pdf")
for (i in 1:length(test_data)) {
  pd = winsorize(test_data[[i]], "b2m", 0.005)
  plot(pd$b2m, pd$adj_ret,
       main=paste(names(test_data)[[i]], "BM Scatter"), xlab="BM", ylab="ret",
       pch=16, pex=0.2)
}
dev.off()

td = test_data[[1]]
names(test_data)[[1]]
load("data/data_processed.Rdata")
inv = data_80_18 %>% 
  filter(date=="1980-03-31") %>% 
  select(permno, date, size, beta) %>% 
  na.omit() %>% 
  mutate(ep=ib/me_dec)
earning = inv$ib
price = inv$me_dec
plot(log(price), log(earning), pch = 16)
abline(h=0, col="blue")
abline(v=0, col="blue")
abline(a=0, b=1, col="red")
qqnorm(earning/price)

qqnorm(ep_win$`80mar`$ep)

boxplot(earning/price)
boxplot(ep_win$`80mar`$ep)

require(ggplot2)

p <- ggplot(inv, aes(sample = ep, col=me_dec))
p + stat_qq() + stat_qq_line()

make_qq <- function(dd, x) {
  dd<-dd[order(dd[[x]]), ]
  dd$qq <- qnorm(ppoints(nrow(dd)))
  dd
}

ggplot(make_qq(inv, "ep")) + 
  geom_point(aes(x=qq, y=ep, color=log(me_dec))) + 
  labs(x="Theoretical",y="Observed")

winsorize(x, var, 0.005)

ggplot(make_qq(winsorize(inv, "ep", 0.005), "ep")) + 
  geom_point(aes(x=qq, y=ep, size=log(me_dec), color=log(me_dec)), alpha=0.4) + 
  labs(x="Theoretical",y="Observed") + 
  scale_color_gradient(high="blue", low="green")



load(file = "data/res/res_1_63_90.Rdata")
load(file = "data/res/res_1_80_18.Rdata")
load(file = "data/res/res_2_63_90.Rdata")
load(file = "data/res/res_2_80_18.Rdata")
load(file = "data/res/res_4_63_90.Rdata")
load(file = "data/res/res_4_80_18.Rdata")

load(file = "data/res/e2p_1_63_90.Rdata")
load(file = "data/res/e2p_1_80_18.Rdata")
load(file = "data/res/e2p_2_63_90.Rdata")
load(file = "data/res/e2p_2_80_18.Rdata")
load(file = "data/res/e2p_4_63_90.Rdata")
load(file = "data/res/e2p_4_80_18.Rdata")

load(file = "data/res/e2p_57_63_90.Rdata")
load(file = "data/res/e2p_57_80_18.Rdata")
load(file = "data/res/e2p_57t_63_90.Rdata")
load(file = "data/res/e2p_57t_80_18.Rdata")

extractEst2(res_1_63_90)
extractEst2(res_1_80_18)
extractEst2(res_2_63_90)
extractEst2(res_2_80_18)
extractEst2(res_4_63_90)
extractEst2(res_4_80_18)

extractEst2(e2p_1_63_90)
extractEst2(e2p_1_80_18)
extractEst2(e2p_2_63_90)
extractEst2(e2p_2_80_18)
extractEst2(e2p_4_63_90)
extractEst2(e2p_4_80_18)

extractEst(e2p_57_63_90)
extractEst(e2p_57_80_18)
extractEst(e2p_57t_63_90)
extractEst(e2p_57t_80_18)


# Hexbinplots for all periods
# 63-90 / 80-18

# Hexbinplots for 16 months
# where abs(ep slope - ep slope with win) > 5

# Slope Comparison Plots

# Scatter Plot for 


require(hexbin)
require(Hmisc)
rm(list = ls())


require(hexbin)    # hexbin plots
require(Hmisc)     # hexbin plots
require(grid)      # hex bin plots
require(xts)       # time series plots (and else)


# summary(e2p80_15$res$mm_95$`1980-01-31`)$coefficients
# summary(e2p80_15_t$res$mm_95$`1980-01-31`)$coefficients
# summary(lmrobdetMM(adj_ret * 100 ~ e2p, data_80_151[[1]], control = lmrobdet.control(efficiency = 0.95)))$coefficients
# summary(lmrobdetMM(adj_ret * 100 ~ e2p, winsorize(data_80_151[[1]], "e2p", 0.005), 
#                    control = lmrobdet.control(efficiency = 0.95)))$coefficients


slopes <- as.xts(cbind(s1, s2))
colnames(slopes) <- c("MM95", "MM95 with 0.5% Win")

slopeCompare(slopes)


pltSlopes(res_1_63_90, file = "plots/63_90_slope_compare.pdf", time="1963-1990")
pltSlopes(res_1_80_18, file = "plots/80_18_slope_compare.pdf", time="1980-2018")


pltFunc_hex(test_data, "e2p", estimates_16, file = "robust_test/16month_hvcrop.pdf", nbins = 30,
            cropping = list(type = "hv", xcrop = 0.005, ycrop = 0.005), median = FALSE)

pltFunc_hex(test_data, "e2p", estimates_16, file = "robust_test/16month_hvcrop.pdf", nbins = 30,
            cropping = list(type = "hv", xcrop = 0.005, ycrop = 0.005), median = FALSE)



pltFunc3(data_63_90, res_1_63_90, time="63_90")
pltFunc3(data_80_18, res_1_80_18, time="80_18")

pltSlopes(res)



#save(test_data, file = "test_data.Rdata")
#load("robust_test/test_data.Rdata")
#load("data/data_80_15.Rdata")

# t(sapply(data_80_15,dim))[,1] - t(sapply(data_80_151,dim))[,1]

estimates <- list(robust = e2p63_90_t$mm_95$est$res, 
                  linear = e2p63_90_t$lm_win$est$res)
estimates_16 <- list(robust =e2p63_90_t$mm_95$est$res[test_months, ], 
                    linear = e2p63_90_t$lm_win$est$res[test_months, ])
estimates2 <- list(robust = e2p63_90_t$mm_95$est$res, 
                   linear = e2p63_90$mm_95$est$res)
estimates2_16 <- list(robust = e2p63_90_t$mm_95$est$res[test_months,], 
                     linear = e2p63_90$mm_95$est$res[test_months,])

# estimates_t <- list(robust = test2$mm_95$est$res[1:10, ],
#                   linear = test2$lm_win$est$res[1:10, ])
# pltFunc_hex_t(data_80_15[1:10], "e2p", estimates_t, file = "test.pdf", nbins = 30,
#               cropping = list(type = "hv", xcrop = 0.005, ycrop = 0.005), median = TRUE)


pltFunc_hex(test_data, "e2p", estimates_16, file = "test/16month_nocrop.pdf", nbins = 30)
pltFunc_hex(test_data, "e2p", estimates_16, file = "test/16month_hcrop.pdf", nbins = 30,
              cropping = list(type = "h", xcrop = 0.005), median = FALSE)
pltFunc_hex(test_data, "e2p", estimates_16, file = "robust_test/16month_hvcrop.pdf", nbins = 30,
            cropping = list(type = "hv", xcrop = 0.005, ycrop = 0.005), median = FALSE)
pltFunc_hex(data_63_90, "e2p", estimates, file = "test/8015_nocrop.pdf", nbins = 30)
pltFunc_hex(data_63_90, "e2p", estimates, file = "test/8015_hcrop.pdf", nbins = 30,
              cropping = list(type = "h", xcrop = 0.005), median = FALSE)
pltFunc_hex(data_63_90, "e2p", estimates, file = "test/8015_hvcrop.pdf", nbins = 30,
              cropping = list(type = "hv", xcrop = 0.005, ycrop = 0.005), median = FALSE)

pltFunc_hex(test_data, "e2p", estimates2_16, file = "test/16month_rob_nocrop.pdf", nbins = 30, rob_compare = TRUE)
pltFunc_hex(test_data, "e2p", estimates2_16, file = "test/16month_rob_hcrop.pdf", nbins = 30,
              cropping = list(type = "h", xcrop = 0.005), median = FALSE, rob_compare = TRUE)
pltFunc_hex(test_data, "e2p", estimates2_16, file = "test/16month_rob_hvcrop.pdf", nbins = 30,
              cropping = list(type = "hv", xcrop = 0.005, ycrop = 0.005), median = FALSE, rob_compare = TRUE)
pltFunc_hex(data_63_90, "e2p", estimates2, file = "test/8015_rob_nocrop.pdf", nbins = 30, rob_compare = TRUE)
pltFunc_hex(data_63_90, "e2p", estimates2, file = "test/8015_rob_hcrop.pdf", nbins = 30,
              cropping = list(type = "h", xcrop = 0.005), median = FALSE, rob_compare = TRUE)
pltFunc_hex(data_63_90, "e2p", estimates2, file = "test/8015_rob_hvcrop.pdf", nbins = 30,
            cropping = list(type = "hv", xcrop = 0.005, ycrop = 0.005), median = FALSE, rob_compare = TRUE)



load(file = "test/e2p_trim.Rdata")
load(file = "test/e2p_win.Rdata")

load(file = "data/res_1_63_90.Rdata")
load(file = "data/res_1_80_18.Rdata")

s1 = res_1_63_90$size$lm_win$res[,3]
s2 = res_1_63_90$size$mm_95$res[,3]
slopes <- as.xts(cbind(s1, s2))
colnames(slopes) <- c("OLS", "Robust 95%")
slopeCompare(slopes)


# loadData()
# loadRes()

load("data/e2p80_15.Rdata")
pltFunc_hex("80_15", "e2p", "win", "0.95", 30, TRUE, "centroids", "Vtrim_0.005", vtrim = TRUE, plt_trim_ratio = 0.005)
pltFunc_hex("80_15", "e2p", "win", "0.95", 30, TRUE, "centroids", "Ntrim_0.005", vtrim = FALSE, plt_trim_ratio = 0.005)
pltFunc_hex("80_15", "e2p", "win", "0.95", 30, TRUE, "centroids", "Vtrim_0.01", vtrim = TRUE, plt_trim_ratio = 0.01)
pltFunc_hex("80_15", "e2p", "win", "0.95", 30, TRUE, "centroids", "Ntrim_0.01", vtrim = FALSE, plt_trim_ratio = 0.01)
pltFunc_hex("80_15", "e2p", "win", "0.95", 30, TRUE, "centroids", "Vtrim_0.025", vtrim = TRUE, plt_trim_ratio = 0.025)
pltFunc_hex("80_15", "e2p", "win", "0.95", 30, TRUE, "centroids", "Ntrim_0.025", vtrim = FALSE, plt_trim_ratio = 0.025)
pltFunc_hex_lim("80_15", "e2p", "win", "0.95", 30, TRUE, "centroids", "Vtrim_xlim_0.5", vtrim = TRUE, xlim = c(-0.5, 0.5))
pltFunc_hex_lim("80_15", "e2p", "win", "0.95", 30, TRUE, "centroids", "Ntrim_xlim_0.5", vtrim = FALSE, xlim = c(-0.5, 0.5))
# pltFunc_hex <- function(time = c("63_90", "80_15", "63_15", "63_18"), variable, 
#                         outliers = c("win", "trim"),
#                         efficiency = c("0.95", "0.99", "0.999"),
#                         hex_legend = FALSE, style = "centroids", file_name = NULL,
#                         vtrim = FALSE, plt_trim_ratio = 0.005, ...) 



# -------------------------------------------------------------------------
# test
pdf(file = paste("plots/", "e2p_slope_compare.pdf", sep = ""))
slopeSeriesPlot("80_15", "e2p", efficiency = "0.95")
dev.off()




### ggplot hex for greg
source("ggplot_hex.R")
x <- c(rnorm(5000),rnorm(5000,4,1.5))
y <- c(rnorm(5000),rnorm(5000,2,3))
bin = hex_bin(x=x, y=y, frequency.to.area = T)
hexes = hex_coord_df(x=bin$x, y=bin$y, width=attr(bin, "width"), height=attr(bin, "height"), size=pmax(bin$size, 0.25))
hexes$col = rep(bin$col, each=6)
colll = hexes$col
ggplot(hexes, aes(x=x, y=y)) + geom_polygon(aes(group=id, fill = colll)) + scale_fill_gradient(low="#00B6FF", high="#0091CB")

