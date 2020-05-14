t1pa <- data_tmp2 %>%
    filter((date >= "1963-07-01") & (date < "1991-01-01")) %>%
    group_by(size_g, beta_g) %>%
    summarize(avg_rtn = 100 * mean(avg_rtn)) %>%
    spread(beta_g, avg_rtn)

t1pb <- postBeta

t1pc <- data_tmp %>%
    filter((date >= "1963-07-01") & (date < "1991-01-01")) %>%
    drop_na(size, size_g, beta_g) %>%
    group_by(size_g, beta_g) %>%
    summarize(avg_size = mean(size)) %>%
    spread(beta_g, avg_size)

test <- data_tmp %>%
    select(date, permno, adj_ret, size_g, beta_g)

test3 <- as.data.frame(t(sapply(split(data_post, data_post$g), post_beta)))
test3$g <- rownames(test3)
test3 <- test3 %>%
    separate(g, c("size_g", "beta_g"), "_") %>%
    mutate(beta = vwretd + vwretd_l1) %>%
    select(size_g, beta_g, beta) %>%
    mutate(size_g = as.numeric(size_g)) %>%
    mutate(beta_g = as.numeric(beta_g))

test2 <- left_join(test, test3, by= c("size_g", "beta_g")) %>%
    filter(date >= "1963-07-01" & date <= "1990-12-31")
beta_months <- split(test2, test2$date)

get_beta_slope <- function(df) {
    df <- drop_na(df, beta, adj_ret)
    coeff <- coef(lm(adj_ret *100 ~ beta, data=df))
    coeff
}

ttdf <- t(sapply(beta_months, get_beta_slope))
betas <- ttdf[,2]
mean(betas)
sd(betas)/sqrt(sum(length(betas)))

for (i in 1:738) {
    get_beta_slope(beta_months[[i]])
}

# testLs = lapply(nyse_prebeta, testFunc)
#
# df = test
# bps = nyse_breakpoints
# bps <- bps[[ as.character(df$date[1]) ]]
# gps <- df %>%
#     mutate(size_g = findInterval(size, c(-Inf, bps$size, Inf)))
# gpl <- split(gps, gps$size_g)
# lapply(gpl, function(df) {
#     df <- df %>% mutate(beta_g = findInterval(beta, c(-Inf, bps[[ paste("beta_", size_g[1], sep="") ]], Inf)))
# })
# do.call(rbind, gpl)
#
# test2 = gpl[[1]]
# test2 = test2 %>% select(permno, date, size_g, beta)
# View(test2 %>% mutate(beta_g = findInterval(beta, c(-Inf, bps[[ paste("beta_", size_g[[1]], sep="") ]], Inf))) %>% arrange(beta))
#
#
# preBetaBps <- lapply(split(), breakpoints)
#
# nyse_preBeta <- do.call(rbind, nysePreBetaList)
# an_preBeta <- do.call(rbind, anPreBetaList)
#
# preBetaGroups <- preBeta %>%
#     select(permno, date, size, beta) %>%
#     split(preBeta$date)
#
#
# a = preBetaGroup(test, nyse_breakpoints)
# a %>% mutate(beta_g = paste("beta_", size_g, sep = ""))
#
# test1 <- test %>% mutate(sg = ntile(size, 10))
# test2 <- test1 %>% mutate(sg2 = cut)
# test2 <- test1 %>% mutate(sg2 = findInterval(size, c(-Inf, bps$size, Inf)))
# bps = nyse_breakpoints[[1]]
#
# factor(findInterval(test1$size, c(-Inf, quantile(test1$size, seq(0.1, 0.9, 0.1), na.rm = TRUE), Inf)), labels=1:10)
#
# #
# # test <- data_beta %>% filter(permno == 10147)
# #
# # test <- test %>%
# #     mutate(beta = NA) %>%
# #     mutate(beta1 = NA) %>%
# #     mutate(beta0 = NA) %>%
# #     mutate(vwretd_l1 = lag(vwretd)) %>%
# #     mutate(reg = ifelse((exchcd %in% c(1, 31)) & (month(date) == 7) & (year(date) >= 1962) & (num >=24) & !is.na(at) & !is.na(be) & !is.na(ib) & (at != 0) & (be != 0), 1, NA))
# # for (i in 1:nrow(test)) {
# #     if (!is.na(test$reg[i])) {
# #         df_tmp <- test[(i - test$num[i]):i, ]
# #         coeff <- lm(adj_ret ~ vwretd + vwretd_l1, data = df_tmp)$coef
# #         print(coeff)
# #         test$beta0[i] <- coeff[2]
# #         test$beta1[i] <- coeff[3]
# #         test$beta[i] <- sum(coeff[2:3])
# #     }
# # }
# #
# # View(test %>% filter(!is.na(beta)))
#
# # save(data_beta, file = "C:/Users/danau/data_beta.Rdata")
# # # load("C:/Users/danau/data_beta.Rdata")
#
# nyseNames <- unique(filter(data_beta, exchcd %in% c(1, 31))$permno)
# nyse <- data_beta %>%
#     filter(permno %in% nyseNames) %>%
#     data.frame()
#
# amexNasNames <- unique(filter(data_beta, !(exchcd %in% c(1, 31)))$permno)
# amexNas <- data_beta %>%
#     filter(permno %in% amexNasNames) %>%
#     data.frame()
#
#
#




###### beta ######

som <- function(date) {
    floor_date(date, "month")
}

eom <- function(date) {
    ceiling_date(date, "month") - days(1)
}

tmp_beta <- data_beta_reg %>%
    select(permno, date, adj_ret) %>%
    na.omit(adj_ret) %>%
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
tmp_beta2 <- left_join(tmp_beta, tmp_beta1, by = c("permno", "index" = "past5")) %>%
    mutate(past5.y = index) %>%
    arrange(permno, date.y) %>%
    mutate(num = num.y - num.x) %>%
    mutate(date = eom(date.y)) %>%
    mutate(adj_ret = adj_ret.y) %>%
    select(permno, date, adj_ret, num) # %>%
# filter(num >= 24)

data_beta <- tmp_beta2 %>%
    left_join(select(data_beta_reg, permno, date, size, exchcd, vwretd), by = c("permno", "date"))# %>%
# filter(!is.na(size))

save(data_beta, file = "C:/Users/danau/data_beta.Rdata")
# load("C:/Users/danau/data_beta.Rdata")

NYSE <- data_beta %>%
    filter(exchcd %in% c(1, 31))

tmp_sp <- NYSE %>%
    mutate(fye = floor_date(date %m-% months(6), "year") + months(6) - days(1))

tmp_sizeg <- tmp_sp %>%
    group_by(fye, permno) %>%
    summarise(size = mean(size)) %>%
    group_by(fye) %>%
    mutate(decile = ntile(size, 10))


gg = grey(seq(0.7,0.1,length=lvl))[as.numeric(ff)]
hexbinplot(adj_ret ~ beta, data=test, aspect="1", xbins=25, 
           style = "lattice", pen = gg, border = rgb(1,1,1),
           panel=function(x, y, ...)
           {
               panel.hexbinplot(x, y, ...)
               panel.abline(a = 0, b = 1, col=1)
               panel.abline(a = 2, b = -1, col=2)
           },
           key=list(space="right",
                    lines=list(col=1:2, lty=1:2, lwd=1),
                    text=list(c("Purple Line"," Dark-green Line"))
           ),
           main = "test", colorkey = F, # colorcut = seq(0, 1, length=10),
           minarea = 0.15, maxarea = 0.9# , colramp = magent
)
legend("topleft", lty = 1, col = 1, legend = "test")

hb2 =  hexbin(test$beta, test$adj_ret, xbins = 25, shape = 1)

hb =  hexbin(test$beta, test$adj_ret, xbins = 25, shape = 1, xbnds = range(test$beta, finite=T), ybnds = range(test$adj_ret, finite=T))
ff = as.factor(hb@count)
lvl = length(levels(ff))
gg = rgb(red = seq(0.6,0.4,length=lvl), green = seq(0.6,0.4,length=lvl), blue = .8)[as.numeric(ff)]
gg1 = grey(seq(1,0,length=lvl))[as.numeric(ff)]

par(xpd = F)
hb2 = hexbin(test$beta, test$adj_ret, xbins = 25)
sc = max(hb2@count) - 1
bnds = round(1 + sc * seq(0, 1, length = 17))
n = len(d)


ff = as.factor(hb2@count)
lvl = length(levels(ff))
gplot.hexbin(hb2, style = "lattice", pen = 1, 
             border = grey(1), legend=F, minarea= 0.1, maxarea = 0.9)
abline(a = 0, b = 1, col = 1)
P <- plot(hb2, type="n", main = "Bivariate mixture (10000)")# asp=1

## 2) add hexagons (in the proper viewport):
pushHexport(P$plot.vp)
grid.hexagons(hb2, style= "lattice", pen = grey(seq(1,1,length=39))[as.numeric(ff)])
grid.abline(0, 1, gp = gpar(col=2))
popViewport()

hb2

grey(seq(0, 1, length(bins@count+1)))



hb2 = hexbin(x, y)
grid.hexagons(hb2)

x = test$beta
y = test$adj_ret
xbins = 30
xbnds = range(x, finite = TRUE)
ybnds = range(y, finite = TRUE)

shape <- 1 * (diff(ybnds) / diff(a$y.limits)) / (diff(xbnds) / diff(a$x.limits))

hexbin(x = x, y = y,
       xbins = xbins, shape = shape,
       xbnds = xbnds, ybnds = ybnds)



# postBeta <- as.data.frame(t(sapply(split(tmp_postBeta, tmp_postBeta$g), post_beta)))
# postBeta$g <- rownames(postBeta)
# postBeta <- postBeta %>%
#     separate(g, c("size_g", "beta_g"), "_") %>%
#     mutate(beta = vwretd + vwretd_l1) %>%
#     select(size_g, beta_g, beta) %>%
#     spread(beta_g, beta) %>%
#     select(c(1, 2, 4:11, 3)) %>%
#     arrange(as.numeric(size_g))



coll = rgb(red = seq(0.6,0,length=249), green = seq(0.8,0.6,length=249), blue = 0.9)[as.numeric(as.factor(hexes$id))]



pltFunc_s <- function(time = c("63_90", "80_15", "63_15", "63_18"), variable, outliers = c("win", "trim"), efficiency = c("0.95", "0.99", "0.999"), ...) {
    time <- match.arg(time)
    outliers <- match.arg(outliers)
    data <- get(paste("data_", time, sep = ""))
    regRes <- get(paste(variable, time, "_s", sep=""))$res
    linearRes <- regRes[[paste("lm_", outliers, sep = "")]]
    robustRes <- regRes[[paste("mm_", substr(efficiency, 3, 5), sep = "")]]
    pdf(file = paste("plots/", variable, time, outliers, efficiency, "_s.pdf", sep = ""))
    for (i in 1:length(data)) {
        month <- data[[i]]
        plot(month[[variable]], month$adj_ret * 100, xlab = variable, ylab = "Adjusted Return(%)", 
             main = paste(names(data)[i], ":", dim(month)[1],"stocks"), pch = 20, ...) # , xlim = c(-10, 10)
        linear <- linearRes[[i]]$coefficients
        abline(linear[1], linear[2], col = 2)
        robust <- robustRes[[i]]$coefficients
        abline(robust[1], robust[2])
        legend("topleft", legend = c(paste("robustMM:", efficiency, " Slope:(", round(robust[2], 2), ")", sep=""),
                                     paste("linear:",  outliers, " Slope(", round(linear[2], 2), ")", sep="")), 
               col = 1:2, lty = 1, cex = 0.75)
    }
    dev.off()
}

pltFunc_hex <- function(time = c("63_90", "80_15", "63_15", "63_18"), variable, 
                        outliers = c("win", "trim"),
                        efficiency = c("0.95", "0.99", "0.999"), nbins = 25,
                        hex_legend = FALSE, style = "centroids", file_name = NULL,
                        vtrim = FALSE, plt_trim_ratio = 0.005, xlim = NULL, ...) {
    time <- match.arg(time)
    outliers <- match.arg(outliers)
    data <- get(paste("data_", time, sep = ""))
    regRes <- get(paste(variable, time, sep=""))$res
    linearRes <- regRes[[paste("lm_", outliers, sep = "")]]
    robustRes <- regRes[[paste("mm_", substr(efficiency, 3, 5), sep = "")]]
    file_name = ifelse(!is.null(file_name), paste("plots/hex_", file_name, ".pdf", sep = ""),
                       paste("plots/hex_", variable, time, outliers, efficiency, ".pdf", sep = ""))
    pdf(file = file_name)
    for (i in 1:length(data)) {
        
        if (vtrim == TRUE) {
            month <- trim(data[[i]], c(variable, "adj_ret"), plt_trim_ratio) # may change here  
        } else {
            month <- trim(data[[i]], variable, plt_trim_ratio) # may change here  
        }
        
        hb =  hexbin(month[[variable]], month$adj_ret, xbins = nbins, shape = 1, 
                     xbnds = range(month[[variable]], finite=T), ybnds = range(month$adj_ret, finite=T))
        countF = as.factor(hb@count)
        lvl = length(levels(countF))
        palette = grey(seq(0.7,0.1,length=lvl))[as.numeric(countF)]
        linear <- linearRes[[i]]$coefficients
        robust <- robustRes[[i]]$coefficients
        hex = hexbinplot(as.formula(paste("adj_ret * 100 ~", variable)), data=month, aspect="1", xbins=nbins, 
                         style = style, pen = palette, border = rgb(1,1,1),
                         panel=function(x, y, ...)
                         {
                             panel.hexbinplot(x, y, ...)
                             panel.abline(a = linear[1], linear[2], col=2)
                             panel.abline(a = robust[1], b = robust[2], col=1)
                         },
                         key=list(background = "gray97", corner = c(0.01, 0.99), #space="right", 
                                  lines=list(col=1:2, lty=1, lwd=1) ,
                                  text=list(c(paste("RobustMM Slope:", round(robust[2], 2)),
                                              paste("OLS Slope:", round(linear[2], 2))))
                         ),
                         main = paste(names(data)[i], ":", dim(data[[i]])[1],"stocks \n RobustMM:", efficiency, " vs  OLS:", outliers),
                         colorkey = hex_legend, xlab = variable, ylab = "Return(%)",
                         minarea = 0.15, maxarea = 0.9)
        print(hex)
    }
    dev.off()
}

pltFunc_hex_lim <- function(time = c("63_90", "80_15", "63_15", "63_18"), variable, 
                            outliers = c("win", "trim"),
                            efficiency = c("0.95", "0.99", "0.999"), nbins = 25,
                            hex_legend = FALSE, style = "centroids", file_name = NULL,
                            vtrim = FALSE, plt_trim_ratio = 0.005, xlim = c(-0.5, 0.5), ...) {
    time <- match.arg(time)
    outliers <- match.arg(outliers)
    data <- get(paste("data_", time, sep = ""))
    regRes <- get(paste(variable, time, sep=""))$res
    linearRes <- regRes[[paste("lm_", outliers, sep = "")]]
    robustRes <- regRes[[paste("mm_", substr(efficiency, 3, 5), sep = "")]]
    file_name = ifelse(!is.null(file_name), paste("plots/hex_", file_name, ".pdf", sep = ""),
                       paste("plots/hex_", variable, time, outliers, efficiency, ".pdf", sep = ""))
    pdf(file = file_name)
    for (i in 1:length(data)) {
        
        if (vtrim == TRUE) {
            month <- trim(data[[i]], c(variable, "adj_ret"), plt_trim_ratio) # may change here  
        } else {
            month <- trim(data[[i]], variable, plt_trim_ratio) # may change here  
        }
        month <- month %>% filter(e2p >= xlim[1] & e2p <= xlim[2])
        
        
        hb =  hexbin(month[[variable]], month$adj_ret, xbins = nbins, shape = 1, 
                     xbnds = range(month[[variable]], finite=T), ybnds = range(month$adj_ret, finite=T))
        countF = as.factor(hb@count)
        lvl = length(levels(countF))
        palette = grey(seq(0.7,0.1,length=lvl))[as.numeric(countF)]
        
        linear <- linearRes[[i]]$coefficients
        robust <- robustRes[[i]]$coefficients
        
        hex = hexbinplot(as.formula(paste("adj_ret * 100 ~", variable)), data=month, aspect="1", xbins=nbins, 
                         style = style, pen = palette, border = rgb(1,1,1),
                         panel=function(x, y, ...)
                         {
                             panel.hexbinplot(x, y, ...)
                             panel.abline(a = linear[1], linear[2], col=2)
                             panel.abline(a = robust[1], b = robust[2], col=1)
                         },
                         key=list(background = "gray97", corner = c(0.01, 0.99), #space="right", 
                                  lines=list(col=1:2, lty=1, lwd=1) ,
                                  text=list(c(paste("RobustMM Slope:", round(robust[2], 2)),
                                              paste("OLS Slope:", round(linear[2], 2))))
                         ),
                         main = paste(names(data)[i], ":", dim(data[[i]])[1],"stocks \n RobustMM:", efficiency, " vs  OLS:", outliers),
                         colorkey = hex_legend, xlab = variable, ylab = "Return(%)",
                         minarea = 0.15, maxarea = 0.9)
        print(hex)
    }
    dev.off()
}


# pltFunc_hex_gg <- function(time = c("63_90", "80_15", "63_15", "63_18"), variable, outliers = c("win", "trim"), efficiency = c("0.95", "0.99", "0.999"), ...) {
#   time <- match.arg(time)
#   outliers <- match.arg(outliers)
#   data <- get(paste("data_", time, sep = ""))
#   regRes <- get(paste(variable, time, sep=""))$res
#   linearRes <- regRes[[paste("lm_", outliers, sep = "")]]
#   robustRes <- regRes[[paste("mm_", substr(efficiency, 3, 5), sep = "")]]
#   pdf(file = paste("plots/heat", variable, time, outliers, efficiency, ".pdf", sep = ""))
#   for (i in 1:length(data)) {
#     month <- data[[i]]
#     gg <- ggplot(month, )
#     plot(month[[variable]], month$adj_ret * 100, xlab = variable, ylab = "Adjusted Return(%)", 
#          main = paste(names(data)[i], ":", dim(month)[1],"stocks"), pch = 20, ...) # , xlim = c(-10, 10)
#     linear <- linearRes[[i]]$coefficients
#     abline(linear[1], linear[2], col = 2)
#     robust <- robustRes[[i]]$coefficients
#     abline(robust[1], robust[2])
#     legend("topleft", legend = c(paste("robustMM:", efficiency, " Slope:(", round(robust[2], 2), ")", sep=""),
#                                  paste("linear:",  outliers, " Slope(", round(linear[2], 2), ")", sep="")), 
#            col = 1:2, lty = 1, cex = 0.75)
#   }
#   dev.off()
# }

writeLines(c(
    '\\documentclass{article}',
    '\\begin{document}', 'Hello world!', '\\end{document}'
), 'test.tex')
tinytex::pdflatex('test.tex')