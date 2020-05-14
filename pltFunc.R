pltFunc_hex <- function(data, variable, estimates, cropping = list(),
                        file = NULL, nbins = 25, median = FALSE, rob_compare = FALSE, ...) {
  efficiency = "0.95"
  outliers = "winorize"

  pdf(file = file)

  efficiency = 0.95
  upper <- lmrobdet.control(efficiency = 0.95)$tuning.psi[["upper"]]

  for (i in 1:length(data)) {
    linear <- estimates$linear[i,]
    robust <- estimates$robust[i,]
    month <- na.omit(data[[i]], variable)
    
    scale <- robust[["scale"]]
    ar <- upper * scale
    ratio <- get_pn_ratio(month, variable, ar, robust[1], robust[3])
    
    monthls <- crop(month, variable, cropping)
    month <- monthls$data
    qx <- monthls$qx
    
    medians <- bin_medians(month, nbins, variable)
    mx <- medians$mx
    my <- medians$my

    hb =  hexbin(month[[variable]], month$adj_ret, xbins = nbins, shape = 1,
                 xbnds = range(month[[variable]], finite=T), ybnds = range(month$adj_ret, finite=T))
    countF = as.factor(hb@count)
    lvl = length(levels(countF))
    palette = grey(seq(0.7,0.1,length=lvl))[as.numeric(countF)]

    if (rob_compare) {
      key=list(background = "gray97", corner = c(0.01, 0.99), #space="right",
               lines=list(col=1:2, lty=1, lwd=1) ,
               text=list(c(paste("RobustMM Slope:", round(robust[3], 2), "(", round(robust[4],2), ")"),
                           paste("RobustMM Slope(w/o winsorization):", round(linear[3], 2), "(", round(linear[4],2), ")"),
                           paste("MM Rejection: ", round(ratio$pos, 2), "%(+)  ", round(ratio$neg, 2), "%(-)", sep = "")
                           )))
      main = paste(names(data)[i], ":", dim(data[[i]])[1],"stocks \n RobustMM vs RobustMM without winsorization:", efficiency)
    } else {
      key=list(background = "gray97", corner = c(0.01, 0.99), #space="right",
               lines=list(col=c(1, 2, 1), lty=c(1, 1, 2), lwd=1) ,
               text=list(c(paste("RobustMM Slope:", round(robust[3], 2), "(", round(robust[4],2), ")"),
                           paste("OLS Slope:", round(linear[3], 2), "(", round(linear[4],2), ")"),
                           paste("MM Rejection: ", round(ratio$pos, 2), "%(+)  ", round(ratio$neg, 2), "%(-)", sep = "")
               )))
      main = paste(names(data)[i], ":", dim(data[[i]])[1],"stocks \n RobustMM:", efficiency, " vs  OLS:", outliers)
    }

    hex = hexbinplot(as.formula(paste("adj_ret * 100 ~", variable)), data=month,
                     aspect="1", xbins=nbins, style = "centroids", minarea = 0.1, maxarea = 0.9,
                     pen = palette, border = rgb(1,1,1),

                     panel=function(x, y, ...)
                     {
                       panel.hexbinplot(x, y, ...)
                       if (median == TRUE) {
                         panel.lines(mx, my, col = 4)
                         panel.points(mx, my, pch = 16, col = 4)
                       }
                       panel.abline(a = linear[1], b = linear[3], col=2, lwd = 1.5)
                       panel.abline(a = robust[1], b = robust[3], col=1, lwd = 1.5)
                       panel.abline(a = robust[1] + ar, b = robust[3], lty = 2)
                       panel.abline(a = robust[1] - ar, b = robust[3], lty = 2)
                       panel.abline(h = 0, lty = 3, col = gray(0.3))
                       if (!is.null(qx)) {
                         panel.abline(v = qx[[1]], lty = 1, col = gray(0.8))
                         panel.abline(v = qx[[2]], lty = 1, col = gray(0.8))
                       }
                     },
                     main = main, key = key,
                     xlab = variable, ylab = "Return(%)", colorkey = TRUE)#, ...)
    print(hex)
  }
  dev.off()
}

bin_medians <- function(data, nbins, variable) {
  data <- data %>% arrange(data[[variable]])
  ret <- data$adj_ret * 100
  var <- data[[variable]]
  n <- length(ret)
  bin_size <- round(n/nbins)
  i <- 1
  x <- c()
  y <- c()
  while (i <= n) {
    y <- c(y, median(ret[i:min(i+bin_size, n)]))
    x <- c(x, (var[i]+var[min(i+bin_size, n)])/2)
    i <- i + bin_size
  }
  return(list(mx = x, my = y))
}

crop <- function(data, variable, cropping) {
  type <- cropping$type
  xcrop <- cropping$xcrop
  ycrop <- cropping$ycrop
  if (is.null(type)) {
    type <- ""
  }
  qx <- NULL
  
  if (grepl("h", type)) {
    if (length(xcrop) == 1) {
      qx <- quantile(data[[variable]], c(xcrop, 1-xcrop), na.rm = TRUE)
    } else if (length(xcrop) == 2) {
      qx <- xcrop
    } else {
      stop("xcrop must be one of 1 or 2")
    }
  }

  if (grepl("v", type)) {
    if (length(ycrop) == 1) {
      qy <- quantile(data$adj_ret, c(ycrop, 1-ycrop), na.rm = TRUE)
    } else if (length(xcrop) == 2) {
      qy <- ycrop
    } else {
      stop("ycrop must be one of 1 or 2")
    }
  }

  if (grepl("h", type)) {
    data[[variable]] <- pmin(pmax(data[[variable]], qx[[1]]), qx[[2]])
  }
  if (grepl("v", type)) {
    data <- data[ data$adj_ret > qy[[1]] & data$adj_ret < qy[[2]] ,]
  }

  return(list(data=data, qx=qx))
}

get_pn_ratio <- function(data, variable, ar, intercept, slope) {
    pos <- sum(data$adj_ret * 100 > intercept + slope * data[[variable]] + ar)/dim(data)[1]
    neg <- sum(data$adj_ret * 100 < intercept + slope * data[[variable]] - ar)/dim(data)[1]
    list(pos = pos * 100, neg = neg * 100)
}


hexbinplot.formula <- function(x, data = NULL,
           prepanel = prepanel.hexbinplot,
           panel = panel.hexbinplot,
           groups = NULL,
           aspect = "xy",
           trans = NULL,
           inv = NULL,
           colorkey = TRUE,
           ...,
           maxcnt,
           legend = NULL,
           legend.width = TRUE,
           subset = TRUE)
{
  ocall <- sys.call(sys.parent())
  ocall[[1]] <- quote(hexbinplot)
  ccall <- match.call()
  if (is.logical(legend.width)) legend.width <- 1.2 * as.numeric(legend.width)
  if (is.character(aspect) && aspect == "fill")
    stop("aspect = 'fill' not permitted")
  if (!is.null(trans) && is.null(inv))
    stop("Must supply the inverse transformation 'inv'")
  ccall$data <- data
  ccall$prepanel <- prepanel
  ccall$panel <- panel
  ccall$aspect <- aspect
  ccall$trans <- trans
  ccall$inv <- inv
  ccall$legend <- legend
  ccall[[1]] <- quote(lattice::xyplot)
  ans <- eval(ccall, parent.frame())
  
  ## panel needs to know aspect ratio to calculate shape
  ans <- update(ans, .aspect.ratio = ans$aspect.ratio)
  
  ## also need maxcnt, o.w. can't draw legend, panels not comparable
  ## either
  if (missing(maxcnt))
    maxcnt <-
    max(mapply(panel.hexbinplot, ## note: not 'panel'
               x = lapply(ans$panel.args, "[[", "x"),
               y = lapply(ans$panel.args, "[[", "y"),
               .xlim =
                 if (is.list(ans$x.limits)) ans$x.limits
               else rep(list(ans$x.limits), length(ans$panel.args)),
               .ylim =
                 if (is.list(ans$y.limits)) ans$y.limits
               else rep(list(ans$y.limits), length(ans$panel.args)),
               MoreArgs =
                 c(ans$panel.args.common,
                   list(.prelim = TRUE, .cpl = NA))))
  ans <- update(ans, maxcnt = maxcnt)
  if (colorkey)
    ans <-
    update(ans,
           legend = updateList(ans$legend,
                               list(right =
                                      list(fun = hexlegendGrob,
                                           args =
                                             list(maxcnt = maxcnt,
                                                  trans = trans,
                                                  inv = inv,
                                                  legend = legend.width,
                                                  ...)))))
  ans$call <- ocall
  ans
}


hexlegendGrob <- function(legend = 1.2,
           inner = legend / 5,
           cex.labels = 1,
           cex.title = 1.2,
           style = "colorscale",
           minarea = 0.05, maxarea = 0.8,
           mincnt = 1, maxcnt,
           trans = NULL, inv = NULL,
           colorcut = seq(0, 1, length = 17),
           density = NULL, border = NULL, pen = NULL,
           colramp = function(n) { LinGray(n,beg = 90,end = 15) },
           ...,
           vp = NULL,
           draw = FALSE)
{
  ## the formal arg matching should happen
  style <- match.arg(style, eval(formals(grid.hexagons)[["style"]]))
  if (style %in% c("centroids", "lattice", "colorscale")) {
    ## _______________tranformations_______________________
    if(is.null(trans))
    {
      sc <- maxcnt - mincnt
      bnds <- round(mincnt + sc * colorcut)
    }
    else
    {
      if(!is.function(trans) && !is.function(inv))
        stop("'trans' and 'inv' must both be functions if 'trans' is not NULL")
      con <- trans(mincnt)
      sc <- trans(maxcnt) - con
      bnds <- round(inv(con + sc * colorcut))
    }
  }
  
  ## grob
  ans <-
    switch(style,
           "centroids" = ,
           "lattice" = {
             warning("legend shows relative sizes")
             
             ## Note: it may not be impossible to get absolute
             ## sizes.  The bigger problem is that when
             ## [xy]bnds="data", the sizes (for the same count) may
             ## not be the same across panels.  IMO, that's a more
             ## useful feature than getting the absolute sizes
             ## right.
             
             radius <- sqrt(minarea + (maxarea - minarea) * colorcut)/1.5
             n <- length(radius)
             if(is.null(pen)) pen <- 1
             if(is.null(border)) border <- pen
             
             hexxy <- hexcoords(dx = 1, n = 1)[c("x", "y")]
             maxxy <- max(abs(unlist(hexxy)))
             hexxy <- lapply(hexxy, function(x) 0.5 * x/ maxxy)
             
             pol <-
               polygonGrob(x = 0.2 + rep(radius, each = 6) * rep(hexxy$x, n),
                           y = (-0.2 + rep(0.5 + 1:n, each = 6) +
                                  rep(radius, each = 6) * hexxy$y - 1) / n,
                           id.lengths = rep(6, n),
                           gp = gpar(fill = gray(seq(0.7,0.1,length=17)), col = border),
                           default.units = "npc")
             txt <-
               textGrob(as.character(bnds),
                        x = 0.2,
                        y = (1:n - 0.7) / n,
                        gp = gpar(cex = cex.labels),
                        default.units = "npc")
             ttl <- textGrob("Counts", gp = gpar(cex = cex.title))
             
             key.layout <-
               grid.layout(nrow = 2, ncol = 2,
                           heights =
                             unit(c(1.5, 1),
                                  c("grobheight", "grobheight"),
                                  data = list(ttl, txt)),
                           widths =
                             unit(c(1/n, 1),
                                  c("grobheight", "grobwidth"),
                                  data = list(pol, txt)),
                           respect = TRUE)
             key.gf <- frameGrob(layout = key.layout, vp = vp)
             
             key.gf <- placeGrob(key.gf, ttl, row = 1, col = 1:2)
             key.gf <- placeGrob(key.gf, pol, row = 2, col = 1)
             key.gf <- placeGrob(key.gf, txt, row = 2, col = 2)
             key.gf
           })
  if (draw)
  {
    grid.draw(ans)
    invisible(ans)
  }
  else ans
}

prepanel.hexbinplot <- function(x, y, type = character(0),...)
{
  if('tmd'%in%type){
    tmp <- x
    x <- (y + x)/sqrt(2)
    y <- (y - tmp)/sqrt(2)
  }
  ans <-
    list(xlim = range(x, finite = TRUE),
         ylim = range(y, finite = TRUE),
         dx = IQR(x,na.rm=TRUE),
         dy = IQR(y,na.rm=TRUE))
}

updateList <- function (x, val)
{
  if (is.null(x)) x <- list()
  modifyList(x, val)
}

pltSlopes <- function(res, file, time="") {
  pdf(file = file)
  for (i in 1:length(res)) {
    OLS <- res[[i]]$lm_win$res[,3]
    Robust_95 <- res[[i]]$mm_95$res[,3]
    slopes <- as.xts(cbind(OLS, Robust_95))
    colnames(slopes) <- c("OLS", "Robust 95%")
    print(slopeCompare(slopes, main=paste(time, names(res)[[i]])))
  }
  dev.off()
}

pltFunc2 <- function(data, variable, estimates, time = "", ...) {
  pltFunc_hex(data, variable, estimates, file = paste("plots/", time, "_", variable, "_nocrop.pdf", sep=""), nbins = 30)
  pltFunc_hex(data, variable, estimates, file = paste("plots/", time, "_", variable, "_hcrop.pdf", sep=""), nbins = 30,
              cropping = list(type = "h", xcrop = 0.005), median = FALSE)
  pltFunc_hex(data, variable, estimates, file = paste("plots/", time, "_", variable, "_hvcrop.pdf", sep=""), nbins = 30,
              cropping = list(type = "hv", xcrop = 0.005, ycrop = 0.005), median = FALSE)
}

pltFunc3 <- function(data, results, time = "", ...) {
  vars <- c("size", "b2m", "beta", "e2p")
  for (i in 1:length(results)) {
    variable <- vars[[i]]
    estimates <- list(robust = results[[variable]]$mm_95$res,
                      linear = results[[variable]]$lm_win$res)
    pltFunc2(data, variable, estimates, time)
  }
}

slopeCompare <- function(slopes, ...) {
  plot(slopes, main = "Slope Comparison", lwd = 0.5, col = 4,
       xlab = "Date", multi.panel = TRUE, yaxis.same = TRUE, grid.col = gray(0.8), yaxis.right = FALSE)
}

slopeCompare <- function(slopes, ...) {
  plot(slopes, lwd = 0.5, col = 4,
           xlab = "Date", multi.panel = TRUE, yaxis.same = TRUE, grid.col = gray(0.8), yaxis.right = FALSE, ...)
}