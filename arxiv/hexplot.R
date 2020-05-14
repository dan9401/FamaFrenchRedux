library(hexbin)
library(ggplot2)
library(Hmisc)

load("/home/gsb25/Downloads/data_63_90.Rdata")

# TODO: Add legend
hexplot <- function(data, x, y, slopes, n.models = 2, n.bins = 40, title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL) {
  # data: data.table containing x and y variables
  # x: variable for x-axis
  # y: variable for y-axis
  
  dat <- na.omit(data, c(y, x))
  
  # Get bins
  bins <- hexbin(x = dat[[x]], y = dat[[y]], xbins = n.bins)
  
  if (!is.null(subtitle)) {
    main <- paste0(main, "\n\n", subtitle)
  }
  
  # Create hexbin plot
  plot(bins,
       style = 'centroids',
       border = gray(0.65),
       colramp = function(n) { LinGray(n, beg=1, end=255) },
       xlab = xlab,
       ylab = ylab,
       main = main,
       clip="off")
  
  # Add slopes
  for (j in 1:n.models) {
    dt = slopes[[i]]
    color = abline.color[[i]]
    lty = abline.lty[[i]]
    hexVP.abline(plt$plot.vp, a = dt[1], b = dt[2], col = color, lty = lty)
  }
}

# TODO: Add ablines, legend, and different sizing for bins
ggplot_hex <- function(data, x, y, n.bins = 40, cuts = 5, title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL) {
  # data: data.table containing x and y variables
  # x: variable for x-axis
  # y: variable for y-axis
  
  dat <- na.omit(data, c(y, x))
  n <- nrow(x)
  
  # Get bins
  bins <- hexbin(x = dat[[x]], y = dat[[y]], xbins = n.bins)
  
  ggbins <- data.frame(hcell2xy(bins), count = bins@count,
                       xo = bins@xcm, yo = bins@ycm,
                       c = cut2(bins@count, g = cuts))
  
  if (!is.null(subtitle)) {
    main <- paste0("tt", "\n\n", subtitle)
  }
  
  gg <- ggplot(ggbins) +
          geom_hex(aes(x = x, y = y, fill = c),
                   color = "black", stat = "identity")
  
  gg
}
