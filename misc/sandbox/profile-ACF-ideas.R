##
## Use auto-correlation function to investigate / document / visualize the 
## vertical anisotropy within soil profiles
## TODO: keep track of where ACF -> some small value
##
##
##


library(aqp)
library(soilDB)

x <- fetchKSSL('drummer')

x <- x[1:10, ]

par(mar = c(0, 0, 3, 2))
plotSPC(x, color = 'estimated_om', max.depth = 250)
plotSPC(x, color = 'db_13b', max.depth = 250)
plotSPC(x, color = 'clay', max.depth = 250)



## idea #1

acfPlot <- function(x, v, md = 125) {
  
  # resample to 1cm slices, equal weighting of horizon data
  y <- dice(x)
  
  # limit ACF to max depth
  y <- trunc(y, 0, md)
  
  # ACF by profile 
  a <- profileApply(y, function(i, var = v) {
    .a <- acf(i[[var]], lag.max = nrow(i), plot = TRUE)$acf
    
    # positive values
    .a <- .a[which(.a > 0)]
    
    # integral of positive ACF
    sum(.a)
    
    # optional idea:
    # number of lags to 0 ACF
    # length(.a)
  })
  
  
  
  idx <- order(a, decreasing = TRUE)
  
  par(mar = c(4, 0, 3, 2))
  
  plotSPC(x, color = v, name.style = 'center-center', cex.names = 0.8, width = 0.33, plot.order = idx)
  
  axis(side = 1, at = 1:length(x), labels = format(a[idx], digits = 3))
  
  .txt <- sprintf('Integral of positive ACF from top-most horizon to %scm', md)
  mtext(side = 1, text = .txt, at = 0.5, adj = 0, line = 2.5, font = 2)
  
}


acfPlot(x, 'estimated_oc')

x$ln_soc <- log(x$estimated_oc)
acfPlot(x, 'ln_soc')

d.2 <- lapply(1:10, random_profile, SPC=TRUE, n=6:10, n_prop=1)
d.2 <- combine(d.2)

acfPlot(d.2, v = 'p1', md = 100)



## idea #2:

acfPlot2 <- function(x, v, resample = FALSE) {
  
  if(resample) {
    y <- dice(x)
  } else {
    y <- x
  }
    
  ## TODO: needs more thought
  a <- profileApply(y, simplify = TRUE, FUN = function(i, var = v) {
    .res <- acf(
      i[[var]],
      plot = FALSE, 
      lag.max = nrow(i)
    )
    
    # just the ACF
    .res <- .res$acf
    
    # first value isn't interesting
    .res[1] <- NA
    return(.res)
  }
  )
  
  y$.acf <- a
  
  par(mar = c(0, 2, 3, 2), xpd = NA)
  plotSPC(x, color = v, max.depth = 200, name.style = 'center-center', cex.names = 0.8, x.idx.offset = -0.5)
  
  plotSPC(y, color = '.acf', max.depth = 200, divide.hz = FALSE, name = NA, add = TRUE, width = 0.15, print.id = FALSE, show.legend = FALSE, depth.axis = FALSE, col.palette = hcl.colors(100, 'greens', rev = TRUE))
  
  
}


acfPlot2(x, 'estimated_oc')
acfPlot2(x, 'estimated_oc', resample = TRUE)

acfPlot(x, 'clay')
acfPlot2(x, 'clay')
acfPlot2(x, 'clay', resample = TRUE)

acfPlot(x, 'ph_h2o')
acfPlot2(x, 'ph_h2o')
acfPlot2(x, 'ph_h2o', resample = TRUE)

