library(aqp)
library(av)

x <- data.frame(
  id = 'A',
  top = c(0, 5, 8, 10, 15, 45, 50, 53, 100),
  bottom = c(5, 8, 10, 15, 45, 50, 53, 100, 135),
  name = c('Oi', 'A', 'AB', 'BA', 'Bt1', 'Btqm', 'Btqzny', 'C1', 'C2')
)

depths(x) <- id ~ top + bottom
hzdesgnname(x) <- 'name'

plotIT <- function(n = 1, cn = 0.85) {
  
  # label collision has to know about character expansion applied to the labels
  # AFTER plot has been started
  
  for(i in 1:n) {
    par(mar = c(0, 0, 0, 0))
    plotSPC(x, plot.depth.axis = FALSE, width = 0.1, print.id = FALSE, name = NA, cex.names = cn, name.style = 'center-center', shrink = TRUE, shrink.thin = 4, hz.depths = FALSE)
    
    # boilerplate already done in plotSPC
    htb <- horizonDepths(x)
    top <- x[[htb[1]]]
    bottom <- x[[htb[2]]]
    
    # number of horizons
    nh <- length(bottom)
    
    # only horizons 2:(n-1) are in play here
    s <- top[-1]
    labs <- as.character(s)
    
    # text height is about right for threshold detection
    tr <- abs(strheight('1', cex = cn)) 
    
    # find / fix overlap
    p <- suppressMessages(fixOverlap(s, thresh = tr, adj = tr/2, min.x = min(top) + tr, max.x = max(bottom) - tr))
    
    # annotation to help understand what is happening
    
    segments(x0 = rep(1.1, times = length(p)), y0 = s, x1 = rep(1.13, times = length(p)), y1 = p)
    text(x = rep(1.13, times = length(p)), y = p, labels = labs, col = 2, pch = 16, cex = cn, font = 2, adj = 0) 
    
    text(x = rep(1.13, times = 1), y = top[1], labels = top[1], col = 1, pch = 16, cex = cn, font = 2, adj = 0) 
    text(x = rep(1.13, times = 1), y = bottom[nh], labels = bottom[nh], col = 1, pch = 16, cex = cn, font = 2, adj = 0)
    
    text(x = rep(0.89, times = 1), y = top[1], labels = top[1], col = 1, pch = 16, cex = cn, font = 2, adj = 1) 
    text(x = rep(0.89, times = 1), y = bottom[nh], labels = bottom[nh], col = 1, pch = 16, cex = cn, font = 2, adj = 1)
    
    text(x = rep(0.89, times = length(s)), y = s, labels = labs, col = 1, pch = 16, cex = cn, font = 2, adj = 1) 
  }
  
}

plotIT(cn = 1)


# Play 1 plot per sec, and use an interpolation filter to convert into 10 fps
video.file <- file.path('E:/temp', 'hz-depth-fixes.mp4')

av::av_capture_graphics(plotIT(100), video.file, 480, 640, res = 144, framerate = 10)






