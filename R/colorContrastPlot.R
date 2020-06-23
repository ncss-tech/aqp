
## quickly display two sets of colors and soil color contrast metrics
# m1: munsell colors
# m2: munsell colors
# col.cex: scaling for color labels
# col.font: font for color labels
# d.cex: scaling for contrast metric labels
# cc.font: font for contrast class
# dE00.font: font for delta-E00 label
# labels: vector of labels for colors being compared
# printMetrics: add contrast metrics
# \dots{}: further arguments passed to colorspace::swatchplot
colorContrastPlot <- function(m1, m2, col.cex=1, col.font=2, d.cex=1, cc.font=3, dE00.font=1, labels=c('m1', 'm2'), label.cex=1, label.font=1, printMetrics=TRUE, ...) {
  
  # compose list of swatchplot
  colors <- list(
   parseMunsell(m1), 
   parseMunsell(m2)
  ) 
  names(colors) <- labels
  
  # basic plot
  colorspace::swatchplot(colors, cex=label.cex, font=label.font, ...)
  
  # eval constrat metrics
  d <- colorContrast(m1, m2)
  
  ## TODO this could use some more thought
  # helpers for computing position
  nx <- length(m1)
  ny <- 2
  
  # positions of Munsell color labels
  x.pos <- seq(from=0, to=1, by=1/nx)[1:nx]
  y.pos1 <- rep(0.51, times=nx)
  y.pos2 <- rep(0.01, times=nx)
  
  # create high-contrast label colors
  m1.label.colors <- invertLabelColor(colors$m1)
  m2.label.colors <- invertLabelColor(colors$m2)
  
  # annotate top row
  text(x.pos, y.pos1, labels = m1, col=m1.label.colors, font=col.font, cex=col.cex, adj = c(-0.125, -0.33))
  # annotate bottom row
  text(x.pos, y.pos2, labels = m2, col=m2.label.colors, font=col.font, cex=col.cex, adj = c(-0.125, -0.33))
  
  # usually want to add the contrast metrics, but sometimes it is handy to suppress
  if(printMetrics) {
    # positions for color contrast class
    cc.y <- rep(0.45, times=nx)
    cc.lab <- as.character(d$cc)
    text(x.pos, cc.y, labels = cc.lab, adj = 0, font=cc.font, cex=d.cex)
    
    # positions for dE00
    dE00.y <- rep(0.4, times=nx)
    dE00.lab <- signif(d$dE00, 3)
    
    # this is clunky but I can't figure out a better solution
    # iterate over delta-E00, format with plotmath and add text
    # note on plotmath formatting and bquote() tricks
    # https://lukemiller.org/index.php/2017/05/r-plotmath-functions-combined-with-variable-values/
    for(i in 1:nx) {
      dE00.lab.expr <- bquote(Delta*E['00']~.(dE00.lab[i]))
      text(x.pos[i], dE00.y[i], labels = dE00.lab.expr, adj = 0, font=3, cex=d.cex)
    } 
  }
  
}

