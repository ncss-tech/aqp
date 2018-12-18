## TODO:
# * optional sorting strategies, log(RI) from Barron & Torrent, 1986
# * plot in nMDS-space via CIE2000 distances
# * plot in layout similar to Munsell Color Book, alpha ~ frequency
# * distance matrix calculation takes time, careful with n > 1,000

## quick preview of colors, sorted by clustering of CIE LAB representation
# grid size estimation needs some work
previewColors <- function(cols, nrow=ceiling(sqrt(length(cols))), ncol=nrow, border.col='black') {
  
  # sanity check, need this for color distance eval
  if(!requireNamespace('farver'))
    stop('pleast install the `farver` package.', call.=FALSE)
  
  # remove NA
  cols <- na.omit(cols)
  
  ## TODO
  # use unique colors when length(cols) > threshold
  # cols <- unique(cols)
  
  # hex represntation -> sRGB
  cols.srgb <- t(col2rgb(cols)) / 255
  # sRGB -> CIE LAB
  cols.lab <- grDevices::convertColor(cols.srgb, from = 'sRGB', to = 'Lab', from.ref.white='D65', to.ref.white='D65', clip=FALSE)
  # convert to DF for use in diana
  cols.lab <- as.data.frame(cols.lab)
  
  # divisive hierarchical clustering for order
  # distances are based on CIE2000 color comparison
  d <- farver::compare_colour(cols.lab, cols.lab, from_space='lab', to_space = 'lab', method='CIE2000')
  d <- as.dist(d)
  col.order <- cluster::diana(d)$order
  
  # re-order colors and convert into a matrix
  m <- matrix(NA, nrow=nrow, ncol=ncol)
  m[1:length(cols)] <- cols[col.order]
  
  par(mar=c(1,0,3,0))
  plot(1, 1, type='n', axes=FALSE, xlab='', ylab='', ylim=c(ncol+0.5, 0.5), xlim=c(0.5, nrow+0.5))
  rect(xleft = col(m) - 0.5, ybottom = row(m) -0.5, xright = col(m) + 0.5, ytop = row(m) + 0.5, col = m, border = border.col, lwd=0.5)
}

