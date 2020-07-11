## TODO: https://github.com/ncss-tech/aqp/issues/68
# * plot in layout similar to Munsell Color Book, alpha ~ frequency
# * distance matrix calculation takes time, careful with n > 1,000
# * gower's distance of {L,A,B} looks pretty good too
# * more control over graphical elements and par()

## quick preview of colors, sorted by clustering of CIE LAB representation
# grid size estimation needs some work
previewColors <- function(cols, method='grid', col.order=NULL, nrow=ceiling(sqrt(length(cols))), ncol=nrow, border.col='black', pt.cex=2) {

  # sanity check, need this for color distance eval
  if(!requireNamespace('farver', quietly = TRUE))
    stop('package `farver` is required', call.=FALSE)

  # remove NA
  cols <- na.omit(cols)

  # safety catch: a 5000 x 5000 element distance matrix is about all we really need for a preview
  if(length(cols) > 5000) {
    cols <- unique(cols)
    warning('using unique colors')
  }

  # use unique colors when using MDS
  if(method == 'MDS') {
    cols <- unique(cols)
  }

  # manual ordering, defined by col.order
  if(method == 'manual') {
    # sanity check
    if(is.null(col.order)) {
      stop('must specify color ordering vector', call. = FALSE)
    }

    # re-order colors and convert into a matrix
    m <- matrix(NA, nrow=nrow, ncol=ncol)
    m[1:length(cols)] <- cols[col.order]

    par(mar=c(1,0,3,0))
    plot(1, 1, type='n', axes=FALSE, xlab='', ylab='', xlim=c(0.5, ncol+0.5), ylim=c(0.5, nrow+0.5))
    rect(xleft = col(m) - 0.5, ybottom = row(m) -0.5, xright = col(m) + 0.5, ytop = row(m) + 0.5, col = m, border = border.col, lwd=0.5)

    invisible(col.order)
  }

  # hex represntation -> sRGB
  cols.srgb <- t(col2rgb(cols)) / 255
  # sRGB -> CIE LAB
  cols.lab <- grDevices::convertColor(cols.srgb, from = 'sRGB', to = 'Lab', from.ref.white='D65', to.ref.white='D65', clip=FALSE)

  # distances are based on CIE2000 color comparison
  # note: single argument -> all pair-wise distances
  # output is transposed relative to `dist` object
  d <- farver::compare_colour(cols.lab, from_space='lab', to_space = 'lab', method='CIE2000')
  d <- as.dist(t(d))

  # distances in CIELAB
  # col.order <- cluster::diana(cols.lab, stand = TRUE)$order

  if(method == 'grid') {
    # divisive hierarchical clustering for order
    col.order <- cluster::diana(d)$order

    # re-order colors and convert into a matrix
    m <- matrix(NA, nrow=nrow, ncol=ncol)
    m[1:length(cols)] <- cols[col.order]

    par(mar=c(1,0,3,0))
    plot(1, 1, type='n', axes=FALSE, xlab='', ylab='', xlim=c(0.5, ncol+0.5), ylim=c(0.5, nrow+0.5))
    rect(xleft = col(m) - 0.5, ybottom = row(m) -0.5, xright = col(m) + 0.5, ytop = row(m) + 0.5, col = m, border = border.col, lwd=0.5)

    invisible(col.order)
  }


  # MDS display of unique colors
  if(method == 'MDS') {

    # consider other methods
    # this is fairly robust to 0 distances
    mds <- cmdscale(d)

    # simple plot, no indication of density
    par(mar=c(1,1,3,1))
    plot(mds, type='n', axes=FALSE)
    grid(nx=10, ny=10, col=par('fg'))
    points(mds, pch=15, col=cols, cex=pt.cex)
    box()
  }

}

