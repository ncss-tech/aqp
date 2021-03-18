## TODO: https://github.com/ncss-tech/aqp/issues/68
# * plot in layout similar to Munsell Color Book, alpha ~ frequency
# * distance matrix calculation takes time, careful with n > 1,000
# * gower's distance of {L,A,B} looks pretty good too
# * more control over graphical elements and par()
# * grid size estimation needs some work
 



#' @title Preview Colors
#' 
#' @description Preview colors arranged according to CIE2000 distances or manual specification.
#'
#' @param cols vector of R colors
#' @param method either "grid", "MDS", or "manual", see details
#' @param col.order integer vector used to order colors
#' @param nrow number of rows used by "grid" method
#' @param ncol number of columns used by "grid" method
#' @param border.col border color used by "grid" method
#' @param pt.cex point scaling factor used by "MDS" method
#' @param pt.pch point symbol used by "MDS" method
#' 
#' @details Color sorting is based on CIE2000 distances as calculated by \code{farver::compare_colour()}. The "grid" method arranges colors in a rectangular grid with ordering based on divisive hierarchical clustering of the pair-wise distances. Unique colors are used when \code{cols} contains more than 5,000 colors.
#' 
#' The "MDS" method arranges unique colors via classical multidimensional scaling (principal coordinates) via \code{MASS::cmdscale}.
#' 
#' Colors can be manually arranged by supplying a vector of integers to \code{col.order} and setting \code{method='manual'}.
#' 
#' @author D.E. Beaudette
#' 
#' 
#'
#' @return When \code{method = "grid" or "manual"} a vector of color order is returned. When \code{method = "MDS"}, the output from \code{MASS::cmdscale}.
#' @export
#'
#' @examples
#' 
#' # example data
#' data(sp2)
#' 
#' # convert into SoilProfileCollection object
#' depths(sp2) <- id ~ top + bottom
#' 
#' previewColors(sp2$soil_color)
#' previewColors(sp2$soil_color, method = 'MDS', pt.cex = 3)
#' 
#' # create colors using HCL space
#' cols.hcl <- hcl(h = 0:360, c = 100, l = 50)
#' 
#' # grid, colors sorted by dE00
#' previewColors(cols.hcl)
#' 
#' # manual specification
#' previewColors(cols.hcl, method = 'manual', col.order = 1:361)
#' 
#' # MDS
#' previewColors(cols.hcl, method = 'MDS', pt.cex = 1)
#' 
previewColors <- function(cols, method = c('grid', 'MDS', 'manual'), col.order = NULL, nrow = ceiling(sqrt(length(cols))), ncol = nrow, border.col = 'black', pt.cex = 2, pt.pch = 15) {

  # sanity check, need this for color distance eval
  if(!requireNamespace('farver', quietly = TRUE))
    stop('package `farver` is required', call.=FALSE)

  # remove NA
  cols <- na.omit(cols)
  
  # expected method argument
  method <- match.arg(method)

  # safety catch: a 5000 x 5000 element distance matrix is about all we really need for a preview
  if(length(cols) > 5000) {
    cols <- unique(cols)
    pt.cex <- pt.cex[1]
    pt.pch <- pt.pch
    warning('using unique colors, and first `pt.cex` / `pt.pch')
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

  if(method == 'grid') {
    # divisive hierarchical clustering for order
    col.order <- cluster::diana(d)$order

    # re-order colors and convert into a matrix
    m <- matrix(NA, nrow = nrow, ncol = ncol)
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
    points(mds, col = cols, cex = pt.cex, pch = pt.pch)
    box()
    
    invisible(mds)
  }

}

