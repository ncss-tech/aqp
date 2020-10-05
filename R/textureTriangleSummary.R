## ideas:

# 2016-03-21
# may not work correctly with compositions
# good idea + code here:
# http://stackoverflow.com/questions/31893559/r-adding-alpha-bags-to-a-2d-or-3d-scatterplot
#

## TODO:
## 1. allow iteration over groups for plotting multiple horizons
## 2. consider embedding into lattice panels

# helper function
.get.ssc.low.rv.high <- function(ssc, p, delta, TT.obj) {

  if(!requireNamespace("soiltexture", quietly = TRUE))
    stop("package `soiltexture` is required", call.=FALSE)

  # sanity checks
  if(delta < 1)
    stop('delta values smaller than 1 may result in memory overflow', call.=FALSE)

  # compute _univariate_ low - rv - high by variable
  ssc.stats <- apply(ssc, 2, Hmisc::hdquantile, probs=p, na.rm=TRUE)

  # re-order for plotting
  sand.stats <- sort(ssc.stats[, 1])
  silt.stats <- sort(ssc.stats[, 2])
  clay.stats <- sort(ssc.stats[, 3])

  # make a grid of sand, silt, clay values within the low--high range of the data
  sand.seq <- round(seq(from=sand.stats[1], to=sand.stats[3], by=delta))
  silt.seq <- round(seq(from=silt.stats[1], to=silt.stats[3], by=delta))
  clay.seq <- round(seq(from=clay.stats[1], to=clay.stats[3], by=delta))
  g <- expand.grid(SAND=sand.seq, SILT=silt.seq, CLAY=clay.seq)

  # subset to only include those sand, silt, clay values that sum to 100% (0.01% tolerance)
  real.textures <- which(abs(apply(g, 1, sum) - 100) < 0.01 )
  g <- g[real.textures, ]

  # plot low and high values with no symbol, so that we can access the {x,y} screen coordinates
  tp.low.high <- soiltexture::TT.points(tri.data = g, geo = TT.obj, pch = NA, tri.sum.tst = FALSE)

  # create range polygon
  poly.order <- chull(tp.low.high$x, tp.low.high$y)

  # package quantiles and bounding polygon geometry
  res <- list(
    stats = ssc.stats, 
    range = list(
      x = tp.low.high$x[poly.order], 
      y = tp.low.high$y[poly.order]
    )
  )
  
  return(res)
}


# compute and plot "low"--"representative value"--"high" soil textures based on:
# ssc: data.frame/matrix of [sand, silt, clay]
# p: requested percentiles


#' 
#' @title Soil Texture Low-RV-High as Defined by Quantiles
#' 
#' @author D.E. Beaudette, J. Nemecek, K. Godsey
#' 
#' @description This function accepts soil texture components (sand, silt, and clay percentages) and plots a soil texture triangle with a "representative value" (point) and low-high region (polygon) defined by quantiles (estimated with \code{Hmisc::hdquantile}). Marginal quantiles of sand, silt, and clay are used to define the boundary of a low-high region. The defualt settings place the RV symbol at the texture defined by marginal medians of sand, silt, and clay. The default low-high region is defined by the 5th and 95th marginal percentiles of sand, silt, and clay.
#' 
#' @param ssc \code{data.frame} with columns: 'SAND', 'SILT', 'CLAY', values are percentages that should add to 100. No NA allowed.
#' 
#' @param p vector of percentiles (length = 3) defining 'low', 'representative value', and 'high'
#' 
#' @param delta grid size used to form low-high region
#' 
#' @param rv.col color used for representative value (RV) symbol
#' 
#' @param range.border color used for polygon border enclosing the low-high region
#' 
#' @param range.col color used for polygon enclosing the low-high region
#' 
#' @param range.alpha transparency of the low-high range polygon (0-255)
#' 
#' @param range.lty line style for polygon enclosing the low-high region
#' 
#' @param range.lwd line weight polygon enclosing the low-high region
#' 
#' @param main plot title
#' 
#' @param legend.cex scaling factor for legend
#' 
#' @param legend logical, enable/disable automatic legend
#' 
#' @param \dots further arguments passed to \code{soiltexture::TT.points}
#' 
#' @return an invisible \code{matrix} with marginal percentiles of sand, silt, and clay 
#' 
#' @seealso \code{\link{bootstrapSoilTexture}}
#' 
#' @keywords hplots
#' 
#' @examples
#' 
#' \donttest{
#' 
#' if(
#' requireNamespace("Hmisc") &
#'   requireNamespace("compositions") &
#'   requireNamespace("soiltexture")
#' ) {
#'   
#'   # sample data
#'   data('sp4')
#'   
#'   # subset rows / columns
#'   ssc <- sp4[grep('^Bt', sp4$name), c('sand', 'silt', 'clay')]
#'   names(ssc) <- toupper(names(ssc))
#'   
#'   # make figure, marginal percentiles are silently returned
#'   stats <- textureTriangleSummary(
#'     ssc, pch = 1, cex = 0.5, 
#'     range.alpha = 50, 
#'     range.lwd = 1,
#'     col = grey(0.5), 
#'     legend = FALSE
#'   )
#'   
#'   # check
#'   stats
#'   
#'   # simulate some data and try again
#'   s <- bootstrapSoilTexture(ssc, n = 100)$samples
#'   
#'   # make the figure, ignore results
#'   textureTriangleSummary(
#'     s, pch = 1, cex = 0.5, 
#'     range.alpha = 50, 
#'     range.lwd = 1,
#'     col = grey(0.5), 
#'     legend = FALSE
#'   )
#' }
#' 
#' }
#' 
textureTriangleSummary <- function(ssc, p = c(0.05, 0.5, 0.95), delta = 1, rv.col = 'red', range.border = 'black', range.col = 'RoyalBlue', range.alpha = 80, range.lty = 1, range.lwd = 2, main = 'Soil Textures', legend.cex = 0.75, legend = TRUE, ...) {

  # sanity check
  if(
    ! requireNamespace("soiltexture", quietly = TRUE) | 
    ! requireNamespace("Hmisc", quietly = TRUE)
  ) {
    stop("packages `Hmisc` and `soiltexture` are required", call.=FALSE)
  }
  
  # TODO: filter NA?
  
  # check for appropriate column names
  # all must be present
  # may be other columns as well, ignore those
  name.check <- sapply(c('SAND', 'SILT', 'CLAY'), function(i) {
    any(names(ssc) %in% i)
  })
  
  if(! all(name.check)) {
    stop('`ssc` must contain columns: `SAND`, `SILT`, `CLAY`.')
  }
  
  # subset via column names
  ssc <- ssc[, c('SAND', 'SILT', 'CLAY')]
  
  
	# setup colors
	range.col <- rgb(t(col2rgb(range.col)), maxColorValue=255, alpha=range.alpha)

	# setup legend elements
  rv.text <- paste0('Sample RV (', paste(p[c(2)], collapse='-'), ')')
  low.high.range.text <- paste0('Low-High Range (', paste(p[c(1,3)], collapse='-'), ')')
	legend.text <- c(rv.text, low.high.range.text)
	legend.cols <- c('black', 'black')
	legend.bg <- c(rv.col, range.col)
	legend.pch <- c(22, 22)
  
	# setup plot
	TT <- soiltexture::TT.plot(
	  class.sys = "USDA-NCSS.TT",    # use "our" texture triangle
	  main = main,          # title
	  tri.sum.tst = FALSE,            # do not test for exact sum(sand, silt, clay) == 100
	  cex.lab = 0.75,                 # scaling of label text
	  cex.axis = 0.75,                # scaling of axis
	  frame.bg.col = 'white',         # background color
	  class.lab.col = 'black',        # color for texture class labels
	  lwd.axis = 1.5,
	  lwd.lab = 2,
	  arrows.show=TRUE
	)
	
	# compute RV / range polygon for data
	res <- .get.ssc.low.rv.high(ssc,  p=p, delta=delta, TT.obj = TT)
	
	# add polgon defining range of data
	polygon(res$range$x, res$range$y, col=range.col, border = range.border, lty = range.lty, lwd = range.lwd)
	
	# add original data, passing in additional arguments
	soiltexture::TT.points(tri.data = ssc, geo = TT, tri.sum.tst=FALSE, lwd = 1, ...)
	
	# plot population RV
	soiltexture::TT.points(
	  tri.data = data.frame(t(res$stats[2, ])), 
	  geo = TT, 
	  bg = rv.col, 
	  pch = 22, 
	  cex = 1.25, 
	  lwd = 1, 
	  tri.sum.tst = FALSE
	  )
	
	# legend
	if(legend) {
	  legend(
	    'topleft', 
	    legend = legend.text, 
	    pt.bg = legend.bg, 
	    pch = legend.pch, 
	    col = legend.cols, 
	    bty = 'n', 
	    cex = legend.cex, 
	    pt.cex = 1.25, 
	    horiz = TRUE
	  ) 
	}
	
  # invisibly return stats
	invisible(res$stats)

}

