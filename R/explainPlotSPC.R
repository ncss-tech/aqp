#' @title Visual Explanation for \code{plotSPC}
#' 
#' @description Create a visual explanation for the many arguments to \code{plotSPC}. Call this function instead of \code{plotSPC}, all objects after \code{x} are passed on to \code{plotSPC}. Nearly all of the figures in the \href{https://ncss-tech.github.io/AQP/aqp/aqp-intro.html}{Introduction to SoilProfileCollection Objects tutorial} are created with this function.
#'
#' @author D.E. Beaudette
#' 
#' @seealso \code{\link{plotSPC}}
#' 
#' @keywords manip
#' 
#' @param x a \code{SoilProfileCollection} object
#' 
#' @param \dots arguments passed to \code{\link{plotSPC}}
#'
#' @return a list of internally-used ordering vectors and graphical offsets / scaling factors
#'
#' @keywords manip
#' @export
#' @examples
#'
#' # sample data
#' data(sp4)
#' depths(sp4) <- id ~ top + bottom
#'
#' # proposed vector of relative positions, overlap likely
#' pos <- c(1, 1.1, 3, 4, 5, 5.2, 7, 8, 9, 10)
#'
#' # try it
#' explainPlotSPC(sp4, name = 'name', relative.pos=pos)
#'
#' # attempt to fix using an integer sequence, short-circut will prevent adjustments
#' explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(1:10))
#'
#' # attempt to adjust using defaults
#' explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos))
#'
#' # attempt to adjust and tinker with defaults
#' explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos, adj = 0.2))
#'
#' # enforce larger space between
#' explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos, thresh = 0.7))
#'
#' # more complex adjustments required
#' pos <- c(1, 2, 3, 3.3, 5, 5.1, 5.5, 8, 9, 10)
#'
#' # tinker
#' explainPlotSPC(sp4, name = 'name', relative.pos = pos)
#' explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos))
#' explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos, thresh = 0.7))
#' explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos, thresh = 0.7, adj = 0.2))
#'
#' # no solution possible given these constraints
#' explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos, thresh = 1, adj = 0.2))
#' 
#' # use alternative strategy
#' explainPlotSPC(sp4, name = 'name', relative.pos = fixOverlap(pos, thresh = 1, method = 'E'))
#'
explainPlotSPC <- function(x, ...) {
  plotSPC(x, id.style='side', ...)
  box()
  
  # figure coordinates
  .usr <- par('usr')
  
  # get last plot parameters
  lsp <- get('last_spc_plot', envir=aqp.env)
  
  # get max depth by profile
  max.depths <- profileApply(x, max)
  
  # re-order max depths
  max.depths <- max.depths[lsp$plot.order]
  
  # apply y-offset and scaling factor
  # y-offset is a vector
  scaled.max.depths <- lsp$y.offset + (lsp$scaling.factor * max.depths)
  
  # suitable location for y-space annotation
  y.space.x <- 2.5
  
  # suitable location for x-space annotation
  # index of last profile + some
  x.space.x <- lsp$n + (length(x) * 0.05)
  
  # 95% of lower figure coordinates
  x.space.y <- .usr[3] * 0.95
  
  # original profile index text y-coordinate
  # roughly 10% of the max(transformed depths)
  original.profile.idx.y <- lsp$y.offset + (-max(scaled.max.depths) * 0.08)
  
  # segment depth starting locations
  # 90% of bottom of plotting region ----> scaled max depths 
  # or max(scaled depths), whichever is larger
  .segment.bottom.y <- pmax(.usr[3] * 0.9, max(scaled.max.depths, na.rm = TRUE))
  
  # inspect plotting area, very simple to overlay graphical elements
  segments(x0 = lsp$x0, x1 = lsp$x0, y0 = .segment.bottom.y, y1 = scaled.max.depths, lty=3, lwd=2, col='darkgreen')
  
  # profiles are centered at integers, from 1 to length(obj)
  axis(1, line=0.25, at=round(lsp$x0, 2), cex.axis=0.75, font=4, col='darkgreen', col.axis='darkgreen', lwd=2)
  mtext('canvas x-coordinate', side=1, line=2.25, font=4, col='darkgreen')
  
  # y-axis is based on profile depths
  axis(2, line=0.25, cex.axis=0.75, font=4, las=1, col='blue', col.axis='blue', lwd=2)
  mtext('canvas y-coordinate', side=2, line=2.25, font=4, col='blue')
  
  # show extra y-space
  arrows(x0=y.space.x, x1=y.space.x, y0=0, y1=-lsp$extra_y_space, length = 0.08, code = 3, col='orange', lwd=1)
  text(x=y.space.x, y=-lsp$extra_y_space/2, labels = 'extra y-space', cex=0.65, pos=2, font=3, col='orange')
  text(x=y.space.x, y=-lsp$extra_y_space/2, labels = lsp$extra_y_space, cex=0.85, pos=4, font=2, col='orange')
  
  # show extra x-space
  arrows(x0=lsp$n, x1=lsp$n + lsp$extra_x_space, y0=x.space.y, y1=x.space.y, length = 0.08, code = 3, col='orange', lwd=1)
  text(x=x.space.x, y=x.space.y, labels = 'extra x-space', cex=0.65, pos=3, font=3, col='orange')
  text(x=x.space.x, y=x.space.y, labels = lsp$extra_x_space, cex=0.85, pos=1, font=2, col='orange')
  
  # demonstrate width on first profile
  arrows(x0=lsp$x0[1] - lsp$width, x1=lsp$x0[1] + lsp$width, y0=x.space.y, y1=x.space.y, length = 0.08, code = 3, col='orange', lwd=1)
  text(x=lsp$x0[1], y=x.space.y, labels = 'width', cex=0.65, pos=3, font=3, col='orange')
  text(x=lsp$x0[1], y=x.space.y, labels = lsp$width, cex=0.85, pos=1, font=2, col='orange')
  
  # plotting order
  text(x=lsp$x0, y=original.profile.idx.y, labels=lsp$plot.order, col='darkred', font=4, cex=0.75)
  mtext('original profile index', side=3, line=0, font=4, col='darkred')
  
  # hz depth label adjustment index
  if(! all(is.na(lsp$hz.depth.LAI))) {
    # annotate
    .LAI.txt <- sprintf("LAI\n%s", round(lsp$hz.depth.LAI, 2))
    text(x = lsp$x0 + lsp$width, y = .segment.bottom.y, pos = 4, cex = 0.66, labels = .LAI.txt)
  }
  
  
  invisible(lsp)
}

