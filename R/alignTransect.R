
## TODO: think about how to better integrate this into aqp and other packages
## TODO: dump scales import

# cheap alternative to scales::rescale(..., to = c(x0, x1))
.rescaleRange <- function(x, x0, x1) {
  res <- (x1 - x0) * ( (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) + x0
  return(res)
}



#' Calculate Relative Positions from Transect Data
#' 
#' @description This function is used to support relative positioning of soil profiles by `plotSPC`, based on transect or gradient values typically associated with a site level attribute (e.g. elevation). Gradient values specified in `x` are translated to the range used by `plotSPC` (usually `1, length(SPC)`) specified in `x.min` and `x.max`.
#'
#' @param x numeric vector, describing values along a transect: distance, elevation, climatic variables, etc.. Typically sourced from the site level attributes of a `SoilProfileCollection` object. Order is not important.
#' 
#' @param x.min numeric, lower boundary to reltative position scale
#' @param x.max numeric, upper boundary to reltative position scale
#' @param fix logical, attempt fxing overlapping positions with `fixOverlap`
#' @param ... additional arguments to `fixOverlap`
#'
#' @return `list` containing:
#'   * `grad`: values of `x` in ascending order
#'   * `order`: ordering vector of `x`
#'   * `relative.pos`: elements of `x` translated to the new relative scale defined by `x.min` and `x.max`
#'   
#' @export
#'
#' @examples
#' 
#' data("sierraTransect")
#' 
#' # split transects
#' g <- subset(sierraTransect, transect == 'Granite')
#' a <- subset(sierraTransect, transect == 'Andesite')
#' 
#' g.p <- alignTransect(g$elev, x.min = 1, x.max = length(g), fix = FALSE)
#' a.p <- alignTransect(a$elev, x.min = 1, x.max = length(a), fix = FALSE)
#' 
#' op <- par(mar=c(2,0,0,2), mfrow=c(2,1))
#' 
#' plotSPC(g, width=0.25, name.style='center-center', 
#'         cex.names=0.75, 
#'         relative.pos = g.p$relative.pos, plot.order = g.p$order)
#' 
#' axis(1, at = g.p$relative.pos, labels = g.p$grad, line = -1.5)
#' 
#' plotSPC(a, width=0.25, name.style='center-center', 
#'         cex.names=0.75, 
#'         relative.pos = a.p$relative.pos, plot.order = a.p$order)
#' 
#' axis(1, at = a.p$relative.pos, labels = a.p$grad, line = -1.5)
#' 
#' 
#' par(op)
#' 
alignTransect <- function(x, x.min, x.max, fix = TRUE, ...) {
  
  # sanity checks
  # no NA allowed in min/max
  if(is.na(x.min) | is.na(x.max)) {
    stop('NA are not allowed in `x.min` or `x.max`', call. = TRUE)
  }
  
  # TODO: it may be possible to "fill" NA in x
  if(any(is.na(x))) {
    warning('NA in `x` may lead to unexpected results', call. = FALSE)
  }
  
  # monotonic mapping from range(x) -> [x.min, x.max]
  o <- order(x)
  r <- .rescaleRange(x[o], x0 = x.min, x1 = x.max)
  
  # optionally fix overlap
  if(fix) {
    r <- fixOverlap(r, ...)  
  }
  
  res <- list(
    grad = x[o],
    order = o,
    relative.pos = r
  )
  
  return(res)
  
}

