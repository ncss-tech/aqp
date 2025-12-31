

# https://www.fao.org/3/cb0509en/CB0509EN.pdf


#' @title Inflate / Deflate Horizon Thickness
#' 
#' @description This function applies a warping factor to the horizons of a single-profile `SoilProfileCollection` object. Warping values >1 will inflate horizon thickness, values <1 will deflate horizon thickness.
#'
#' @param x a `SoilProfileCollection` object with a single soil profile
#' 
#' @param fact numeric or character; warping factor specified as a single numeric value, vector of numeric values (length = `nrow(x)`), or column name of a horizon level attribute containing numeric values
#' 
#' @param scaleTo numeric, target depth resulting from rescaling all horizon thickness values, due to rounding the actual depth may be within +/- 1 depth unit
#' 
#' @param soilDepthFun function for computing soil depth, either: 
#' 
#'   * `estimateSoilDepth()`: "soil depth" is defined as depth to contact, requires that `hzdesgnname()` be set, see [estimateSoilDepth()] for details
#'   * `max()`: "soil depth" is defined as the bottom depth of the soil profile
#' 
#' @param updateProfileID logical; modify profile IDs
#' 
#' @param suffix character; suffix added to profile IDs when `updateProfileID = TRUE`
#' 
#' @author D.E. Beaudette and S.W. Salley
#'
#' @return a modified version of `x`, `SoilProfileCollection` object
#' @export
#'
#' @examples 
#' 
#' # create an example profile
#' s <- quickSPC('p1:AA|Bt1Bt1Bt1|Bt2Bt2B|Bt3|Cr|RRRRR')
#' 
#' # warp each horizon
#' # values > 1: inflation
#' # values < 1: deflation (erosion / compaction)
#' s.w <- warpHorizons(s, fact = c(1.3, 0.7, 0.8, 1, 1, 1))
#' 
#' # combine original + warped
#' x <- combine(s, s.w)
#' 
#' # compute profile bottom depths
#' .bottoms <- x[, , .LAST, .BOTTOM]
#' 
#' # change in total depth after warping
#' # used to vertically offset the warped profile
#' .yoff <- c(0, .bottoms[1] - .bottoms[2])
#' 
#' # depths for line segments connecting horizon tops
#' .y1 <- x[1, , .TOP]
#' .y2 <- x[2, , .TOP] + .yoff[2]
#' 
#' # sketches
#' # can't automatically add a depth axis
#' par(mar = c(0.5, 0, 0, 2))
#' plotSPC(
#'   x, 
#'   name.style = 'center-center', 
#'   cex.names = 0.8, 
#'   width = 0.2, 
#'   max.depth = 150, 
#'   depth.axis = FALSE, 
#'   y.offset = .yoff
#' )
#' 
#' # illustrate warping with arrows
#' arrows(x0 = 1 + 0.25, y0 = .y1, x1 = 2 - 0.25, y1 = .y2, len = 0.1, col = 2)
#' 
#' # manually add depth axis
#' axis(side = 4, line = -3.5, las = 1, at = seq(from = 0, to = 150, by = 25))
#' 
#' 
#' # apply to multiple profiles
#' # text-based template
#' .template <- c(
#' 'P1:AAA|BwBwBwBw|CCCCCCC|CdCdCdCd',
#' 'P2:ApAp|AA|E|BhsBhs|Bw1Bw1|CCCCC',
#' 'P3:A|Bt1Bt1Bt1|Bt2Bt2Bt2|Bt3|Cr|RRRRR'
#' )
#' 
#' # each horizon label is '10' depth-units (default)
#' s <- quickSPC(.template)
#' 
#' # random warping factor, by horizon
#' s$f <- runif(n = nrow(s), min = 0.8, max = 1.2)
#' 
#' # warp horizons by profile, result is a list of SPCs
#' s.w <- profileApply(s, FUN = warpHorizons, fact = 'f')
#'
#' # flatten list -> SoilProfileCollection
#' s.w <- combine(s.w)
#' 
#' # combine with original SPC
#' x <- combine(s, s.w)
#' 
#' # sketches 
#' par(mar = c(0.5, 0, 0, 2.5))
#' plotSPC(
#'   x,
#'   name.style = 'center-center',
#'   cex.names = 0.8,
#'   width = 0.3,
#'   max.depth = 165,
#'   depth.axis = list(line = -2)
#' )
#' 
#' # rescale all profiles to 100cm soil depth (depth to contact)
#' .template <- c(
#'   'P1:AAA|BwBwBwBw|CCCCCCC|CdCdCdCd',
#'   'P2:ApAp|AA|E|BhsBhs|Bw1Bw1|CCCCC',
#'   'P3:A|Bt1Bt1Bt1|Bt2Bt2Bt2|Bt3|Cr|RRRRR',
#'   'P4:AAAAA|CCC|RRRRRR'
#' )
#' 
#' # each horizon label is '10' depth-units (default)
#' s <- quickSPC(.template)
#' 
#' # warp horizons by profile, result is a list of SPCs
#' w <- profileApply(s, FUN = function(i) {
#'   warpHorizons(i, scaleTo = 100, soilDepthFun = estimateSoilDepth)
#' })
#' 
#' # flatten list -> SoilProfileCollection
#' w <- combine(w)
#' 
#' # combine with original SPC
#' x <- combine(s, w)
#' 
#' # highlight "contact"
#' x$color <- rep(grey(0.9), times = nrow(x))
#' x$color[grep('R|Cr|Cd', x$name)] <- 'royalblue'
#' 
#' # sketches
#' par(mar = c(0.5, 0, 0, 2.5))
#' plotSPC(
#'   x,
#'   color = 'color',
#'   name.style = 'center-center',
#'   cex.names = 0.8,
#'   width = 0.3,
#'   max.depth = 165,
#'   depth.axis = list(line = -2)
#' )
#' 
#' abline(h = 100, lty = 3)
#'
warpHorizons <- function(x, fact = NULL, scaleTo = NULL, soilDepthFun = estimateSoilDepth, updateProfileID = TRUE, suffix = '-w') {
  
  ## TODO: 
  ## * vectorize over profiles, and make more efficient
  
  # extract parts and pieces of the SPC
  .h <- horizons(x)
  .htb <- horizonDepths(x)
  .n <- nrow(.h)
  
  # sanity check: fact should be:
  # * NULL, not specified in favor of scaleTo
  # * numeric, length == 1, used by all horizons
  # * numeric, length == nrow(x), each horizon has its own factor
  # * hz column name
  
  if(!is.null(fact)) {
    if(inherits(fact, 'character')) {
      fact <- x[[fact]]
      if(is.null(fact)) {
        stop('fact must name a column in horizons', call. = FALSE)
      }
    } else if(inherits(fact, 'numeric')) {
      
      if(length(fact) > 1 && length(fact) != .n) {
        stop('fact must be length 1 or nrow(x)', call. = FALSE)
      }
      
    } else {
      stop('fact must be either character vector or numeric', call. = FALSE)
    }
  } else if(!is.null(scaleTo)) {
    
    if(length(scaleTo) > 1) {
      stop('scaleTo should have length of 1')
    }
    
    # compute fact from scaleTo and soil depth
    # this requires
    fact <- (1 / soilDepthFun(x)) * scaleTo
    
  } else {
    # identity
    message('no scaling factor provided, using `fact = 1`')
    fact <- 1
  }
  
  # warping is applied to horizon thickness
  .thick <- .h[[.htb[2]]] - .h[[.htb[1]]]
  
  # apply inflation/deflation factor to horizon thickness
  # round to integers
  .thick <- round(.thick * fact)
  
  # future: apply offset here
  
  # generate new horizon depth sequence
  # starting from original topmost depth
  .start <- .h[1,  .htb[1]]
  
  # tops / bottoms
  .tops <- c(.start, cumsum(.thick[-.n]))
  .bottoms <- c(cumsum(.thick))
  
  # replace original values
  .h[[.htb[1]]] <- .tops
  .h[[.htb[2]]] <- .bottoms
  
  # re-pack horizons
  replaceHorizons(x) <- .h
  
  # optionally update profile ID
  # this is handy when trying to combine(old, new)
  if(updateProfileID) {
    .pID <- profile_id(x)
    profile_id(x) <- sprintf("%s%s", .pID, suffix)
  }
  
  return(x)
}


