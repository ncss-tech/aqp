
#' @title Flag perfectly overlapping horizons within a SoilProfileCollection
#'
#' @param x a `SoilProfileCollection` object
#'
#' @return logical vector with length (and order) matching the horizons of `x` 
#' 
#' @author D.E. Beaudette, A.G. Brown
#' 
#' @export
#' @details
#' Horizons with `NA` depths can be flagged as overlapping. Consider finding these horizons with `checkHzDepthLogic(byhz=TRUE)` and removing or fixing them.
#' 
#' @seealso [checkHzDepthLogic()] [fillHzGaps()]
#'
#' @examples
#' 
#' # two overlapping horizons
#' z <- data.frame(
#'   id = 'SPC',
#'   top = c(0, 25, 25, 50, 75, 100, 100),
#'   bottom = c(25, 50, 50, 75, 100, 125, 125)
#' )
#' 
#' # init SPC
#' depths(z) <- id ~ top + bottom
#' 
#' # flag perfectly overlapping horizons
#' z$.overlapFlag <- flagOverlappingHz(z)
#' 
#' # thematic sketches
#' plotSPC(z, color = '.overlapFlag', hz.depths = TRUE, 
#' depth.axis = FALSE, cex.names = 0.85)
#' 
flagOverlappingHz <- function(x) {
  h <- horizons(x)
  hzd <- horizonDepths(x)
  
  # extract horizon depths
  .tops <- h[[hzd[1]]]
  .bottoms <- h[[hzd[2]]]
  
  # recode missing depths so they will be recognized as runs with length >1
  .topna <- is.na(.tops)
  .botna <- is.na(.bottoms)
  .tops[.topna] <- -9999
  .botna[.botna] <- -9999
  
  .rt <- rle(.tops) 
  .rb <- rle(.bottoms)
  
  .ot <- .rt$values[which(.rt$lengths > 1)]
  .ob <- .rb$values[which(.rb$lengths > 1)]
  
  # index affected horizons
  .m1 <- outer(.ot, .tops, '==') 
  .m2 <- outer(.ob, .bottoms, '==')
  idx1 <- unlist(as.vector(apply(.m1, 1, which)))
  idx2 <- unlist(as.vector(apply(.m2, 1, which)))
  
  # generate flag vector along sequence of horizons 
  .res <- rep(FALSE, times = length(.tops))
  .res[intersect(idx1, idx2)] <- TRUE
  .res
}

