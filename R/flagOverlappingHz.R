
#' @title Flag perfectly overlapping horizons within a SoilProfileCollection
#'
#' @param x a `SoilProfileCollection` object
#'
#' @return logical vector with length (and order) matching the horizons of `x` 
#' 
#' @author D.E. Beaudette
#' 
#' @export
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
  
  # TODO: horizons with NA depths are not flagged as overlapping
  # TODO: bottom depths are never used or compared
  
  .tops <- h[[hzd[1]]]
  # .bottoms <- h[[hzd[2]]]
  
  .rt <- rle(.tops) # NOTE: rle gives length 1 for each NA
  # .rb <- rle(.bottoms)
  
  .ot <- .rt$values[which(.rt$lengths > 1)]
  # .ob <- .rb$values[which(.rt$lengths > 1)]
  
  # index affected horizons
  # TODO: handle NA in logical comparisons
  .m1 <- outer(.ot, .tops, '==') 
  # .m2 <- outer(.ob, .bottoms, '==') 
  idx1 <- unlist(as.vector(apply(.m1, 1, which)))
  # idx2 <- unlist(as.vector(apply(.m2, 1, which)))
  
  # generate flag vector along sequence of horizons 
  .res <- rep(FALSE, times = length(.tops))
  # .res[intersect(idx1, idx2)] <- TRUE
  .res[idx1] <- TRUE
  .res
}

