
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
  
  # crude prototype, single profile at a time
  .fo <- function(i, hzd) {
    
    # tops / bottoms
    # NA not currently handled
    .tops <- i[[hzd[1]]]
    .bottoms <- i[[hzd[2]]]
    
    # find perfect overlap
    .rt <- rle(.tops)
    .rb <- rle(.bottoms)
    
    # id affected horizons
    .ot <- .rt$values[which(.rt$lengths > 1)]
    .ob <- .rb$values[which(.rb$lengths > 1)]
    
    ## TODO: tests required
    # index affected horizons
    .m <- outer(.ot, .tops, '==')
    idx <- unlist(as.vector(apply(.m, 1, which)))
    
    # generate flag vector along sequence of horizons 
    .res <- rep(FALSE, times = length(.tops))
    .res[idx] <- TRUE
    
    return(.res)
  }
  
  h <- data.table::data.table(horizons(x))
  l <- list(h[[idname(x)]])
  names(l) <- idname(x)
  h[, .fo(.SD, horizonDepths(x)), by = l]$V1
}

