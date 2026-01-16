


#' @title Shuffle a single soil profile (internal function)
#'
#' @param i `SoilProfileCollection` of length 1
#' @param mode character, shuffling mode
#' @param replace logical, replacement argument to `base::sample()`
#' @param size integer, size argument to `base::sample()`
#'
#' @returns `SoilProfileCollection`, modified from original
#' @noRd
#' 
.shuffleProfile <- function(i, mode, replace, size) {
  
  # horizon metadata
  .nm <- horizonNames(i)
  .hzd <- horizonDepths(i)
  .id <- idname(i)
  
  # this function only needs access to horizon-level data
  .h <- horizons(i)
  
  # original horizon depths / thickness preserved
  # associated data are shuffled
  if(mode == 'data') {
    # static horizon data
    .hs <- .h[, c(.id, .hzd)]
    
    # dynamic horizon data -> these will be shuffled
    .hd <- .h[, which(! .nm %in% c(.hzd, .id))]
    
    # shuffle 
    # ignore: replace and size arguments
    .idx <- sample(1:nrow(.hd))
    .hd <- .hd[.idx, ]
    
    # combine static + shuffled
    .hnew <- cbind(.hs, .hd)
    
    # replace
    replaceHorizons(i) <- .hnew
  }
  
  # physical horizons (thickness + data) are shuffled
  if(mode == 'horizon') {
    
    # interpret size argument to sample
    if(is.null(size)) {
      size <- nrow(.h)
    }
    
    # shuffle
    # replacement and size arguments subject to constraints on base::sample()
    .idx <- sample(1:nrow(.h), replace = replace, size = size)
    .hnew <- .h[.idx, ]
    
    # initial conditions:
    # thickness
    .thick <- .hnew[[.hzd[2]]] - .hnew[[.hzd[1]]]
    # top-most depth from original data
    .topmost <- min(.h[[.hzd[1]]])
    
    # compute new depths
    .bottom <- cumsum(.thick)
    .top <- c(.topmost, .bottom[-length(.bottom)])
    
    # replace depths
    .hnew[[.hzd[1]]] <- .top
    .hnew[[.hzd[2]]] <- .bottom
    
    replaceHorizons(i) <- .hnew
  }
  
  return(i)
}



#' @title Shuffle Horizons of a SoilProfileCollection
#' 
#' @description
#' This function shuffles the horizon data or physical ordering of horizons within profiles of a `SoilProfileCollection` object.
#' 
#' @param x `SoilProfileCollection`
#' @param mode character, one of 'data' or 'horizon'
#' 
#'  * 'data': shuffle the _data_ associated with physical horizons, making no change to the original horizon thickness and horizon IDs
#'  
#'  * 'horizon': shuffle physical horizons, horizon top and bottom depths are re-calculated
#' 
#' @param replace logical, replacement argument to `base::sample()`, only used when `mode = 'horizon'`
#' 
#' @param size integer, size argument to `base::sample()`, only used when `mode = 'horizon'`
#'
#' @returns `SoilProfileCollection`
#' 
#' @export
#'
#' @examples
#' 
#' data('osd', package = 'aqp')
#' o <- osd
#' 
#' # shuffling of data only
#' o.d <- shuffle(o, mode = 'data')
#' 
#' # shuffling of horizons
#' o.h <- shuffle(o, mode = 'horizon')
#' 
#' # shuffling / sampling with replacement
#' o.h2 <- shuffle(o, mode = 'horizon', replace = TRUE)
#' 
#' # add method to IDs
#' profile_id(o.d) <- sprintf("%s\ndata", profile_id(o.d))
#' profile_id(o.h) <- sprintf("%s\nhz", profile_id(o.h))
#' profile_id(o.h2) <- sprintf("%s\nhz R", profile_id(o.h2))
#' 
#' # combine into single SPC
#' g <- combine(o, o.d, o.h, o.h2)
#' 
#' # graphical comparison
#' op <- par(mar = c(0, 0, 0.5, 2.5))
#' plotSPC(g, name.style = 'center-center', cex.names = 0.66, width = 0.3, cex.id = 0.75)
#' 
#' par(op)
#' 
shuffle <- function(x, mode = c('data', 'horizon'), replace = FALSE, size = NULL) {
  
  # check arguments
  mode <- match.arg(mode)
  
  # TODO: implement via data.table
  
  # apply to all profiles
  .res <- profileApply(x, .shuffleProfile, mode = mode, replace = replace, size = size)
  
  # list -> SPC
  .res <- combine(.res)
  
  return(.res)
}


