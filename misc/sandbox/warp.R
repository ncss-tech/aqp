library(aqp)
library(soilDB)


# https://www.fao.org/3/cb0509en/CB0509EN.pdf


#' @title Inflate/Deflate Horizon Thickness
#'
#' @param x a `SoilProfileCollection` object
#' @param fact numeric or character; warping factor specified as a single numeric value, vector of numeric values (length = nrow(x)), or column name of a horizon level attribute containing numeric values
#' @param updateProfileID logical; modifiy profile IDs
#' @param suffix character; suffix added to profile IDs when `updateProfileID = TRUE`
#'
#' @return a modified version of `x`, `SoilProfileCollection` object
#' @export
#'
#'
warp <- function(x, fact, updateProfileID = TRUE, suffix = '-w') {
  
  ## TODO: vectorize over profiles, and make more efficient

  # parts
  .h <- horizons(x)
  .htb <- horizonDepths(x)
  .n <- nrow(.h)
  
  # sanity check: fact should be length:
  # * 1, used by all horizons
  # * nrow(x), each horizon has its own factor
  # * column name
  
  if(inherits(fact, 'character')) {
    fact <- x[[fact]]
    if(is.null(fact)) {
      stop('fact must name a column in horizons')
    }
  } else if(inherits(fact, 'numeric')) {
    
    if(length(fact) > 1 && length(fact) != .n) {
      stop('fact must be length 1 or nrow(x)')
    }
    
  } else {
    stop('fact must be either character vector or numeric')
  }
  
  # hz thickness
  .thick <- .h[[.htb[2]]] - .h[[.htb[1]]]
  
  # apply inflation/deflation factor to horizon thickness
  # round to integers
  .thick <- round(.thick * fact)
  
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
  if(updateProfileID) {
    .pID <- profile_id(x)
    profile_id(x) <- sprintf("%s%s", .pID, suffix)
  }
  
  return(x)
}


o <- fetchOSD('zook')
oo <- warp(o, fact = c(1.8, 1.3, 0.6, 0.75, 0.8, 1, 1, 1))
x <- combine(o, oo)

.y1 <- x[1, , .TOP]
.y2 <- x[2, , .TOP]

par(mar = c(1, 0, 0 , 2))
plotSPC(x, name.style = 'center-center', cex.names = 0.8, width = 0.2, max.depth = 200)
arrows(x0 = 1 + 0.2, y0 = .y1, x1 = 2 - 0.2, y1 = .y2, len = 0.1, col = 2)



o$fact <- c(1, 1, 1, 1, 1, 1, 1, 1)
oo$fact <- c(1.8, 1.3, 0.6, 0.75, 0.8, 1, 1, 1)
x <- combine(o, oo)

par(mar = c(1, 0, 3 , 2))
plotSPC(x, name.style = 'center-center', cex.names = 0.8, width = 0.2, max.depth = 200, depth.axis = FALSE, hz.depths = TRUE, color = 'fact')
arrows(x0 = 1 + 0.2, y0 = .y1, x1 = 2 - 0.2, y1 = .y2, len = 0.1, col = 'grey')



