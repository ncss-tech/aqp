
## TODO: add "direction" argument for selecting, top / bottom / both

#' @title Grow a `SoilProfileCollection` with Empty Horizons
#' 
#' @description This function appends a single "empty" horizon to each profile that is shallower than `z`. Horizon IDs are reset and the horizons are re-sorted according to profile ID and horizon top depth. This will fail if horizon top depths are `NA`. Adding empty horizons to profiles is one way to ensure that data (even NA) will be returned from each profile when sliced at depths exceeding some profiles in the collection.
#' 
#' @param x a `SoilProfilecollection` object
#' 
#' @param z new bottom depth, after growing with empty horizons
#' 
#' @return a `SoilProfilecollection` object
#' 
#' @author D.E. Beaudette
#' 
#' @examples 
#' 
#' data(sp4)
#' depths(sp4) <- id ~ top + bottom
#' 
#' par(mar = c(0, 0, 0, 0))
#' 
#' plotSPC(sp4)
#' 
#' g <- growEmptyHz(sp4, 50)
#' plotSPC(g, color = 'hzID', show.legend = FALSE)
#' plotSPC(g, color = 'Ca', show.legend = FALSE)
#' 
#' d <- dice(g, fm = 0:50 ~ .)
#' plotSPC(d, color = 'sliceID', show.legend = FALSE)
#' plotSPC(d, color = 'Ca', show.legend = FALSE)
#' 
growEmptyHz <- function(x, z) {
  
  # IDs, depths
  hzidn <- hzidname(x)
  idn <- idname(x)
  htb <- horizonDepths(x)
  
  # sanity check: x cannot include NA horizon top depths
  if(any(is.na(x[[htb[1]]]))){
    stop('NA horizon top depths are not allowed', call. = FALSE)
  }
  
  # z must be an integer > 0
  if(!inherits(z, 'numeric') | z < 1) {
    stop('z must be an integer and > 0', call. = FALSE)
  }
  
  
  # short-circuit: if all profiles are deeper than z, do nothing
  
  # old-way of profile max depth
  # there is a large cost to profileApply, best for complex split-apply-combine
  # max.d <- profileApply(x, max)
  
  # fix for no visible binding for global variables
  .LAST <- NULL
  .HZID <- NULL
  
  # get a vector of profile bottom depths
  # FAST c/o AGB and new .LAST and .HZID shortcuts
  max.d <- x[[htb[2]]][x[,,.LAST,.HZID]]
  
  if(all(max.d > z)) {
    message(sprintf('all profiles are deeper than %s, doing nothing', z))
    return(x)
  }
  

  # get horizons
  h <- horizons(x)
  
  ## this is effective and simple to understand, but does not scale well
  # # get bottom-most horizons
  # b <- profileApply(x, simplify = FALSE, frameify = TRUE, FUN = function(i) {
  #   horizons(i)[nrow(i), ]
  # })
  
  # get bottom-most horizons
  # ~ 50% faster than above
  # c/o AGB
  b <- horizons(x[, , .LAST])
  
  # just those profiles with bottom-most depth > z
  idx <- which(b[[htb[2]]] < z)
  if(length(idx) > 0) {
    message(sprintf('only some profiles shallower than %s', z))
    b <- b[idx, ]
  } else {
    stop('this should not happen')
  }
  
  # bottom becomes top
  b[[htb[[1]]]] <- b[[htb[[2]]]]
  
  # bottom becomes z
  b[[htb[[2]]]] <- z
  
  # set all hz vars (except IDs) to NA
  nm <- names(b)
  idx <- which(!nm %in% c(idn, hzidn, htb))
  vars <- nm[idx]
  
  for(i in vars){
    b[[i]] <- NA
  }
  
  # combine original horizons + fake horizons
  nh <- rbind(h, b)
  
  # reset hzIDs
  nh[[hzidn]] <- NA
  nh[[hzidn]] <- 1:nrow(nh)
  
  ## TODO: faster with data.table when using very large collections
  # re-order
  nh <- nh[order(nh[[idn]], nh[[htb[1]]]), ]
  
  # re-pack horizons
  replaceHorizons(x) <- nh
  
  return(x)
}
