
## TODO: add "direction" argument for selecting, top / bottom / both

#' @title Grow a `SoilProfileCollection` with Empty Horizons
#' 
#' @description This function appends a single "empty" horizon to each profile that is shallower than (`direction = 'down'`) or starts below (`direction = 'up'`) `z`. This will fail if horizon top depths are `NA`. Adding empty horizons to profiles is one way to ensure that data (even NA) will be returned from each profile when sliced at depths above / below some profiles in the collection.
#' 
#' @param x a `SoilProfilecollection` object
#' 
#' @param z new top or bottom anchor (see `direction`), after growing with empty horizons
#' 
#' @param direction empty horizons are "grown" *down* to `z` or *up* to `z`
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
#' # remove 1st horizons from profiles 1:4
#' idx <- sp4[,, .FIRST, .HZID]
#' replaceHorizons(sp4) <- horizons(sp4)[-idx[1:4], ]
#' 
#' 
#' par(mar = c(0, 0, 0, 0))
#' plotSPC(sp4)
#' 
#' # grow-down
#' g <- growEmptyHz(sp4, z = 50)
#' plotSPC(g, color = 'Ca', show.legend = FALSE)
#' 
#' # grow-up
#' g2 <- growEmptyHz(g, z = 0, direction = 'up')
#' plotSPC(g2, color = 'Ca', show.legend = FALSE)
#' 
#' # demonstrate with dicing/slicing
#' d <- dice(g2, fm = 0:50 ~ .)
#' plotSPC(d, color = 'Ca', show.legend = FALSE)
#' 
growEmptyHz <- function(x, z, direction = c('down', 'up')) {
  
  # fix for no visible binding for global variables
  .FIRST <- NULL
  .LAST <- NULL
  .HZID <- NULL
  
  # IDs, depths
  hzidn <- hzidname(x)
  idn <- idname(x)
  htb <- horizonDepths(x)
  hznames <- horizonNames(x)
  
  # IDs + top/bottom
  ids.top.bottom.idx <- match(c(idn, hzidn, htb), hznames)
  
  # sanity check: x cannot include NA horizon top depths
  if(any(is.na(x[[htb[1]]]))){
    stop('NA horizon top depths are not allowed', call. = FALSE)
  }
  
  # valid direction argument
  direction <- match.arg(direction)
  
  # z must be an integer >= 0
  if(!inherits(z, 'numeric') | z < 0) {
    stop('z must be an integer and >= 0', call. = FALSE)
  }
  
  # get horizons
  h <- horizons(x)
  
  ## TODO: abstract to a function so that we can do direction = "both"
  
  # create empty horizons according to direction
  if(direction == 'down') {
    
    ## short-circuit: if all profiles are deeper than z, do nothing
    
    # get a vector of profile bottom depths
    # .LAST -> return top-most horizon
    # .HZID -> return row-index to these horizons
    max.d <- x[[htb[2]]][x[,,.LAST,.HZID]]
    
    if(all(max.d > z)) {
      message(sprintf('all profiles are deeper than %s, doing nothing', z))
      return(x)
    }
    
    # get bottom-most horizons
    # ~ 50% faster than profileApply c/o AGB
    b <- horizons(x[, , .LAST])
    
    ## TODO: make data.table safe with .data.frame.j()
    # keep only relevant columns: profile ID, horizon ID, top, bottom
    b <- b[, ids.top.bottom.idx]
    
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
    
    # zap empty horizon IDs
    b[[hzidn]] <- NA
    
  } else {
    # direction = 'up'
    
    ## short-circuit: if all profiles are shallower than z, do nothing
    
    # get a vector of profile bottom depths
    # FAST c/o AGB and new .LAST and .HZID shortcuts
    # .FIRST -> return top-most horizon
    # .HZID -> return row-index to these horizons
    min.d <- x[[htb[1]]][x[,, .FIRST, .HZID]]
    
    if(all(min.d <= z)) {
      message(sprintf('all profiles are shallower than %s, doing nothing', z))
      return(x)
    }
    
    # get top-most horizons
    # ~ 50% faster than profileApply c/o AGB
    b <- horizons(x[, , .FIRST])
    
    ## TODO: make data.table safe with .data.frame.j()
    # keep only relevant columns: profile ID, horizon ID, top, bottom
    b <- b[, ids.top.bottom.idx]
   
    # just those profiles with top-most depth > z
    idx <- which(b[[htb[1]]] > z)
    if(length(idx) > 0) {
      message(sprintf('only some profiles start below than %s', z))
      b <- b[idx, ]
    } else {
      stop('this should not happen')
    }
    
    # move top <- bottom
    b[[htb[[2]]]] <- b[[htb[[1]]]]
    
    # top becomes z
    b[[htb[[1]]]] <- z
    
    # zap empty horizon IDs
    b[[hzidn]] <- NA
     
  }
  
  # combine original horizons + empty horizons, padding missing columns with NA
  nh <- rbindlist(list(h, b), fill = TRUE)
  
  # TODO: back to original class
  # via aqp_df_class(x)
  nh <- as.data.frame(nh)
  
  # reset hzIDs of empty horizons
  idx <- which(is.na(nh[[hzidn]]))
  if(length(idx) > 0) {
    
    # new sequence for affected hz
    m <- max(as.numeric(nh[[hzidn]]), na.rm = TRUE)
    s <- seq(
      from = m + 1,
      to = m + length(idx),
      by = 1
    )
    
    # insert new horizon IDs  
    nh[[hzidn]][idx] <- as.character(s)
  }
  
  
  ## TODO: faster with data.table when using very large collections
  # re-order
  nh <- nh[order(nh[[idn]], nh[[htb[1]]]), ]
  
  # re-pack horizons
  replaceHorizons(x) <- nh
  
  return(x)
}
