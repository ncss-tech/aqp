


#' @title Find and Fill Horizon Gaps
#' 
#' @description This function attempt to find "gaps" in the horizon records of a `SoilProfileCollection` object and fill with place holder horizons. 
#' 
#' Gaps are defined as: 
#'  * within each profile, for horizons `i` to `n_hz`:
#'  * `bottom_i != top_i+1 (but only to i = 1:(n_hz - 1)`
#'
#' @param x `SoilProfileCollection` object
#' @param flag logical, flag empty horizons that have been added
#'
#' @return `SoilProfileCollection` object
#' @export
#'
#' @examples
#' 
#' data(sp4)
#' depths(sp4) <- id ~ top + bottom
#' 
#' # introduce gaps
#' idx <- c(2, 8, 12)
#' sp4$top[idx] <- NA
#' 
#' # check
#' horizons(sp4)[idx, ]
#' 
#' # remove problematic horizons
#' x <- HzDepthLogicSubset(sp4, byhz = TRUE)
#' 
#' # gaps and now problematic profiles
#' par(mar = c(0, 0, 0, 1))
#' plotSPC(x, width = 0.3, default.color = 'royalblue')
#' 
#' z <- fillHzGaps(x, flag = TRUE)
#' 
#' # BUG: plotSPC can't use logical data for color
#' z$.filledGap <- as.factor(z$.filledGap)
#' plotSPC(z, width = 0.3, color = '.filledGap', show.legend = FALSE)
#' 
#' 
fillHzGaps <- function(x, flag = FALSE) {
  
  # assumes profile ID / top depth sorting !
  
  ## extract pieces
  idn <- idname(x)
  hzidn <- hzidname(x)
  htb <- horizonDepths(x)
  hznames <- horizonNames(x)
  
  # IDs + top/bottom
  ids.top.bottom.idx <- match(c(idn, hzidn, htb), hznames)
  
  # only using hz data
  h <- horizons(x)
  
  # find gaps
  # gap: 
  # within a profile
  # bottom_i != top_i+1 (but only to i = 1:(n_hz - 1)
  
  # TODO: short-circuit / optimization
  #  https://github.com/ncss-tech/aqp/issues/205
  #  use findGaps() to work on affected subset of profiles ONLY
  #  likely a data.table approach
  
  
  # TODO: faster version with data.table
  
  # slow / simple version with split
  hs <- split(h, h[[idn]])
  
  h.filled <- lapply(hs, function(i) {
    # number of horizons
    n <- nrow(i)
    
    # comparison index
    s <- 1:(n-1)
    
    # local copies for simpler expressions
    .top <- i[[htb[1]]]
    .bottom <- i[[htb[2]]]
    
    # find gaps
    # affected horizons are c(idx, idx + 1)
    idx <- which(.bottom[s] != .top[s + 1])
    
    # only if there are gaps
    if(length(idx) > 0) {
      gap.top <- .bottom[idx]
      gap.bottom <- .top[idx + 1]
      
      # copy 1st horizon for gap-filling empty hz
      hz.template <- i[1, ids.top.bottom.idx]
      # edit depths
      hz.template[[htb[1]]] <- gap.top
      hz.template[[htb[2]]] <- gap.bottom
      
      # zap horizon ID
      hz.template[[hzidn]] <- NA
      
      # combine with original horizons, padding NA
      res <- rbindlist(list(i, hz.template), fill = TRUE)
      
      # TODO: back to original class
      # via aqp_df_class(x)
      res <- as.data.frame(res)
      
      return(res)
    } else {
      # do nothing
      return(i)
    }
    
  })
  
  # back to DF
  h.filled <- do.call('rbind', h.filled)
  
  # re-sort
  o <- order(h.filled[[idn]], h.filled[[htb[1]]])
  h.filled <- h.filled[o, ]
  
  # fill missing hzID
  idx <- which(is.na(h.filled[[hzidn]]))
  if(length(idx) > 0) {
    # new sequence for affected hz
    m <- max(as.numeric(h[[hzidn]]), na.rm = TRUE)
    s <- seq(
      from = m + 1,
      to = m + length(idx),
      by = 1
    )
    
    # optionally flag filled gaps
    if(flag) {
      h.filled[['.filledGap']] <- FALSE
      h.filled[['.filledGap']][idx] <- TRUE
    }
    
    # insert new horizon IDs  
    h.filled[[hzidn]][idx] <- as.character(s)
  }
  
  # re-pack horizons
  replaceHorizons(x) <- h.filled
  
  return(x)
}

