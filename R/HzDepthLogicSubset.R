## fairly generic, will be required by (at least):"
# * dice() [x]
# * profile_compare()
# * slab()
# * spc2mpspline()

## TODO do we need all arguments to checkHzDepthLogic()?
# if using `...` then we need to explicitly "grab" byhz for later

#'
#' @title Subset `SoilProfileCollection` Objects or Horizons via `checkHzDepthLogic`
#' 
#' @param x a `SoilProfileCollection` object
#' @param byhz logical, evaluate horizon depth logic at the horizon level (profile level if `FALSE`)
#' 
#' @return a `SoilProfileCollection` object
#' 
#' @export
#' 
HzDepthLogicSubset <- function(x, byhz = FALSE) {
  
  # additional arguments?
  hz.tests <- checkHzDepthLogic(x, fast = TRUE, byhz = byhz)
  
  # short-circuit: no invalid records, stop here
  if(all(hz.tests$valid)) {
    return(x)
  }
  
  # invalid data filtering modes:
  if(byhz) {
    # profile-level
    message("dropping horizons with invalid depth logic, see `metadata(x)$dice.removed.horizons`")
    
    # locate horizons to keep
    idx <- which(hz.tests$valid)
    
    # test for empty SPC
    if(length(idx) < 1) {
      stop('there are no valid profiles in this collection', call. = FALSE)
    }
    
    # keep track of invalid horizon IDs in @metadata
    bad.ids <- hz.tests[[hzidname(x)]][-idx]
    metadata(x)$dice.removed.horizons <- bad.ids
    
    # perform drop
    # this will trigger an error if SPC is corrupted (site w/o horizons)
    res <- try(
      replaceHorizons(x) <- horizons(x)[idx, ], 
      silent = TRUE
    )
    
    if(inherits(res, 'try-error')) {
      stop('removing horizons with invalid depth logic would corrupt `x`, use `byhz = FALSE`', call. = FALSE)
    }
    
  } else {
    # profile-level
    message("dropping profiles with invalid depth logic, see `metadata(x)$dice.removed.profiles`")
    
    # locate profiles to keep
    idx <- which(hz.tests$valid)
    
    # test for empty SPC
    if(length(idx) < 1) {
      stop('there are no valid profiles in this collection', call. = FALSE)
    }
    
    # keep track of invalid profile IDs in @metadata
    bad.ids <- hz.tests[[idname(x)]][-idx]
    metadata(x)$dice.removed.profiles <- bad.ids
    
    # perform drop
    x <- x[idx, ]
  }
  
  return(x)
  
}
