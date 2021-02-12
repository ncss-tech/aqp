

# TODO: there may be something implemented already, will check
#       for now, the following is general / fast enough
# 
# 
#' @title Get IDs of Deepest Horizons by Profile
#' 
#' @description Return horizon IDs of the deepest horizon within each profile of a `SoilProfileCollection`. IDs are returned in the same order as `profile_id(x)`. Horizon top depths are used because there are cases where bottom depths may be missing.
#' 
#' @param x a `SoilProfileCollection`
#' 
getLastHorizonID <- function(x) {
  
  hztb <- horizonDepths(x)
  hzidn <- hzidname(x)
  idn <- idname(x)
  
  # basic idea: iterate over profiles, but only within horizon data
  # return deepest horizon IDs
  h <- horizons(x)
  h <- split(h, h[[idn]])
  
  # this is the safe set of horizons which can be repaired
  res <- sapply(h, function(i) {
    bottom.idx <- which.max(i[[hztb[1]]])
    res <- i[[hzidn]][bottom.idx]
    return(res)
  })
  
  ## TODO: this should never happen, including until tests are complete
  # just in case, ensure that profile order has not been compromised
  if(! all(profile_id(x) == names(res))) {
    stop('results out of order, how did this happen?', call. = FALSE)
  }
  
  # a named vector, in the same order as profile_id(x)
  return(res)
}

