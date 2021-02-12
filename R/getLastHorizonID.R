# 
#' @title Get IDs of Deepest Horizons by Profile
#' 
#' @description Return horizon IDs of the deepest horizon within each profile of a `SoilProfileCollection`. IDs are returned in the same order as `profile_id(x)`. Horizon top depths are used because there are cases where bottom depths may be missing.
#' 
#' @param x a `SoilProfileCollection`
#' 
getLastHorizonID <- function(x) {
  
  # satisfy R CMD check
  .SD <- NULL
  .N <- NULL
  
  hztb <- horizonDepths(x)
  hzidn <- hzidname(x)
  idn <- idname(x)
  
  # iterate over profile horizon subset data.frame; return deepest horizon IDs
  # data.table upgrade c/o AGB
  h <- data.table::as.data.table(horizons(x))
  
  h.sub <- h[, .SD[.N,], by = list(h[[idn]])]
  res <- h.sub[[hzidn]]
  
  names(res) <- h.sub[[idn]]
  
  # a named vector, in the same order as profile_id(x)
  return(res)
}

