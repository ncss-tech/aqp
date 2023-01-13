#
#' @title Get IDs of Deepest Horizons by Profile
#'
#' @description Return horizon IDs of the deepest horizon within each profile of a `SoilProfileCollection`. IDs are returned in the same order as `profile_id(x)`. Horizon top depths are used because there are cases where bottom depths may be missing.
#'
#' @param x a `SoilProfileCollection`
#' @export
getLastHorizonID <- function(x) {

  # with data.table
  hztb <- horizonDepths(x)
  hzidn <- hzidname(x)
  idn <- idname(x)

  .N <- NULL
  .I <- NULL

  # ensure site and horizons are "in sync"
  stopifnot(spc_in_sync(x)$valid)

  h <- data.table::as.data.table(horizons(x))

  # get row index of last horizon by group
  idx <- h[, .I[.N], by = list(h[[idn]])]$V1

  # get hzID for just last horizon
  res <- hzID(x)[idx]

  # add profile IDs as names
  names(res) <- profile_id(x)

  # a named vector, in the same order as profile_id(x)
  return(res)
}

