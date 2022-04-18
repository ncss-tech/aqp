
#' Select a subset of columns from a SoilProfileCollection
#'
#' Reduce the number of columns in a SoilProfileCollection to a minimal set plus additional selected columns. Optional metadata columns are included if set. At a minimum the profileID, horizon top and bottom depth, horizon ID are included. Horizon designation and horizon texture class column names are included if metadata attributes are set. See details.
#' 
#' @param p a SoilProfileCollection
#' @param column_names a set of additional columns to include in the result
#' @details 
#' 
#' Minimum column names included (when `column_names = NULL`)
#'  - `idname(p)`, `horizonDepths(p)`, `hzidname(p)`
#'  
#' Optional column names included (when metadata are set)
#'  - `hzdesgnname(p)`, `hztexclname(p)`
#'  
#' @seealso `hzdesgnname()` `hztexclname()`
#' @return a SoilProfileCollection
#' @export
reduceSPC <- function(p, column_names = NULL) {
  
  minnames <- c(idname(p), horizonDepths(p), hzidname(p))
  extnames <- c(hzdesgnname(p), hztexclname(p))
  ecn <- column_names[!column_names %in% c(minnames, extnames)]
  sn <- siteNames(p)
  hn <- horizonNames(p)
  
  stn <- sn[sn %in% ecn]
  hzn <- c(minnames, extnames[extnames != ""], hn[hn %in% ecn])
  msn <- ecn[!(ecn %in% stn | ecn %in% hzn)]
  
  if (length(msn) > 0) {
    stop(sprintf("column names (%s) not found in SoilProfileCollection",
                 paste0(msn, collapse = ", ")), call. = FALSE)
  }
  
  replaceHorizons(p) <- .data.frame.j(horizons(p), hzn)
  p@site <- .data.frame.j(site(p), c(idname(p), stn))
  p
}
