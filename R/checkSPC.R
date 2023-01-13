
# test for valid SPC, based on presence / absense of slots as compared to
# class prototype
# likely only used between major versions of aqp where internal structure of SPC has changed
#' Test for a valid SoilProfileCollection
#'
#' Test for a valid SoilProfileCollection
#'
#' Test for valid \code{SoilProfileCollection} by checking for slots defined in
#' the class prototype. Likely only used between major versions of `aqp` where
#' internal structure of \code{SoilProfileCollection} has changed. Use
#' \code{checkHzDepthLogic} to check for common errors in horizon depths.
#'
#' @param x a \code{SoilProfileCollection} object
#' @return TRUE or FALSE. Consider using \code{rebuildSPC()} if FALSE.
#' @author D.E. Beaudette
#' @seealso \code{\link{rebuildSPC}}, \code{\link{checkHzDepthLogic}}
#' @export
checkSPC <- function(x) {

  # get slot names from prototype
  sn <- slotNames(x)

  # test for all slots in the prototype
  s.test <- sapply(sn, function(i) .hasSlot(x, name=i))

  res <- FALSE
  # a valid object will have all slots present
  if(all(s.test)) {
    res <- TRUE
  }

  return(res)
}


