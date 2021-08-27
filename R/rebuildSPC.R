# repair an SPC by breaking into pieces and re-assembling
# likely only used to fix outdated SPC objects that are missing slots

#' Rebuild a SoilProfileCollection object
#'
#' Rebuild a SoilProfileCollection object
#'
#' Attempt rebuilding a \code{SoilProfileCollection} object by splitting into
#' components and re-assembling. Likely only used to fix outdated
#' \code{SoilProfileCollection} objects that are missing slots.
#'
#' @param x a \code{SoilProfileCollection} object
#' @return A valid \code{SoilProfileCollection} object.
#' @author D.E. Beaudette
#' @seealso \code{\link{checkSPC}}
#' Rebuild a SoilProfileCollection object
#'
#' Rebuild a SoilProfileCollection object
#'
#' Attempt rebuilding a \code{SoilProfileCollection} object by splitting into
#' components and re-assembling. Likely only used to fix outdated
#' \code{SoilProfileCollection} objects that are missing slots.
#'
#' @param x a \code{SoilProfileCollection} object
#' @return A valid \code{SoilProfileCollection} object.
#' @author D.E. Beaudette, A.G. Brown
#' @seealso \code{\link{checkSPC}}
rebuildSPC <- function(x) {

  # break into pieces as list
  x.list <- suppressWarnings(as(x, 'list'))

  # seed object for new SPC
  res <- .as.data.frame.aqp(x.list$horizons, aqp_df_class(x))

  # init SPC from pieces
  # note: using depths<- because it will generate a horizon ID
  fm <- as.formula(sprintf("%s ~ %s + %s", x.list$idcol, x.list$depthcols[1], x.list$depthcols[2]))
  depths(res) <- fm

  # preserve original hzidname
  # this is missing in old SPC objects
  if(!is.null(x.list$hzidcol)) {
    hzidname(res) <- x.list$hzidcol
  }

  if(is.null(x.list$hzdesgncol)) {
    x.list$hzdesgncol <- ""
  }

  # preserve original hzdesgnname
  # this is missing in old SPC objects
  if(!is.null(x.list$hzdesgncol)) {
    hzdesgnname(res) <- x.list$hzdesgncol
  }

  if(is.null(x.list$hztexclcol)) {
    x.list$hztexclcol <- ""
  }

  # preserve original hztexclname
  # this is missing in old SPC objects
  if(!is.null(x.list$hztexclcol)) {
    hztexclname(res) <- x.list$hztexclcol
  }

  # transfer metadata
  res <- .transfer.metadata.aqp(x, res)
    
  # replace site
  site(res) <- x.list$site

  # @diagnostic and may be missing if `x` is very old
  # replace with empty data.frame so that setters do not error
  if(is.null(x.list$diagnostic)) {
    x.list$diagnostic <- data.frame()
  }

  # @restructions may be missing if `x` is very old
  # replace with empty data.frame so that setters do not error
  if(is.null(x.list$restrictions)) {
    x.list$restrictions <- data.frame()
  }

  # set
  diagnostic_hz(res) <- x.list$diagnostic
  restrictions(res) <- x.list$restrictions

  # copy valid spatial data from the source object
  # otherwise, the previous and possibly invalid SpatialPoints object created by horizons() is fine
  if(validSpatialData(x)) {
    res@sp <- x.list$sp
  }

  # done
  return(res)
}

