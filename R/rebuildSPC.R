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
  if (!is.null(x.list$hzidcol)) {
    hzidname(res) <- x.list$hzidcol
  }
  
  if (is.null(x.list$hzdesgncol)) {
    x.list$hzdesgncol <- ""
  }
  
  # preserve original hzdesgnname
  # this is missing in old SPC objects
  if (!is.null(x.list$hzdesgncol)) {
    hzdesgnname(res) <- x.list$hzdesgncol
  }
  
  if (is.null(x.list$hztexclcol)) {
    x.list$hztexclcol <- ""
  }
  
  # preserve original hztexclname
  # this is missing in old SPC objects
  if (!is.null(x.list$hztexclcol)) {
    hztexclname(res) <- x.list$hztexclcol
  }
  
  # transfer metadata
  res <- .transfer.metadata.aqp(x, res)
  
  # replace site
  site(res) <- x.list$site
  
  # @diagnostic and may be missing if `x` is very old
  # replace with empty data.frame so that setters do not error
  if (is.null(x.list$diagnostic)) {
    x.list$diagnostic <- data.frame()
  }
  
  # @restructions may be missing if `x` is very old
  # replace with empty data.frame so that setters do not error
  if (is.null(x.list$restrictions)) {
    x.list$restrictions <- data.frame()
  }
  
  # set
  diagnostic_hz(res) <- x.list$diagnostic
  restrictions(res) <- x.list$restrictions

  # 2022-12-21: aqp 2.0; sp slot is no longer transferred by rebuildSPC
  #             but we transfer any existing data back to site 
  if (!is.null(x.list$sp)) {
    if (inherits(x.list$sp, 'SpatialPoints') &&
        ncol(x.list$sp@coords) == 2) {
      newsp <- cbind(data.frame(id = profile_id(x), stringsAsFactors = FALSE), 
                     as.data.frame(x.list$sp, stringsAsFactors = FALSE))
      
      crdnms <- c("x", "y")
      if (any(crdnms %in% names(x))) {
        crdnms <-  paste0("sp.", crdnms)
      } 
      colnames(newsp) <- c(idname(x), crdnms)
      
      site(res) <- newsp
      coordinates(res) <- colnames(newsp)[2:3]
      try(prj(res) <- x.list$sp@proj4string)
    }
  }
  return(res)
}

