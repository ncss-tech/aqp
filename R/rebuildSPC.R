# repair an SPC by breaking into pieces and re-assembling
# likely only used to fix outdated SPC objects that are missing slots
rebuildSPC <- function(x) {
  
  # break into pieces as list
  x.list <- suppressWarnings(as(x, 'list'))
  
  # seed object for new SPC
  res <- x.list$horizons
  
  # init SPC from pieces
  # note: using depths<- because it will generate a horizon ID
  fm <- as.formula(sprintf("%s ~ %s + %s", x.list$idcol, x.list$depthcols[1], x.list$depthcols[2]))
  depths(res) <- fm
  
  # preserve original hzidname
  # this is missing in old SPC objects
  if(!is.null(x.list$hzidcol)) {
    hzidname(res) <- x.list$hzidcol
  }
  
  # add additional pieces
  metadata(res) <- x.list$metadata
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

