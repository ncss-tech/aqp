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
  
  # add additional pieces
  metadata(res) <- x.list$metadata
  site(res) <- x.list$site
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

